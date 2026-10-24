module Engine exposing
    ( Cell(..)
    , Colour(..)
    , Flower
    , Game
    , Outcome(..)
    , Piece(..)
    , Wave
    , batchSize
    , colourName
    , current
    , decode
    , encode
    , isOver
    , matches
    , new
    , play
    , resolve
    , skip
    , stoneTarget
    )

import Array exposing (Array)
import Json.Decode as D
import Json.Encode as E
import Random
import Set


type Colour
    = Rose
    | Gold
    | Blue
    | Violet
    | Coral
    | Mint


type alias Flower =
    { colour : Colour, inner : Maybe Colour }


type Cell
    = Empty
    | Stone
    | Bloom Flower


type Piece
    = Plant Flower
    | Butterfly Colour
    | Spade


type alias Game =
    { board : Array Cell
    , queue : List Piece
    , score : Int
    , level : Int
    , remaining : Int
    , selected : Maybe Int
    , seed : Int
    }


type Outcome
    = Rejected String
    | Selecting Game String
    | Played Game { waves : List (List Int), animation : List Wave, points : Int, levelUp : Bool }


type alias Wave =
    { board : Array Cell, indices : List Int }


batchSize : Int -> Int
batchSize level =
    24 + min ((level - 1) * 4) 32


stoneTarget : Int -> Int
stoneTarget level =
    max 12
        (if level <= 4 then
            12 + (level - 1) * 2

         else
            18 - (level - 4)
        )


stoneOrder : List Int
stoneOrder =
    -- Grow corner wedges in a fixed order so the centre stays connected.
    [ 0, 6, 48, 42, 1, 13, 47, 35, 7, 5, 41, 43, 2, 20, 46, 28, 14, 4 ]


adjustStones : Game -> Game
adjustStones game =
    let
        positions =
            Set.fromList (List.take (stoneTarget game.level) stoneOrder)

        adjust index cell =
            case cell of
                Stone ->
                    if Set.member index positions then
                        Stone

                    else
                        Empty

                Empty ->
                    if Set.member index positions then
                        Stone

                    else
                        Empty

                Bloom _ ->
                    -- A growing border must never overwrite an existing flower.
                    cell
    in
    { game | board = Array.indexedMap adjust game.board }


new : Int -> Game
new seed =
    { board = Array.repeat 49 Empty
    , queue = []
    , score = 0
    , level = 1
    , remaining = batchSize 1
    , selected = Nothing
    , seed = seed
    }
        |> adjustStones
        |> fillQueue


colours : List Colour
colours =
    [ Rose, Gold, Blue, Violet, Coral, Mint ]


colourName : Colour -> String
colourName colour =
    case colour of
        Rose ->
            "rose"

        Gold ->
            "gold"

        Blue ->
            "blue"

        Violet ->
            "violet"

        Coral ->
            "coral"

        Mint ->
            "mint"


pick : List Colour -> Random.Generator Colour
pick choices =
    case choices of
        first :: rest ->
            Random.uniform first rest

        [] ->
            Random.constant Rose


pieceGenerator : Int -> Random.Generator Piece
pieceGenerator level =
    let
        available =
            List.take (min 6 (3 + level // 2)) colours

        flower colour =
            Random.float 0 1
                |> Random.andThen
                    (\roll ->
                        if level >= 2 && roll < doubleChance level then
                            pick (List.filter ((/=) colour) available)
                                |> Random.map (\inner -> Plant { colour = colour, inner = Just inner })

                        else
                            Random.constant (Plant { colour = colour, inner = Nothing })
                    )
    in
    Random.float 0 1
        |> Random.andThen
            (\roll ->
                if roll < 0.07 then
                    Random.map Butterfly (pick available)

                else if roll < 0.12 then
                    Random.constant Spade

                else
                    pick available |> Random.andThen flower
            )


doubleChance : Int -> Float
doubleChance level =
    -- Preserve the opening difficulty; only post-level-14 flowers get harder.
    if level <= 14 then
        min 0.55 (toFloat level * 0.075)

    else
        min 0.9 (0.55 + toFloat (level - 14) * 0.025)


fillQueue : Game -> Game
fillQueue game =
    if List.length game.queue < min 5 game.remaining then
        let
            ( ( piece, seed ), _ ) =
                -- Persist a fresh integer seed so a save resumes the same sequence.
                Random.step
                    (Random.pair (pieceGenerator game.level) (Random.int 0 2147483647))
                    (Random.initialSeed game.seed)
        in
        fillQueue { game | queue = game.queue ++ [ piece ], seed = seed }

    else
        game


current : Game -> Maybe Piece
current game =
    List.head game.queue


isOver : Game -> Bool
isOver game =
    case current game of
        Just (Plant _) ->
            not (Array.toList game.board |> List.member Empty)

        _ ->
            False


matches : Array Cell -> List Int
matches board =
    let
        run start step colour index =
            -- Horizontal runs must stop at row boundaries, not wrap into the next row.
            if index >= 49 || (step == 1 && index // 7 /= start // 7) then
                []

            else
                case Array.get index board of
                    Just (Bloom flower) ->
                        if flower.colour == colour then
                            index :: run start step colour (index + step)

                        else
                            []

                    _ ->
                        []

        at step start =
            case Array.get start board of
                Just (Bloom flower) ->
                    let
                        indices =
                            run start step flower.colour start
                    in
                    if List.length indices >= 3 then
                        indices

                    else
                        []

                _ ->
                    []
    in
    [ 1, 7 ]
        |> List.concatMap (\step -> List.concatMap (at step) (List.range 0 48))
        |> Set.fromList
        -- A crossing belongs to one wave and scores each flower only once.
        |> Set.toList


resolve : Game -> ( Game, List (List Int) )
resolve game =
    resolveHelp 1 [] game
        |> Tuple.mapSecond (List.map .indices)


resolveHelp : Int -> List Wave -> Game -> ( Game, List Wave )
resolveHelp multiplier waves game =
    let
        indices =
            -- Find the whole wave before revealing any inner petals.
            matches game.board

        reveal index board =
            case Array.get index board of
                Just (Bloom flower) ->
                    Array.set index
                        (case flower.inner of
                            Just colour ->
                                Bloom { colour = colour, inner = Nothing }

                            Nothing ->
                                Empty
                        )
                        board

                _ ->
                    board
    in
    if List.isEmpty indices then
        ( game, List.reverse waves )

    else
        resolveHelp (multiplier + 1)
            ({ board = game.board, indices = indices } :: waves)
            { game
                | board = List.foldl reveal game.board indices
                , score = game.score + List.length indices * 10 * multiplier
            }


finish : Game -> Outcome
finish game =
    let
        ( resolved, animation ) =
            resolveHelp 1 [] game

        levelUp =
            game.remaining == 1

        advanced =
            -- Resolve the final piece before changing stones or refilling the queue.
            if levelUp then
                { resolved
                    | level = game.level + 1
                    , remaining = batchSize (game.level + 1)
                    , score = resolved.score + game.level * 100
                }
                    |> adjustStones

            else
                { resolved | remaining = game.remaining - 1 }

        next =
            fillQueue { advanced | queue = List.drop 1 game.queue, selected = Nothing }
    in
    Played next { waves = List.map .indices animation, animation = animation, points = next.score - game.score, levelUp = levelUp }


play : Int -> Game -> Outcome
play index game =
    if index < 0 || index >= 49 then
        Rejected "Choose a patch inside the garden."

    else if isOver game then
        Rejected "This garden is full. Start a new garden to play again."

    else
        case ( current game, Array.get index game.board ) of
            ( _, Just Stone ) ->
                Rejected "That's a stone. Choose another patch."

            ( Just (Plant flower), Just Empty ) ->
                finish { game | board = Array.set index (Bloom flower) game.board }

            ( Just (Plant _), _ ) ->
                Rejected "Choose an empty patch for this flower."

            ( Just (Butterfly colour), Just (Bloom flower) ) ->
                finish { game | board = Array.set index (Bloom { flower | colour = colour }) game.board }

            ( Just (Butterfly _), _ ) ->
                Rejected "Choose a flower for the butterfly to recolour."

            ( Just Spade, Just (Bloom _) ) ->
                -- Selecting a source is not a turn; only moving it consumes the spade.
                let
                    selected =
                        if game.selected == Just index then
                            Nothing

                        else
                            Just index
                in
                Selecting { game | selected = selected }
                    (if selected == Nothing then
                        "Selection cancelled. Choose a flower to move."

                     else
                        "Now choose an empty patch for this flower."
                    )

            ( Just Spade, Just Empty ) ->
                case game.selected |> Maybe.andThen (\source -> Array.get source game.board |> Maybe.map (Tuple.pair source)) of
                    Just ( source, Bloom flower ) ->
                        finish
                            { game
                                | board =
                                    game.board
                                        |> Array.set source Empty
                                        |> Array.set index (Bloom flower)
                            }

                    _ ->
                        Rejected "First choose a flower to lift with the spade."

            _ ->
                Rejected "The game has no playable piece."


skip : Game -> Outcome
skip game =
    case current game of
        Just Spade ->
            finish game

        Just (Butterfly _) ->
            finish game

        _ ->
            Rejected "Flowers must be planted. Only tools can be skipped."


encodeFlower : Flower -> E.Value
encodeFlower flower =
    E.object
        [ ( "type", E.string "flower" )
        , ( "colour", E.string (colourName flower.colour) )
        , ( "inner", Maybe.map (colourName >> E.string) flower.inner |> Maybe.withDefault E.null )
        ]


encodeCell : Cell -> E.Value
encodeCell cell =
    case cell of
        Empty ->
            E.null

        Stone ->
            E.object [ ( "type", E.string "rock" ) ]

        Bloom flower ->
            encodeFlower flower


encodePiece : Piece -> E.Value
encodePiece piece =
    case piece of
        Plant flower ->
            encodeFlower flower

        Spade ->
            E.object [ ( "type", E.string "spade" ) ]

        Butterfly colour ->
            E.object [ ( "type", E.string "butterfly" ), ( "colour", E.string (colourName colour) ) ]


encode : Game -> E.Value
encode game =
    E.object
        [ ( "version", E.int 1 )
        , ( "board", E.array encodeCell game.board )
        , ( "queue", E.list encodePiece game.queue )
        , ( "score", E.int game.score )
        , ( "level", E.int game.level )
        , ( "remaining", E.int game.remaining )
        , ( "over", E.bool (isOver game) )
        , ( "seed", E.int game.seed )
        ]


colourDecoder : D.Decoder Colour
colourDecoder =
    D.string
        |> D.andThen
            (\name ->
                case List.filter (\colour -> colourName colour == name) colours |> List.head of
                    Just colour ->
                        D.succeed colour

                    Nothing ->
                        D.fail ("Unknown flower colour: " ++ name)
            )


flowerDecoder : D.Decoder Flower
flowerDecoder =
    D.map2 Flower (D.field "colour" colourDecoder) (D.field "inner" (D.nullable colourDecoder))


cellDecoder : D.Decoder Cell
cellDecoder =
    D.oneOf
        [ D.null Empty
        , D.field "type" D.string
            |> D.andThen
                (\kind ->
                    case kind of
                        "rock" ->
                            D.succeed Stone

                        "flower" ->
                            D.map Bloom flowerDecoder

                        _ ->
                            D.fail "Invalid board cell."
                )
        ]


pieceDecoder : D.Decoder Piece
pieceDecoder =
    D.field "type" D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "flower" ->
                        D.map Plant flowerDecoder

                    "spade" ->
                        D.succeed Spade

                    "butterfly" ->
                        D.map Butterfly (D.field "colour" colourDecoder)

                    _ ->
                        D.fail "Invalid queued piece."
            )


decode : Int -> D.Decoder Game
decode fallbackSeed =
    D.field "version" D.int
        |> D.andThen
            (\version ->
                if version /= 1 then
                    D.fail "Unsupported saved garden version."

                else
                    D.map8
                        (\board queue score level remaining over seed selected ->
                            ( { board = board
                              , queue = queue
                              , score = score
                              , level = level
                              , remaining = remaining
                              , seed = seed
                              , selected = selected
                              }
                            , over
                            )
                        )
                        (D.field "board" (D.array cellDecoder))
                        (D.field "queue" (D.list pieceDecoder))
                        (D.field "score" D.int)
                        (D.field "level" D.int)
                        (D.field "remaining" D.int)
                        (D.field "over" D.bool)
                        (D.keyValuePairs D.value
                            -- Old saves lack a seed; a present but invalid seed must still fail.
                            |> D.andThen
                                (\fields ->
                                    if List.any (\( key, _ ) -> key == "seed") fields then
                                        D.field "seed" D.int

                                    else
                                        D.succeed fallbackSeed
                                )
                        )
                        (D.succeed Nothing)
                        |> D.andThen
                            (\( game, over ) ->
                                -- Reject inconsistent saves instead of repairing them silently.
                                if
                                    Array.length game.board
                                        == 49
                                        && game.score
                                        >= 0
                                        && game.level
                                        >= 1
                                        && game.remaining
                                        >= 1
                                        && game.remaining
                                        <= batchSize game.level
                                        && List.length game.queue
                                        == min 5 game.remaining
                                        && over
                                        == isOver game
                                        && List.isEmpty (matches game.board)
                                then
                                    D.succeed game

                                else
                                    D.fail "Saved garden violates the game rules."
                            )
            )
