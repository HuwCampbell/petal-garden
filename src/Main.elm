port module Main exposing (main)

import Array
import Browser
import Browser.Dom
import Engine exposing (Cell(..), Colour(..), Game, Outcome(..), Piece(..))
import Html exposing (Html, button, div, footer, h1, h2, header, main_, p, section, span, strong, text)
import Html.Attributes as A
import Html.Events as Events exposing (onClick)
import Html.Keyed
import Json.Decode as D
import Json.Encode as E
import Process
import Svg
import Svg.Attributes as S
import Task


port persist : E.Value -> Cmd msg


port playSound : { action : String, pop : Int, levelUp : Bool } -> Cmd msg


port interopError : (String -> msg) -> Sub msg


port openDialog : String -> Cmd msg


type alias Flags =
    { seed : Int, saved : Maybe String, legacy : Maybe String, best : Int, warning : String }


type alias Model =
    { game : Game
    , best : Int
    , legacy : Maybe String
    , message : String
    , warning : String
    , sound : Bool
    , animation : List Engine.Wave
    , pop : Int
    , levelUp : Bool
    , turn : Int
    }


type Msg
    = Play Int
    | Skip
    | AskNew
    | Restart
    | Help
    | AskImport
    | Import
    | Sound
    | InteropError String
    | NextPop Int
    | Focus Int
    | Focused (Result Browser.Dom.Error ())


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> interopError InteropError
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        -- Decoding failures start a fresh garden but remain visible as a warning.
        restored =
            case flags.saved of
                Just raw ->
                    D.decodeString (Engine.decode flags.seed) raw

                Nothing ->
                    Ok (Engine.new flags.seed)

        game =
            Result.withDefault (Engine.new flags.seed) restored

        warning =
            case restored of
                Ok _ ->
                    flags.warning

                Err error ->
                    "Your saved garden couldn't be restored: " ++ D.errorToString error
    in
    ( { game = game
      , best = max flags.best game.score
      , legacy = flags.legacy
      , message = prompt game
      , warning = warning
      , sound = False
      , animation = []
      , pop = 0
      , levelUp = False
      , turn = 0
      }
    , if Engine.isOver game then
        openDialog "over-dialog"

      else
        Cmd.none
    )


prompt : Game -> String
prompt game =
    if Engine.isOver game then
        "Your garden is full. Start a new garden when you're ready."

    else
        case Engine.current game of
            Just Spade ->
                "Spade: choose a flower, then an empty patch. Or skip this tool."

            Just (Butterfly colour) ->
                Engine.colourName colour ++ " butterfly: tap a flower to recolour its outer petals."

            _ ->
                "Plant a flower on any empty patch."


save : Model -> Cmd Msg
save model =
    persist (E.object [ ( "game", Engine.encode model.game ), ( "best", E.int model.best ) ])


nextPop : Int -> Cmd Msg
nextPop turn =
    Process.sleep 450 |> Task.perform (\_ -> NextPop turn)


apply : String -> Outcome -> Model -> ( Model, Cmd Msg )
apply action outcome model =
    case outcome of
        Rejected message ->
            ( { model | message = message }, Cmd.none )

        Selecting game message ->
            -- Spade selection changes the UI, not the persisted turn or sound.
            ( { model | game = game, message = message }, Cmd.none )

        Played game details ->
            let
                next =
                    { model
                        | game = game
                        , best = max model.best game.score
                        , turn = model.turn + 1
                        , animation = details.animation
                        , pop = 1
                        , levelUp = details.levelUp
                        , message =
                            (if details.levelUp then
                                "Level " ++ String.fromInt game.level ++ "! "

                             else
                                ""
                            )
                                ++ (if details.points > 0 then
                                        "+" ++ String.fromInt details.points ++ " points. "

                                    else
                                        ""
                                   )
                                ++ (if List.length details.waves > 1 then
                                        String.fromInt (List.length details.waves) ++ "-step cascade! "

                                    else
                                        ""
                                   )
                                ++ prompt game
                    }
            in
            ( next
            , Cmd.batch
                [ save next
                , if model.sound then
                    playSound
                        { action = action
                        , pop =
                            if List.isEmpty details.animation then
                                0

                            else
                                1
                        , levelUp = details.levelUp && List.isEmpty details.animation
                        }

                  else
                    Cmd.none
                , if Engine.isOver game && List.isEmpty details.animation then
                    openDialog "over-dialog"

                  else
                    Cmd.none
                , if List.isEmpty details.animation then
                    Cmd.none

                  else
                    nextPop next.turn
                ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play index ->
            let
                action =
                    case Engine.current model.game of
                        Just Spade ->
                            "spade"

                        Just (Butterfly _) ->
                            "butterfly"

                        _ ->
                            "plant"
            in
            if List.isEmpty model.animation then
                apply action (Engine.play index model.game) model

            else
                ( model, Cmd.none )

        Skip ->
            if List.isEmpty model.animation then
                apply "skip" (Engine.skip model.game) model

            else
                ( model, Cmd.none )

        AskNew ->
            ( model, openDialog "new-dialog" )

        Help ->
            ( model, openDialog "help-dialog" )

        AskImport ->
            ( model, openDialog "import-dialog" )

        Restart ->
            let
                next =
                    { model
                        | game = Engine.new model.game.seed
                        , message = "A fresh garden is ready."
                        , animation = []
                        , turn = model.turn + 1
                    }
            in
            ( next, save next )

        Import ->
            -- Legacy import is explicit and writes only to the Elm save key.
            case model.legacy of
                Nothing ->
                    ( { model | message = "No JavaScript garden is available at this address." }, Cmd.none )

                Just raw ->
                    case D.decodeString (Engine.decode model.game.seed) raw of
                        Err error ->
                            ( { model | warning = "Import failed: " ++ D.errorToString error }, Cmd.none )

                        Ok game ->
                            let
                                next =
                                    { model
                                        | game = game
                                        , best = max model.best game.score
                                        , message = "Copied your JavaScript garden. The original save is unchanged."
                                        , animation = []
                                        , turn = model.turn + 1
                                    }
                            in
                            ( next
                            , Cmd.batch
                                [ save next
                                , if Engine.isOver game then
                                    openDialog "over-dialog"

                                  else
                                    Cmd.none
                                ]
                            )

        Sound ->
            ( { model | sound = not model.sound }
            , if model.sound then
                playSound { action = "mute", pop = 0, levelUp = False }

              else
                playSound { action = "preview", pop = 0, levelUp = False }
            )

        InteropError error ->
            ( { model | warning = error, sound = False }, Cmd.none )

        NextPop turn ->
            if turn /= model.turn || List.isEmpty model.animation then
                ( model, Cmd.none )

            else
                let
                    remaining =
                        List.drop 1 model.animation

                    finished =
                        List.isEmpty remaining

                    next =
                        { model | animation = remaining, pop = model.pop + 1 }
                in
                ( next
                , Cmd.batch
                    [ if model.sound then
                        playSound
                            { action =
                                if finished then
                                    "skip"

                                else
                                    "pop"
                            , pop =
                                if finished then
                                    0

                                else
                                    next.pop
                            , levelUp = finished && model.levelUp
                            }

                      else
                        Cmd.none
                    , if finished then
                        if Engine.isOver model.game then
                            openDialog "over-dialog"

                        else
                            Cmd.none

                      else
                        nextPop turn
                    ]
                )

        Focus index ->
            ( model, Browser.Dom.focus ("patch-" ++ String.fromInt index) |> Task.attempt Focused )

        Focused result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err _ ->
                    ( { model | message = "That patch can't receive keyboard focus." }, Cmd.none )


palette : Colour -> { fill : String, dark : String, label : String, petals : Int }
palette colour =
    case colour of
        Rose ->
            { fill = "#ed7198", dark = "#922e55", label = "R", petals = 6 }

        Gold ->
            { fill = "#f0c750", dark = "#876112", label = "Y", petals = 8 }

        Blue ->
            { fill = "#78b9e4", dark = "#285d8d", label = "B", petals = 5 }

        Violet ->
            { fill = "#b39ae2", dark = "#60478f", label = "V", petals = 4 }

        Coral ->
            { fill = "#f59a6b", dark = "#9c462b", label = "O", petals = 7 }

        Mint ->
            { fill = "#87d9af", dark = "#286d51", label = "G", petals = 9 }


flowerShape : Colour -> Float -> Svg.Svg Msg
flowerShape colour scale =
    let
        colourStyle =
            palette colour
    in
    Svg.g
        [ S.transform ("translate(" ++ String.fromFloat (32 * (1 - scale)) ++ " " ++ String.fromFloat (32 * (1 - scale)) ++ ") scale(" ++ String.fromFloat scale ++ ")")
        , S.fill colourStyle.fill
        , S.stroke colourStyle.dark
        , S.strokeWidth "1.2"
        ]
        (List.range 0 (colourStyle.petals - 1)
            |> List.map
                (\i ->
                    Svg.ellipse
                        [ S.cx "32"
                        , S.cy "17"
                        , S.rx "9"
                        , S.ry "14"
                        , S.transform ("rotate(" ++ String.fromFloat (toFloat i * 360 / toFloat colourStyle.petals) ++ " 32 32)")
                        ]
                        []
                )
        )


svgText : String -> String -> Colour -> Svg.Svg Msg
svgText x y colour =
    let
        style =
            palette colour
    in
    Svg.text_ [ S.x x, S.y y, S.textAnchor "middle", S.fontSize "9", S.fontWeight "bold", S.fill style.dark ]
        [ Svg.text style.label ]


icon : Cell -> Maybe Piece -> Html Msg
icon cell tool =
    let
        artwork =
            case tool of
                Just Spade ->
                    [ Svg.g [ S.transform "rotate(35 32 32)" ]
                        [ Svg.path [ S.d "M27 7h10v12H27Z", S.fill "none", S.stroke "#d5af73", S.strokeWidth "4" ] []
                        , Svg.path [ S.d "M32 19v21", S.stroke "#d5af73", S.strokeWidth "5" ] []
                        , Svg.path [ S.d "M20 35h24v11q-4 10-12 14-8-4-12-14Z", S.fill "#d9e5de", S.stroke "#718d82", S.strokeWidth "2" ] []
                        ]
                    ]

                Just (Butterfly colour) ->
                    let
                        style =
                            palette colour
                    in
                    [ Svg.path [ S.d "M31 32C1 1 5 46 27 37 7 38 16 61 30 43ZM33 32C63 1 59 46 37 37 57 38 48 61 34 43Z", S.fill style.fill, S.stroke style.dark, S.strokeWidth "1.5" ] []
                    , Svg.path [ S.d "M32 25v24m0-23-5-8m5 8 5-8", S.stroke "#f4edce", S.strokeWidth "2", S.fill "none" ] []
                    , svgText "18" "31" colour
                    ]

                Just (Plant flower) ->
                    bloomArt flower

                Nothing ->
                    case cell of
                        Empty ->
                            []

                        Bloom flower ->
                            bloomArt flower

                        Stone ->
                            [ Svg.path [ S.d "M9 40 15 23 28 16 46 21 55 39 45 49 19 48Z", S.fill "#a5afa2", S.stroke "#677c72", S.strokeWidth "2" ] []
                            , Svg.path [ S.d "m16 24 16 9 14-11M32 33l-3 14", S.fill "none", S.stroke "#d1d8ca", S.strokeWidth "2" ] []
                            ]
    in
    if List.isEmpty artwork then
        text ""

    else
        Svg.svg [ S.viewBox "0 0 64 64", A.attribute "aria-hidden" "true", A.attribute "focusable" "false" ] artwork


bloomArt : Engine.Flower -> List (Svg.Svg Msg)
bloomArt flower =
    List.concat
        [ [ flowerShape flower.colour 1 ]
        , case flower.inner of
            Nothing ->
                []

            Just colour ->
                [ flowerShape colour 0.46 ]
        , [ Svg.circle
                [ S.cx "32"
                , S.cy "32"
                , S.r
                    (if flower.inner == Nothing then
                        "8"

                     else
                        "6"
                    )
                , S.fill "#fff4c8"
                , S.stroke (palette flower.colour).dark
                ]
                []
          , svgText "32" "35" (Maybe.withDefault flower.colour flower.inner)
          ]
        , if flower.inner == Nothing then
            []

          else
            [ Svg.circle [ S.cx "49", S.cy "49", S.r "8", S.fill (palette flower.colour).fill, S.stroke (palette flower.colour).dark ] []
            , svgText "49" "52" flower.colour
            ]
        ]


describeCell : Cell -> String
describeCell cell =
    case cell of
        Empty ->
            "Empty patch"

        Stone ->
            "Stone"

        Bloom flower ->
            Engine.colourName flower.colour ++ " flower" ++ (Maybe.map (\c -> ", " ++ Engine.colourName c ++ " centre") flower.inner |> Maybe.withDefault "")


describePiece : Piece -> String
describePiece piece =
    case piece of
        Plant flower ->
            describeCell (Bloom flower)

        Spade ->
            "Spade: move a flower"

        Butterfly colour ->
            Engine.colourName colour ++ " butterfly: recolour a flower"


eyebrow : String -> Html Msg
eyebrow label =
    p [ A.class "eyebrow" ] [ text label ]


textButton : String -> Msg -> String -> Html Msg
textButton id msg label =
    button [ A.id id, A.class "text-button", onClick msg ] [ text label ]


keyboard : Int -> Html.Attribute Msg
keyboard index =
    Events.preventDefaultOn "keydown"
        (D.field "key" D.string
            |> D.andThen
                (\key ->
                    let
                        target =
                            case key of
                                "ArrowLeft" ->
                                    if modBy 7 index > 0 then
                                        Just (index - 1)

                                    else
                                        Nothing

                                "ArrowRight" ->
                                    if modBy 7 index < 6 then
                                        Just (index + 1)

                                    else
                                        Nothing

                                "ArrowUp" ->
                                    if index >= 7 then
                                        Just (index - 7)

                                    else
                                        Nothing

                                "ArrowDown" ->
                                    if index < 42 then
                                        Just (index + 7)

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                    in
                    case target of
                        Just dest ->
                            D.succeed ( Focus dest, True )

                        Nothing ->
                            D.fail "Not a navigation key."
                )
        )


view : Model -> Html Msg
view model =
    let
        game =
            model.game

        wave =
            List.head model.animation

        animating =
            not (List.isEmpty model.animation)

        stat label id value =
            div [] [ span [ A.class "stat-label" ] [ text label ], strong [ A.id id ] [ text (String.fromInt value) ] ]

        patch index cell =
            button
                [ A.id ("patch-" ++ String.fromInt index)
                , A.type_ "button"
                , A.classList [ ( "patch", True ), ( "occupied", cell /= Empty ), ( "selected", game.selected == Just index ) ]
                , A.disabled (animating || Engine.isOver game || cell == Stone)
                , onClick (Play index)
                , keyboard index
                , A.attribute "aria-label" ("Row " ++ String.fromInt (index // 7 + 1) ++ ", column " ++ String.fromInt (modBy 7 index + 1) ++ ": " ++ describeCell cell)
                , A.attribute "aria-pressed"
                    (if game.selected == Just index then
                        "true"

                     else
                        "false"
                    )
                ]
                [ Html.Keyed.node "span"
                    [ A.classList [ ( "flower-art", True ), ( "pop", Maybe.map (\frame -> List.member index frame.indices) wave |> Maybe.withDefault False ) ] ]
                    [ ( String.fromInt model.turn ++ "-" ++ String.fromInt model.pop, icon cell Nothing ) ]
                ]

        preview piece =
            span [ A.attribute "role" "img", A.attribute "aria-label" (describePiece piece) ] [ icon Empty (Just piece) ]
    in
    main_ [ A.class "garden" ]
        [ header [ A.class "heading" ]
            [ div [] [ eyebrow "A LITTLE ROOM TO BLOOM", h1 [] [ text "Petal Garden", span [] [ text "." ] ] ]
            , button [ A.id "help", A.class "round-button", A.attribute "aria-label" "How to play", onClick Help ] [ text "?" ]
            ]
        , section [ A.class "stats", A.attribute "aria-label" "Garden statistics" ]
            [ stat "SCORE" "score" game.score, stat "LEVEL" "level" game.level, stat "BEST" "best" model.best ]
        , section [ A.class "tray", A.attribute "aria-label" "Upcoming pieces" ]
            [ div [ A.class "current-slot" ]
                [ eyebrow "PLANT THIS"
                , div [ A.id "current", A.attribute "role" "img", A.attribute "aria-label" (Engine.current game |> Maybe.map describePiece |> Maybe.withDefault "") ]
                    [ icon Empty (Engine.current game) ]
                ]
            , div [ A.class "upcoming-slot" ] [ eyebrow "COMING UP", div [ A.id "upcoming" ] (List.map preview (List.drop 1 game.queue)) ]
            ]
        , div [ A.class "board-frame" ]
            [ div
                [ A.id "board"
                , A.attribute "aria-label" "Garden, seven rows and seven columns"
                , A.attribute "aria-busy"
                    (if animating then
                        "true"

                     else
                        "false"
                    )
                ]
                (Array.toIndexedList (Maybe.map .board wave |> Maybe.withDefault game.board) |> List.map (\( i, cell ) -> patch i cell))
            ]
        , div [ A.class "progress-label" ]
            [ span [ A.id "progress-label" ] [ text ("Level " ++ String.fromInt game.level) ]
            , span [ A.id "remaining" ] [ text (String.fromInt game.remaining ++ " pieces left") ]
            ]
        , Html.progress [ A.id "progress", A.max (String.fromInt (Engine.batchSize game.level)), A.value (String.fromInt (Engine.batchSize game.level - game.remaining)), A.attribute "aria-label" "Level progress" ] []
        , p [ A.id "message", A.attribute "role" "status", A.attribute "aria-live" "polite" ] [ text model.message ]
        , p [ A.id "storage-warning", A.class "warning", A.attribute "role" "alert", A.hidden (String.isEmpty model.warning) ] [ text model.warning ]
        , footer []
            [ textButton "new-game" AskNew "New garden"
            , button
                [ A.id "skip"
                , A.class "text-button"
                , A.disabled animating
                , onClick Skip
                , A.hidden
                    (Engine.isOver game
                        || (case Engine.current game of
                                Just (Plant _) ->
                                    True

                                _ ->
                                    False
                           )
                    )
                ]
                [ text "Skip this tool" ]
            , button
                [ A.id "sound"
                , A.class "text-button"
                , onClick Sound
                , A.attribute "aria-pressed"
                    (if model.sound then
                        "true"

                     else
                        "false"
                    )
                ]
                [ text
                    (if model.sound then
                        "Sound: on"

                     else
                        "Sound: off"
                    )
                ]
            ]
        , if model.legacy /= Nothing then
            textButton "import-game" AskImport "Copy JavaScript garden"

          else
            text ""
        , p [ A.class "credit" ] [ text "A little game in Elm." ]
        , dialog "help-dialog"
            "Make a little space."
            [ p [] [ text "Tap an empty patch to plant. Match three or more outer petal colours horizontally or vertically. No diagonals, gravity or timer." ]
            , p [] [ text "Doubles reveal their inner colour when matched, possibly starting a cascade. A butterfly recolours outer petals. A spade moves a flower: tap it, then an empty patch. Tap it again to cancel selection. Tools may be skipped." ]
            , p [] [ text "Complete each batch to advance through the levels, as additional colours, stones, and the rate of bi-colour flowers inceases." ]
            , p [] [ text "Saves stay in this browser." ]
            , closeButton "Let's plant"
            ]
        , dialog "new-dialog"
            "A fresh start?"
            [ p [] [ text "This replaces only your garden. Your best score stays." ], closeButton "Keep playing", actionButton Restart "Start fresh" ]
        , dialog "import-dialog"
            "Copy your garden?"
            [ p [] [ text "This replaces your garden with the JavaScript save found when this page opened. Its original save is untouched." ], closeButton "Cancel", actionButton Import "Copy garden" ]
        , dialog "over-dialog"
            "Every patch is full."
            [ p [ A.id "final-score" ] [ text ("You reached level " ++ String.fromInt game.level ++ " with " ++ String.fromInt game.score ++ " points.") ]
            , closeButton "See my garden"
            , actionButton Restart "Plant again"
            ]
        ]


dialog : String -> String -> List (Html Msg) -> Html Msg
dialog id title children =
    Html.node "dialog" [ A.id id ] [ Html.form [ A.method "dialog" ] (h2 [] [ text title ] :: children) ]


closeButton : String -> Html Msg
closeButton label =
    button [ A.class "text-button", A.type_ "submit" ] [ text label ]


actionButton : Msg -> String -> Html Msg
actionButton msg label =
    button [ A.class "primary", A.type_ "submit", onClick msg ] [ text label ]
