port module TestRunner exposing (main)

import Engine exposing (Outcome(..), Piece(..))
import Json.Decode as D
import Json.Encode as E
import Platform


port request : (D.Value -> msg) -> Sub msg


port response : E.Value -> Cmd msg


port sampleLevel : (Int -> msg) -> Sub msg


port sampleResult : E.Value -> Cmd msg


type Msg
    = Run D.Value
    | Sample Int


main : Program () () Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.batch [ request Run, sampleLevel Sample ]
        , update =
            \msg _ ->
                ( ()
                , case msg of
                    Run value ->
                        response (run value)

                    Sample level ->
                        sampleResult (sample level)
                )
        }


sample : Int -> E.Value
sample level =
    let
        -- Skip a tool to generate one fresh piece without needing board moves.
        step _ counts =
            let
                game =
                    counts.game
            in
            case Engine.skip { game | level = level, remaining = 2, queue = [ Spade ] } of
                Played next _ ->
                    case Engine.current next of
                        Just (Plant flower) ->
                            { counts
                                | game = next
                                , flowers = counts.flowers + 1
                                , doubles =
                                    counts.doubles
                                        + (if flower.inner == Nothing then
                                            0

                                           else
                                            1
                                          )
                            }

                        _ ->
                            { counts | game = next, tools = counts.tools + 1 }

                _ ->
                    counts

        result =
            List.foldl step { game = Engine.new 2468, flowers = 0, doubles = 0, tools = 0 } (List.range 1 6000)
    in
    E.object [ ( "flowers", E.int result.flowers ), ( "doubles", E.int result.doubles ), ( "tools", E.int result.tools ) ]


run : D.Value -> E.Value
run value =
    case D.decodeValue (D.map2 Tuple.pair (D.field "game" (Engine.decode 12345)) (D.field "actions" (D.list (D.nullable D.int)))) value of
        Err error ->
            E.object [ ( "error", E.string (D.errorToString error) ) ]

        Ok ( initial, actions ) ->
            let
                step action ( game, events ) =
                    -- Keep rejected and selecting actions in the report without consuming a turn.
                    case Maybe.map (\index -> Engine.play index game) action |> Maybe.withDefault (Engine.skip game) of
                        Rejected message ->
                            ( game, events ++ [ E.object [ ( "ok", E.bool False ), ( "message", E.string message ) ] ] )

                        Selecting selected message ->
                            ( selected, events ++ [ E.object [ ( "ok", E.bool True ), ( "selecting", E.bool True ), ( "message", E.string message ) ] ] )

                        Played played details ->
                            ( played
                            , events
                                ++ [ E.object
                                        [ ( "ok", E.bool True )
                                        , ( "waves", E.list (E.list E.int) details.waves )
                                        , ( "points", E.int details.points )
                                        , ( "levelUp", E.bool details.levelUp )
                                        ]
                                   ]
                            )

                ( final, results ) =
                    List.foldl step ( initial, [] ) actions
            in
            E.object
                [ ( "game", Engine.encode final )
                , ( "selected", Maybe.map E.int final.selected |> Maybe.withDefault E.null )
                , ( "events", E.list identity results )
                ]
