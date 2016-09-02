module Question exposing (..)

import Tuples exposing (..)
import RandomStuff exposing (pickOne, pickABunch, compressList)
import Debug exposing (..)


type QuestionFormat
    = FillInTheBlank
    | MultipleChoice


type alias Question =
    { question : List String
    , distractors : List ResponseAndFeedback
    , answer : ResponseAndFeedback
    , format : QuestionFormat
    }


type alias ResponseAndFeedback =
    ( String, String )


emptyQuestion : Question
emptyQuestion =
    { question = []
    , distractors = []
    , answer = ( "", "" )
    , format = FillInTheBlank
    }


newQuestion : List Int -> Int -> Question
newQuestion randomValues index =
    let
        index' =
            index
    in
        -- one level deep - extract
        if index == 1 then
            let
                tupOfTups =
                    randomTupleOfTuples randomValues [ 1, 2, 3 ] 1

                randIndex =
                    pickOne (List.drop 21 randomValues) [ 1, 2, 3 ] 1

                strExpr =
                    "#" ++ toString randIndex ++ " e"

                question' =
                    [ "What is the value of ans after the following ML expressions are evaluated?"
                    , "val e = " ++ (tupleOfTuplesToString tupOfTups)
                    , "val ans = " ++ strExpr
                    ]

                answer' =
                    if randIndex == 1 then
                        tupleToString (Maybe.withDefault [ RandomInt 1 ] (List.head tupOfTups))
                    else if randIndex == 2 then
                        tupleToString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 1 tupOfTups)))
                    else
                        tupleToString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 2 tupOfTups)))

                distractors =
                    [ tupleToString (Maybe.withDefault [ RandomInt 1 ] (List.head tupOfTups))
                    , tupleToTypeString (Maybe.withDefault [ RandomInt 1 ] (List.head tupOfTups))
                    , tupleToListString (Maybe.withDefault [ RandomInt 1 ] (List.head tupOfTups))
                    , tupleToString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 1 tupOfTups)))
                    , tupleToTypeString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 1 tupOfTups)))
                    , tupleToListString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 1 tupOfTups)))
                    , tupleToString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 2 tupOfTups)))
                    , tupleToTypeString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 2 tupOfTups)))
                    , tupleToListString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 2 tupOfTups)))
                    ]

                ( _, distractors' ) =
                    List.partition (\d -> d == answer') (compressList distractors)

                d =
                    Debug.log "partitioned " distractors'
            in
                { question = question'
                , distractors = List.map (\dis -> ( dis, "Incorrect." )) distractors'
                , answer = ( answer', "Correct" )
                , format = MultipleChoice
                }
            -- one level deep - what is the type of?
        else if index == 2 then
            let
                rTup =
                    randomTuple randomValues [ 2, 3, 4 ] 2

                question' =
                    [ "What is the type of e?"
                    , "val e = " ++ (tupleToString rTup)
                    ]

                answer' =
                    ( tupleToTypeString rTup
                    , "Correct"
                    )

                distractors' =
                    Debug.log "compressed "
                        (compressList
                            [ ( tupleToWrongTypeString rTup
                              , "Incorrect. You have listed them in the wrong order"
                              )
                            , ( tupleItemToString (Maybe.withDefault (RandomInt 1) (List.head rTup))
                              , "Incorrect. That is the type of the first item in e"
                              )
                            , ( tupleToString rTup
                              , "Incorrect. That is the value of e"
                              )
                            ]
                        )
            in
                { question = question'
                , distractors = (List.drop 1 distractors')
                , answer = answer'
                , format = MultipleChoice
                }
            -- two levels deep - extract
        else
            let
                tupOfTups =
                    randomTupleOfTuples randomValues [ 2, 3 ] 2

                exteriorIndex =
                    pickOne (List.drop 14 randomValues) [ 1, 2, 3 ] 1

                interiorIndex =
                    pickOne (List.drop 15 randomValues) [ 1, 2 ] 1

                strExpr =
                    "#" ++ toString exteriorIndex ++ " (#" ++ toString interiorIndex ++ " e)"

                question' =
                    [ "What is the value of ans after the following ML expressions are evaluated?"
                    , "val e = " ++ (tupleOfTuplesToString tupOfTups)
                    , "val ans = " ++ strExpr
                    ]

                answer' =
                    tupOfTups
                        |> extractTupleFromTuple interiorIndex
                        |> extractItemFromTuple exteriorIndex
                        |> tupleItemToString

                distractors' =
                    Debug.log "compressed "
                        (compressList
                            (tupleOfTuplesToFlatListOfStrings tupOfTups)
                        )

                ( _, distractors'' ) =
                    List.partition (\d -> d == answer') distractors'

                distractors''' =
                    Debug.log "partitioned " distractors''
            in
                { question = question'
                , distractors = List.map (\dis -> ( dis, "Incorrect." )) distractors''
                , answer = ( answer', "Correct" )
                , format = MultipleChoice
                }


findFeedback : String -> String -> List ResponseAndFeedback -> String
findFeedback answer response distractors =
    case distractors of
        [] ->
            "Incorrect. The answer is " ++ answer

        d :: ds ->
            if ((fst d) == response || ((fst d) == "")) then
                (snd d) ++ " The answer is " ++ answer
            else
                findFeedback answer response ds
