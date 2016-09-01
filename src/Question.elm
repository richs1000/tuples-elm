module Question exposing (..)

import Tuples exposing (..)
import RandomStuff exposing (pickOne, pickABunch, compressList)


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

                distractors =
                    [ ( 1
                      , tupleToString (Maybe.withDefault [ RandomInt 1 ] (List.head tupOfTups))
                      )
                    , ( 2
                      , tupleToString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 1 tupOfTups)))
                      )
                    , ( 3
                      , tupleToString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 2 tupOfTups)))
                      )
                    , ( 4
                      , tupleToTypeString (Maybe.withDefault [ RandomInt 1 ] (List.head tupOfTups))
                      )
                    , ( 5
                      , tupleToTypeString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 1 tupOfTups)))
                      )
                    , ( 6
                      , tupleToTypeString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 2 tupOfTups)))
                      )
                    , ( 7
                      , tupleToListString (Maybe.withDefault [ RandomInt 1 ] (List.head tupOfTups))
                      )
                    , ( 8
                      , tupleToListString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 1 tupOfTups)))
                      )
                    , ( 9
                      , tupleToListString (Maybe.withDefault [ RandomInt 1 ] (List.head (List.drop 2 tupOfTups)))
                      )
                    ]

                ( answers, distractors' ) =
                    List.partition (\( index, _ ) -> index == randIndex) distractors

                answer' =
                    Maybe.withDefault ( 0, "uh oh" ) (List.head answers)
            in
                { question = question'
                , distractors = List.map (\( _, dis ) -> ( dis, "Incorrect." )) (compressList distractors')
                , answer = ( snd answer', "Correct" )
                , format = MultipleChoice
                }
            -- one level deep - what is the type of?
        else if index == 2 then
            let
                rTup =
                    randomTuple randomValues [ 2, 3, 4 ] 2

                dTup1 =
                    randomTuple (List.drop 10 randomValues) [ 2, 3, 4 ] 2

                dTup2 =
                    randomTuple (List.drop 20 randomValues) [ 2, 3, 4 ] 2

                question' =
                    [ "What is the type of e?"
                    , "val e = " ++ (tupleToString rTup)
                    ]

                answer' =
                    ( tupleToTypeString rTup
                    , "Correct"
                    )

                distractors' =
                    compressList
                        [ answer'
                        , ( tupleToWrongTypeString rTup
                          , "Incorrect. You have listed them in the wrong order"
                          )
                        , ( tupleItemToString (Maybe.withDefault (RandomInt 1) (List.head rTup))
                          , "Incorrect. That is the type of the first item in e"
                          )
                        , ( tupleToString rTup
                          , "Incorrect. That is the value of e"
                          )
                        , ( tupleToTypeString dTup1
                          , "Incorrect"
                          )
                        , ( tupleToTypeString dTup2
                          , "Incorrect"
                          )
                        ]
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
                    pickOne (List.drop 15 randomValues) [ 1, 2, 3 ] 1

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
                    compressList
                        (tupleOfTuplesToFlatListOfStrings tupOfTups)

                ( _, distractors'' ) =
                    List.partition (\d -> d == answer') distractors'
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
