module Tuples exposing (..)

import RandomStuff exposing (..)


type TupleItem
    = RandomInt Int
    | RandomString String
    | RandomBool Bool


type alias RandomTuple =
    List TupleItem


type alias TupleOfTuples =
    List RandomTuple


tupleItemToTypeString : TupleItem -> String
tupleItemToTypeString t =
    case t of
        RandomInt _ ->
            "int"

        RandomString _ ->
            "string"

        RandomBool _ ->
            "bool"


tupleItemToWrongTypeString : TupleItem -> String
tupleItemToWrongTypeString t =
    case t of
        RandomInt _ ->
            "bool"

        RandomString _ ->
            "int"

        RandomBool _ ->
            "string"


tupleItemToString : TupleItem -> String
tupleItemToString t =
    case t of
        RandomInt i ->
            toString i

        RandomString s ->
            toString s

        RandomBool b ->
            toString b


tupleTo : (TupleItem -> String) -> String -> String -> RandomTuple -> String
tupleTo tupItemTo leftDelimeter rightDelimeter tup =
    let
        helperFunc tup' =
            case tup' of
                [] ->
                    ""

                t :: [] ->
                    (tupItemTo t)

                t :: ts ->
                    (tupItemTo t) ++ ", " ++ (helperFunc ts)
    in
        if List.length tup == 1 then
            helperFunc tup
        else
            leftDelimeter ++ (helperFunc tup) ++ rightDelimeter


tupleToWrongTypeString : RandomTuple -> String
tupleToWrongTypeString tup =
    tupleTo tupleItemToWrongTypeString "(" ")" tup


tupleToTypeString : RandomTuple -> String
tupleToTypeString tup =
    tupleTo tupleItemToTypeString "(" ")" tup


tupleToString : RandomTuple -> String
tupleToString tup =
    tupleTo tupleItemToString "(" ")" tup


tupleToListString : RandomTuple -> String
tupleToListString tup =
    tupleTo tupleItemToString "[" "]" tup


tupleOfTuplesTo : (TupleItem -> String) -> String -> String -> TupleOfTuples -> String
tupleOfTuplesTo tupItemTo leftDelimeter rightDelimeter tups =
    let
        helperFunc tups' =
            case tups' of
                [] ->
                    ""

                t :: [] ->
                    (tupleTo tupItemTo leftDelimeter rightDelimeter t)

                t :: ts ->
                    (tupleTo tupItemTo leftDelimeter rightDelimeter t) ++ ", " ++ (helperFunc ts)
    in
        leftDelimeter ++ helperFunc tups ++ rightDelimeter


tupleOfTuplesToString : TupleOfTuples -> String
tupleOfTuplesToString tups =
    tupleOfTuplesTo tupleItemToString "(" ")" tups


randomTupleItem : List Int -> TupleItem
randomTupleItem randomValues =
    let
        randStrings =
            [ "dog", "cat", "pig", "moose", "cow", "bird" ]

        rType =
            pickOne randomValues [ 1, 2, 3 ] 1
    in
        case rType of
            1 ->
                let
                    rVal =
                        pickOne (List.drop 1 randomValues) [ 1, 2 ] 1
                in
                    if rVal == 1 then
                        RandomBool True
                    else
                        RandomBool False

            2 ->
                let
                    rVal =
                        pickOne (List.drop 1 randomValues) [0..9] 1
                in
                    RandomInt rVal

            _ ->
                let
                    rVal =
                        pickOne (List.drop 1 randomValues) randStrings "cat"
                in
                    RandomString rVal


randomTuple : List Int -> List Int -> Int -> RandomTuple
randomTuple randomValues lengths defVal =
    let
        tupleLen =
            pickOne randomValues lengths defVal

        helperFunc cnt =
            if cnt >= tupleLen then
                []
            else
                (randomTupleItem (List.drop (2 * cnt + 1) randomValues)) :: (helperFunc (cnt + 1))
    in
        helperFunc 0


randomTupleOfTuples : List Int -> List Int -> Int -> TupleOfTuples
randomTupleOfTuples randomValues lengths defVal =
    [ randomTuple randomValues lengths defVal
    , randomTuple (List.drop 7 randomValues) lengths defVal
    , randomTuple (List.drop 14 randomValues) lengths defVal
    ]


extractItemFromTuple : Int -> RandomTuple -> TupleItem
extractItemFromTuple index tup =
    Maybe.withDefault (RandomString "foo") (List.head (List.drop (index - 1) tup))


extractTupleFromTuple : Int -> TupleOfTuples -> RandomTuple
extractTupleFromTuple index tups =
    Maybe.withDefault [ (RandomString "foo") ] (List.head (List.drop (index - 1) tups))


tupleToListOfStrings : RandomTuple -> List String
tupleToListOfStrings tup =
    case tup of
        [] ->
            []

        t :: ts ->
            (tupleItemToString t) :: (tupleToListOfStrings ts)


tupleOfTuplesToFlatListOfStrings : TupleOfTuples -> List String
tupleOfTuplesToFlatListOfStrings tups =
    case tups of
        [] ->
            []

        t :: ts ->
            List.append (tupleToListOfStrings t) (tupleOfTuplesToFlatListOfStrings ts)
