module Internal exposing (intersectionBy, tokenize, tokenizeWord)

import List.Extra as List


tokenize : String -> List String
tokenize value =
    let
        trim : String -> Maybe String
        trim =
            String.trim
                >> (\s ->
                        if s == "" then
                            Nothing

                        else
                            Just s
                   )
    in
    value
        |> String.split " "
        |> List.filterMap trim
        |> List.concatMap (tokenizeWord [])
        |> List.unique


tokenizeWord : List String -> String -> List String
tokenizeWord accumulator s =
    if s == "" then
        accumulator

    else
        tokenizeWord (s :: accumulator) (String.dropLeft 1 s)


intersectionBy : (doc -> comparable) -> List doc -> List doc -> List doc
intersectionBy getRef l1 l2 =
    let
        l2refs : List comparable
        l2refs =
            List.map getRef l2
    in
    List.filter (\doc -> List.member (getRef doc) l2refs) l1
