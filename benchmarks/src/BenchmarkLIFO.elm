module BenchmarkLIFO exposing (main)

import Benchmark exposing (benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Internal as I


alphabet : List String
alphabet =
    List.range 97 122
        |> List.map (Char.fromCode >> String.fromChar)


main : BenchmarkProgram
main =
    let
        prefilledList =
            List.range 1 26
                |> List.concatMap (\_ -> alphabet)

        prefilledLIFO =
            List.range 1 26
                |> List.map (\_ -> alphabet)
    in
    program <|
        describe "LIFO"
            [ describe "List"
                [ benchmark "first push" <|
                    \_ ->
                        List.append alphabet []
                , benchmark "push" <|
                    \_ ->
                        List.append alphabet prefilledList
                , benchmark "pop" <|
                    \_ ->
                        case prefilledList of
                            [] ->
                                ( Nothing, [] )

                            head :: tail ->
                                ( Just head, tail )
                , benchmark "fill&empty" <|
                    \_ ->
                        let
                            l =
                                List.range 1 26
                                    |> List.foldl (\_ -> (::) alphabet) []
                        in
                        List.range 1 676
                            |> List.foldl
                                (\_ lifo ->
                                    case lifo of
                                        [] ->
                                            []

                                        _ :: tail ->
                                            tail
                                )
                                l
                ]
            , describe "List of List"
                [ benchmark "first push" <|
                    \_ ->
                        lifoPush alphabet []
                , benchmark "push" <|
                    \_ ->
                        lifoPush alphabet prefilledLIFO
                , benchmark "pop" <|
                    \_ ->
                        lifoPop prefilledLIFO
                , benchmark "fill&empty" <|
                    \_ ->
                        let
                            l =
                                List.range 1 26
                                    |> List.foldl (\_ -> lifoPush alphabet) []
                        in
                        List.range 1 676
                            |> List.foldl
                                (\_ lifo ->
                                    lifoPop lifo
                                        |> Tuple.second
                                )
                                l
                ]
            ]


type alias LIFO a =
    List (List a)


lifoPush : List a -> LIFO a -> LIFO a
lifoPush =
    (::)


lifoPop : LIFO a -> ( Maybe a, LIFO a )
lifoPop lifo =
    case lifo of
        [] ->
            ( Nothing, lifo )

        [] :: tail ->
            lifoPop tail

        (hHead :: hTail) :: tail ->
            ( Just hHead, hTail :: tail )
