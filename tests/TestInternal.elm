module TestInternal exposing (suite)

import Expect
import Internal
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Internal"
        [ describe "tokenizeWord"
            [ test "basic" <|
                \_ ->
                    Internal.tokenizeWord [] "basic"
                        |> Set.fromList
                        |> Expect.equalSets (Set.fromList [ "basic", "asic", "sic", "ic", "c" ])
            ]
        , describe "tokenize"
            [ test "one word" <|
                \_ ->
                    Internal.tokenize "basic"
                        |> Set.fromList
                        |> Expect.equalSets (Set.fromList [ "basic", "asic", "sic", "ic", "c" ])
            , test "two words" <|
                \_ ->
                    Internal.tokenize "basic test"
                        |> Set.fromList
                        |> Expect.equalSets (Set.fromList [ "basic", "asic", "sic", "ic", "c", "test", "est", "st", "t" ])
            , test "two words with common suffix" <|
                \_ ->
                    Internal.tokenize "basic jurassic"
                        |> Set.fromList
                        |> Expect.equalSets (Set.fromList [ "basic", "asic", "sic", "ic", "c", "jurassic", "urassic", "rassic", "assic", "ssic" ])
            , test "many whitespaces" <|
                \_ ->
                    Internal.tokenize "  \nbasic  : \n\tvery basic"
                        |> Set.fromList
                        |> Expect.equalSets (Set.fromList [ "basic", "asic", "sic", "ic", "c", ":", "very", "ery", "ry", "y" ])
            ]
        , describe "intersectionBy" <|
            [ test "empty lists" <|
                \_ ->
                    Internal.intersectionBy identity [] []
                        |> Expect.equalLists []
            , test "no common elements" <|
                \_ ->
                    Internal.intersectionBy identity [ "a", "b", "c" ] [ "d", "e", "f" ]
                        |> Expect.equalLists []
            , test "with common elements" <|
                \_ ->
                    Internal.intersectionBy identity [ "a", "b", "c" ] [ "b", "e", "a" ]
                        |> Set.fromList
                        |> Expect.equalSets (Set.fromList [ "a", "b" ])
            , test "same elements" <|
                \_ ->
                    Internal.intersectionBy identity [ "a", "b", "c" ] [ "b", "c", "a" ]
                        |> Set.fromList
                        |> Expect.equalSets (Set.fromList [ "a", "b", "c" ])
            ]
        ]
