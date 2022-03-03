module TestTree exposing (suite)

import Expect
import Fuzz
import Set
import SimpleTextIndex.Tree exposing (empty, insert, search)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Tree"
        [ test "search prefix" <|
            \_ ->
                empty
                    |> insert "hello" "HELLO"
                    |> search "hel"
                    |> Set.fromList
                    |> Expect.equalSets (Set.singleton "HELLO")
        , test "search suffix" <|
            \_ ->
                empty
                    |> insert "hello" "HELLO"
                    |> search "lo"
                    |> Set.fromList
                    |> Expect.equalSets (Set.singleton "HELLO")
        , test "search substring" <|
            \_ ->
                empty
                    |> insert "hello" "HELLO"
                    |> search "ll"
                    |> Set.fromList
                    |> Expect.equalSets (Set.singleton "HELLO")
        , fuzz Fuzz.string "insert&search" <|
            \s ->
                empty
                    |> insert s "HELLO"
                    |> search s
                    |> Set.fromList
                    |> Expect.equalSets (Set.singleton "HELLO")
        , fuzz (Fuzz.tuple ( Fuzz.string, Fuzz.list (Fuzz.string |> Fuzz.map (String.left 100)) )) "random insert&lookup" <|
            \( key, keys ) ->
                List.foldl (\s -> insert s s) empty keys
                    |> search key
                    |> Set.fromList
                    |> Expect.equalSets
                        (keys
                            |> List.filter (String.contains key)
                            |> Set.fromList
                        )
        ]
