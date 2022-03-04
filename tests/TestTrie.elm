module TestTrie exposing (suite)

import Expect
import Fuzz
import Set
import SimpleTextIndex.Trie exposing (empty, getBranch, insert)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Trie"
        [ test "get" <|
            \_ ->
                empty
                    |> insert "hello" "HELLO"
                    |> getBranch "hel"
                    |> List.concat
                    |> Set.fromList
                    |> Expect.equalSets (Set.singleton "HELLO")
        , fuzz Fuzz.string "insert&get" <|
            \s ->
                empty
                    |> insert s "HELLO"
                    |> getBranch s
                    |> List.concat
                    |> Set.fromList
                    |> Expect.equalSets (Set.singleton "HELLO")
        , fuzz (Fuzz.tuple ( Fuzz.string, Fuzz.list Fuzz.string )) "random insert&lookup" <|
            \( key, keys ) ->
                List.foldl (\s -> insert s s) empty keys
                    |> getBranch key
                    |> List.concat
                    |> Set.fromList
                    |> Expect.equalSets
                        (keys
                            |> List.filter (String.startsWith key)
                            |> Set.fromList
                        )
        ]
