module TestSimpleIndex exposing (suite)

import Expect
import SimpleTextIndex as Index
import String.Normalize
import Test exposing (Test, describe, test)


type alias SampleData =
    { id : String
    , name : String
    , description : String
    }


sampleDataIndexConfig : Index.Config SampleData
sampleDataIndexConfig =
    Index.config
        { ref = .id
        , fields =
            [ .name
            , .description
            ]
        , normalize = String.toLower >> String.Normalize.removeDiacritics
        }


sample0 : SampleData
sample0 =
    { id = "sample0"
    , name = "Sample 0"
    , description = "It's one of a kind !"
    }


sample1 : SampleData
sample1 =
    { id = "SAMPLE1"
    , name = "SAMPLE 1"
    , description = "IT'S BIG !"
    }


suite : Test
suite =
    describe "TextSimpleIndex"
        [ test "search in empty index returns nothing" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.search "anything"
                    |> Expect.equalLists []
        , test "search 1 word, lower2lower" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.add sample0
                    |> Index.search "ampl"
                    |> Expect.equalLists [ sample0 ]
        , test "search multiple words, lower2lower" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.add sample0
                    |> Index.search "sample one kind"
                    |> Expect.equalLists [ sample0 ]
        , test "search 1 word, upper2upper" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.add sample1
                    |> Index.search "AMPL"
                    |> Expect.equalLists [ sample1 ]
        , test "search multiple words, upper2upper" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.add sample1
                    |> Index.search "SAMPLE BIG"
                    |> Expect.equalLists [ sample1 ]
        , test "search 1 word, lower2upper" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.add sample0
                    |> Index.search "AMPL"
                    |> Expect.equalLists [ sample0 ]
        , test "search multiple words, lower2upper" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.add sample0
                    |> Index.search "SAMPLE ONE KIND"
                    |> Expect.equalLists [ sample0 ]
        , test "search 1 word, upper2lower" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.add sample1
                    |> Index.search "ampl"
                    |> Expect.equalLists [ sample1 ]
        , test "search multiple words, upper2lower" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.add sample1
                    |> Index.search "sample big"
                    |> Expect.equalLists [ sample1 ]
        ]
