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


suite : Test
suite =
    describe "TextSimpleIndex"
        [ test "search in empty index returns nothing" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.search "anything"
                    |> Expect.equalLists []
        , test "search 1 word" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.add sample0
                    |> Index.search "ampl"
                    |> Expect.equalLists [ sample0 ]
        , test "search multiple words" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.add sample0
                    |> Index.search "sample one kind"
                    |> Expect.equalLists [ sample0 ]
        ]
