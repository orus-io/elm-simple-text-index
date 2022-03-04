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


indexAdd : SampleData -> Index.Index SampleData -> Index.Index SampleData
indexAdd =
    Index.add sampleDataIndexConfig


indexSearch : String -> Index.Index SampleData -> List SampleData
indexSearch =
    Index.search sampleDataIndexConfig


suite : Test
suite =
    describe "TextSimpleIndex"
        [ test "search in empty index returns nothing" <|
            \_ ->
                Index.empty
                    |> indexSearch "anything"
                    |> Expect.equalLists []
        , test "search 1 word" <|
            \_ ->
                Index.empty
                    |> indexAdd sample0
                    |> indexSearch "ampl"
                    |> Expect.equalLists [ sample0 ]
        , test "search multiple words" <|
            \_ ->
                Index.empty
                    |> indexAdd sample0
                    |> indexSearch "sample one kind"
                    |> Expect.equalLists [ sample0 ]
        ]
