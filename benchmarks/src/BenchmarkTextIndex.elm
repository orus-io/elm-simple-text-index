module BenchmarkTextIndex exposing (suite)

import Benchmark exposing (..)
import Random
import Random.Char
import Random.String
import SimpleTextIndex as Index


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
        , normalize = String.toLower
        }


sampleData0 : SampleData
sampleData0 =
    { id = "id0"
    , name = "sample 0"
    , description = "It's one of a kind !"
    }


sampleDataList : Int -> List SampleData
sampleDataList qty =
    let
        textLenGenerator =
            Random.int 1 30

        nameGenerator =
            Random.String.rangeLengthString 4 30 Random.Char.english

        descriptionGenerator =
            textLenGenerator
                |> Random.andThen (\len -> Random.list len nameGenerator)
                |> Random.map (String.join " ")
    in
    List.range 1 qty
        |> List.foldl
            (\id ( res, seed ) ->
                let
                    ( name, seed1 ) =
                        Random.step nameGenerator seed

                    ( description, seed2 ) =
                        Random.step descriptionGenerator seed1
                in
                ( { id = "id" ++ String.fromInt id
                  , name = name
                  , description = description
                  }
                    :: res
                , seed2
                )
            )
            ( [], Random.initialSeed 42 )
        |> Tuple.first


suite : Benchmark
suite =
    let
        smallSet =
            sampleDataList 100

        mediumSet =
            sampleDataList 1000

        -- bigSet =
        -- sampleDataList 10000
        -- hugeSet =
        --   sampleDataList 100000
        emptyIndex =
            Index.new sampleDataIndexConfig

        singletonIndex =
            emptyIndex
                |> Index.add sampleData0

        smallIndex =
            smallSet
                |> List.foldl Index.add emptyIndex

        mediumIndex =
            mediumSet
                |> List.foldl Index.add emptyIndex

        -- bigIndex =
        -- bigSet
        -- |> List.foldl (Index.add ) emptyIndex
        -- hugeIndex =
        -- hugeSet
        -- |> List.foldl (Index.add ) emptyIndex
    in
    describe "TextIndex"
        [ -- nest as many descriptions as you like
          benchmark "init empty" <|
            \_ -> Index.new sampleDataIndexConfig
        , benchmark "init & insert 1 item" <|
            \_ ->
                Index.new sampleDataIndexConfig
                    |> Index.add sampleData0
        , benchmark "search singleton" <|
            \_ ->
                singletonIndex
                    |> Index.search "sample"
        , benchmark "add 1 docs to 100" <|
            \_ ->
                smallIndex
                    |> Index.add sampleData0
        , benchmark "add 1 docs to 1000" <|
            \_ ->
                mediumIndex
                    |> Index.add sampleData0
        , benchmark "precise search in medium set" <|
            \_ ->
                mediumIndex
                    |> Index.search "kind"
        , benchmark "vague search in medium set" <|
            \_ ->
                mediumIndex
                    |> Index.search "a"
        ]
