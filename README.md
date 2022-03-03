# elm simple text index

A simple full text search index.

Does NO fuzzy searches, and is decently fast.

For more advanced indexing,
[rluiten/elm-text-search](https://package.elm-lang.org/packages/rluiten/elm-text-search)
is a much more complete alternative.

## Quick Start

```elm
import SimpleTextIndex exposing (empty, add, search)


type alias SampleData =
    { id : String
    , name : String
    , description : String
    }


indexConfig : Index.Config SampleData
indexConfig =
    { ref = .id
    , fields =
        [ .name
        , .description
        ]
    , normalize = String.toLower  --  >> String.Normalize.removeDiacritics
    }


sample0 : SampleData
sample0 =
    { id = "sample0"
    , name = "Sample 0"
    , description = "It's one of a kind !"
    }




main :
    let
        index = empty
            |> add indexConfig sample0
    in
    search indexConfig "ampl" index
      -- returns [sample0]
```
