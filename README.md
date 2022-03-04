# elm simple text index

A simple full text search index.

Does NO fuzzy searches, and is decently fast.

For more advanced indexing,
[rluiten/elm-text-search](https://package.elm-lang.org/packages/rluiten/elm-text-search)
is a much more complete alternative.

## Quick Start

```elm
import SimpleTextIndex


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
    , normalize = String.toLower --  >> String.Normalize.removeDiacritics
    }


sample0 : SampleData
sample0 =
    { id = "sample0"
    , name = "Sample 0"
    , description = "It's one of a kind !"
    }


main =
    let
        -- this would typically be done in a 'init' or 'update' function
        index =
            SimpleTextIndex.empty
                |> SimpleTextIndex.add indexConfig sample0
    in
    -- And this would be done in the 'view', and would returns `[ sample0 ]`
    search indexConfig "ampl" index

```

## How fast

I did a few measurements in a in-house application and some benchmarks.
Although very subjective, here are the results:

Indexing ~10,000 records with little text (firstname & lastname) takes under
a second in most cases.

Searching a long word is very fast (<3µs), and searching a single letter slower
(<3ms) but still fast enough for a very responsive auto-suggestion dropbox.

You can run the benchmarks yourself by viewing the 'benchmarks/main.html' file
in a browser.
