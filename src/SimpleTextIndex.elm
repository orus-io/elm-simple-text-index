module SimpleTextIndex exposing
    ( Index, new
    , Config, config, setMaxResultSize, setConfig
    , add
    , search
    )

{-|


# Init

@docs Index, new


# Configure

@docs Config, config, setMaxResultSize, setConfig


# Insert data

@docs add


# Search

@docs search

-}

import Internal exposing (intersectionBy, tokenize)
import List.Extra as List
import SimpleTextIndex.Trie as Trie exposing (Trie)


{-| A Config is required to add/search in an Index
-}
type Config doc
    = Config
        { ref : doc -> String
        , fields : List (doc -> String)
        , normalize : String -> String
        , maxResultSize : Int
        }


{-| Create doc config

  - `ref` extracts the unique id of a document. It will be used to deduplicate
    results

  - `fields` is a list of content extractors. They return the text to index.

  - `normalize` is called to simplify strings before being indexed, and on the
    searched text.

A typical config is:

    { ref = .id
    , fields = [ .name, .description ]
    , normalize = Sring.toLower >> String.Normalize.removeDiacritics
    }

-}
config :
    { ref : doc -> String
    , fields : List (doc -> String)
    , normalize : String -> String
    }
    -> Config doc
config { ref, fields, normalize } =
    Config
        { ref = ref
        , fields = fields
        , normalize = normalize
        , maxResultSize = 1000
        }


{-| Set the maximum number of results to return on a search

The default value is 1000

This value is indicative only and result may be bigger

-}
setMaxResultSize : Int -> Config a -> Config a
setMaxResultSize size (Config cfg) =
    Config { cfg | maxResultSize = size }


{-| An Index holds data to quickly find doc document given doc substring of its
indexed fields
-}
type Index doc
    = Index (Config doc) (Trie doc)


{-| Creates doc new empty Index
-}
new : Config doc -> Index doc
new cfg =
    Index cfg Trie.empty


{-| Update the index configuration
-}
setConfig : Config doc -> Index doc -> Index doc
setConfig cfg (Index _ trie) =
    Index cfg trie


{-| Add a document to an index
-}
add : doc -> Index doc -> Index doc
add value (Index (Config cfg) trie) =
    List.map (\extract -> extract value)
        cfg.fields
        |> List.concatMap (cfg.normalize >> tokenize)
        |> List.unique
        |> List.foldl
            (\s ->
                Trie.insert s value
            )
            trie
        |> Index (Config cfg)


{-| Search a document in the index
-}
search : String -> Index doc -> List doc
search text (Index (Config cfg) trie) =
    String.split " " text
        |> List.map
            (\word ->
                Trie.getBranch 100 word trie
            )
        |> List.foldl1 (intersectionBy cfg.ref)
        |> Maybe.withDefault []
        |> List.uniqueBy cfg.ref
