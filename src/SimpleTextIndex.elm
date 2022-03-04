module SimpleTextIndex exposing
    ( Index, empty
    , Config, config, setMaxResultSize
    , add
    , search
    )

{-|


# Init

@docs Index, empty


# Configure

@docs Config, config, setMaxResultSize


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
type alias Index doc =
    Trie doc


{-| Creates doc new empty Index
-}
empty : Index doc
empty =
    Trie.empty


{-| Add a document to an index
-}
add : Config doc -> doc -> Index doc -> Index doc
add (Config cfg) value index =
    List.map (\extract -> extract value)
        cfg.fields
        |> List.concatMap (cfg.normalize >> tokenize)
        |> List.unique
        |> List.foldl
            (\s ->
                Trie.insert s value
            )
            index


{-| Search a document in the index
-}
search : Config doc -> String -> Index doc -> List doc
search (Config cfg) text index =
    String.split " " text
        |> List.map
            (\word ->
                Trie.getBranch 100 word index
            )
        |> List.foldl1 (intersectionBy cfg.ref)
        |> Maybe.withDefault []
        |> List.uniqueBy cfg.ref
