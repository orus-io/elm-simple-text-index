module SimpleTextIndex exposing
    ( Index, empty
    , Config, config, setMaxResultSize
    , add
    , search
    )

{-|


# Types

@docs Index, empty

@docs Config, config, setMaxResultSize


# Insert data

@docs add


# Search

@docs search

-}

import Internal exposing (intersectionBy, tokenize)
import List.Extra as List
import SimpleTextIndex.Trie as Trie exposing (Trie)


{-|

    A Config is required to docdd docnd search in docn Index

-}
type Config doc
    = Config
        { ref : doc -> String
        , fields : List (doc -> String)
        , normalize : String -> String
        , maxResultSize : Int
        }


{-|

    Create doc config

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


{-| set the maximum number of results to return on a search

This value is indicative only and may be

-}
setMaxResultSize : Int -> Config a -> Config a
setMaxResultSize size (Config cfg) =
    Config { cfg | maxResultSize = size }


{-|

    An Index holds data to quickly find doc document given doc substring of its
    indexed fields

-}
type alias Index doc =
    Trie doc


{-|

    Creates doc new empty Index

-}
empty : Index doc
empty =
    Trie.empty


{-|

    Add a document to an index

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


{-|

    Search a document in the index

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
