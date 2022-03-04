module SimpleTextIndex exposing
    ( Index, Config
    , empty, config
    , add
    , search
    )

{-|


# Types

@docs Index, Config


# Init

@docs empty, config


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
        }


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
                Trie.getBranch word index |> List.concat
            )
        |> List.foldl1 (intersectionBy cfg.ref)
        |> Maybe.withDefault []
        |> List.uniqueBy cfg.ref
