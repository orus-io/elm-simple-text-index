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

import List.Extra as List
import SimpleTextIndex.Tree as Tree exposing (Tree)


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
config =
    Config


{-|

    An Index holds data to quickly find doc document given doc substring of its
    indexed fields

-}
type alias Index doc =
    Tree doc


{-|

    Creates doc new empty Index

-}
empty : Index doc
empty =
    Tree.empty


{-|

    Add a document to an index

-}
add : Config doc -> doc -> Index doc -> Index doc
add (Config cfg) value index =
    List.map (\extract -> extract value)
        cfg.fields
        |> List.concatMap (cfg.normalize >> tokenize)
        |> List.foldl
            (\s ->
                Tree.insert s value
            )
            index


{-|

    Search a document in the index

-}
search : Config doc -> String -> Index doc -> List doc
search (Config cfg) text index =
    String.split " " text
        |> List.map (\word -> Tree.search word index)
        |> List.foldl1 (intersectionBy cfg.ref)
        |> Maybe.withDefault []
        |> List.uniqueBy cfg.ref


tokenize : String -> List String
tokenize value =
    value
        |> String.split " "
        |> List.concatMap
            tokenizeWord
        |> List.unique


tokenizeWord : String -> List String
tokenizeWord s =
    if s == "" then
        []

    else
        s :: tokenizeWord (String.dropLeft 1 s)


intersectionBy : (doc -> comparable) -> List doc -> List doc -> List doc
intersectionBy getRef l1 l2 =
    let
        l2refs : List comparable
        l2refs =
            List.map getRef l2
    in
    List.filter (\doc -> List.member (getRef doc) l2refs) l1