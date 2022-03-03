module SimpleTextIndex.Tree exposing
    ( Tree
    , empty
    , insert
    , search
    )

import Dict exposing (Dict)


type Node a
    = Node (List a) (EdgeSet a)


type alias EdgeSet a =
    Dict Char (Node a)


type alias Tree a =
    Node a


empty : Tree a
empty =
    emptyNode


emptyNode : Node a
emptyNode =
    Node [] Dict.empty


nodeSetEdge : Char -> Node a -> Node a -> Node a
nodeSetEdge c target (Node items targets) =
    Node items <| Dict.insert c target targets


nodeAddItem : a -> Node a -> Node a
nodeAddItem a (Node items targets) =
    Node (a :: items) targets


expandPaths : String -> List (List Char)
expandPaths s =
    let
        path : List Char
        path =
            String.toList s
    in
    List.range 0 (List.length path)
        |> List.map (\i -> List.drop i path)


insert : String -> a -> Tree a -> Tree a
insert key value tree =
    expandPaths key
        |> List.foldl (\path -> insertPath path value) tree


insertPath : List Char -> a -> Tree a -> Tree a
insertPath path value tree =
    case
        findPath path tree
            |> List.foldl
                (\( c, current ) child ->
                    let
                        node : Node a
                        node =
                            current |> Maybe.withDefault emptyNode
                    in
                    case child of
                        Just ( childC, childNode ) ->
                            Just <| ( c, nodeSetEdge childC childNode node )

                        Nothing ->
                            Just <| ( c, nodeAddItem value node )
                )
                Nothing
    of
        Just ( c, node ) ->
            nodeSetEdge c node tree

        Nothing ->
            nodeAddItem value tree


findPath : List Char -> Node a -> List ( Char, Maybe (Node a) )
findPath path node =
    List.foldl
        (\c ( parent, result ) ->
            case parent of
                Nothing ->
                    ( Nothing, ( c, Nothing ) :: result )

                Just (Node _ edges) ->
                    let
                        nextNode : Maybe (Node a)
                        nextNode =
                            Dict.get c edges
                    in
                    ( nextNode, ( c, nextNode ) :: result )
        )
        ( Just node, [] )
        path
        |> Tuple.second


nodeAllItems : Node a -> List (List a)
nodeAllItems (Node items edges) =
    items :: (edges |> Dict.values |> List.concatMap nodeAllItems)


lookup : String -> Node a -> Maybe (Node a)
lookup text node =
    if text == "" then
        Just node

    else
        findPath (String.toList text) node
            |> List.head
            |> Maybe.andThen Tuple.second


search : String -> Tree a -> List a
search text =
    lookup text
        >> Maybe.map nodeAllItems
        >> Maybe.map List.concat
        >> Maybe.withDefault []
