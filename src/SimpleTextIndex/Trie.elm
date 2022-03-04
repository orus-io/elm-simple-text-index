module SimpleTextIndex.Trie exposing
    ( Trie
    , empty
    , getBranch
    , insert
    )

import Dict exposing (Dict)


type Node a
    = Node (List a) (EdgeSet a)


type alias EdgeSet a =
    Dict Char (Node a)


type alias Trie a =
    Node a


empty : Trie a
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


nodeItems : Node a -> List a
nodeItems (Node items _) =
    items


insert : String -> a -> Trie a -> Trie a
insert =
    String.toList
        >> insertPath


insertPath : List Char -> a -> Trie a -> Trie a
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


getBranch : String -> Trie a -> List (List a)
getBranch text =
    lookup text
        >> Maybe.map nodeAllItems
        >> Maybe.withDefault []


visitHelp : List (Node a) -> List (Node a) -> List (Node a)
visitHelp queue accumulator =
    case queue of
        [] ->
            accumulator

        node :: tail ->
            case node of
                Node _ edges ->
                    visitHelp (Dict.values edges ++ tail) (node :: accumulator)


nodeAllItems : Node a -> List (List a)
nodeAllItems node =
    visitHelp [ node ] []
        |> List.map nodeItems


lookup : String -> Node a -> Maybe (Node a)
lookup text node =
    if text == "" then
        Just node

    else
        findPath (String.toList text) node
            |> List.head
            |> Maybe.andThen Tuple.second
