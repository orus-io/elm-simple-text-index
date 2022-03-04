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


nodeEdges : Node a -> EdgeSet a
nodeEdges (Node _ edges) =
    edges


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


getBranch : Int -> String -> Trie a -> List a
getBranch limit text =
    lookup text
        >> Maybe.map (nodeAllItems limit >> List.concat)
        >> Maybe.withDefault []


type VisitResult a
    = ContinueVisit a
    | StopVisit a


visitHelp : ( List (Node a), List (Node a) ) -> (Node a -> b -> VisitResult b) -> b -> b
visitHelp queues visit previousStep =
    case queues of
        ( [], [] ) ->
            previousStep

        ( [], nextQueue ) ->
            visitHelp ( nextQueue, [] ) visit previousStep

        ( node :: queue, nextQueue ) ->
            case visit node previousStep of
                StopVisit res ->
                    res

                ContinueVisit res ->
                    visitHelp
                        ( queue, List.append (Dict.values (nodeEdges node)) nextQueue )
                        visit
                        res


nodeAllItems : Int -> Node a -> List (List a)
nodeAllItems limit node =
    visitHelp ( [ node ], [] )
        (\n ( l, s ) ->
            let
                items : List a
                items =
                    nodeItems n

                size : Int
                size =
                    List.length items + s
            in
            ( items :: l, size )
                |> (if size < limit then
                        ContinueVisit

                    else
                        StopVisit
                   )
        )
        ( [], 0 )
        |> Tuple.first


lookup : String -> Node a -> Maybe (Node a)
lookup text node =
    if text == "" then
        Just node

    else
        findPath (String.toList text) node
            |> List.head
            |> Maybe.andThen Tuple.second
