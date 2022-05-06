// After submission for Assignment 4, this module was tinkered with a lot in
// collaboration with albn@itu.dk, so if some parts are quite similar, that's
// why. Just to have it in writing and to make clear it's not plagiarism ;)
module Dictionary

type Item =
    | Char of char
    | Sentinel

type Dict = D of bool * CsMap.CsMap<Item, Dict>

let SentinelChar = '#'

let empty (_: unit) = D(false, CsMap.empty ())

let makeItem c =
    if c <> SentinelChar then
        Char(System.Char.ToUpper c)
    else
        Sentinel

// Straight from Albert (albn)
type System.Collections.Generic.Dictionary<'a, 'b> with
    member x.GetValueOption(key: 'a) : 'b option =
        match x.TryGetValue(key) with
        | (true, value) -> Some(value)
        | (false, _) -> None

let normaliseInput (s: string) = Seq.toList s |> List.map makeItem

// Generates all arrangements of the letters with the sentinel in each position
let permutate items =
    let rev = List.rev items

    [ 1 .. items.Length ]
    |> List.fold
        (fun acc i ->
            (rev.[rev.Length - i..] @ (Sentinel :: items.[i..]))
            :: acc)
        []

let insert (s: string) dict =
    let rec put items (D (nodeIsWord, nodeChildren)) =
        match items with
        | [] -> D(true, nodeChildren)
        | item :: remainingItems ->
            let subtree =
                nodeChildren.GetValueOption(item)
                |> Option.defaultValue (D(false, CsMap.empty ()))
                |> put remainingItems

            nodeChildren.[item] <- subtree

            D(nodeIsWord, nodeChildren)

    normaliseInput s
    |> permutate
    |> List.fold (fun d permutation -> put permutation d) dict

let step c (D (_, children)) =
    children.GetValueOption(makeItem c)
    |> Option.map (fun child ->
        match child with
        | D (childIsWord, _) -> (childIsWord, child))

let reverse (D (_, children)) =
    children.GetValueOption(Sentinel)
    |> Option.map (fun child ->
        match child with
        | D (childIsWord, _) -> (childIsWord, child))

let lookup (s: string) dict =
    let items' = normaliseInput s
    // Because of how we create different versions of the word, if we insert a
    // word like HELLO, there will ALWAYS be a version that is H#ELLO.
    let itemsWithSentinel = items'.[0] :: Sentinel :: items'.[1..]

    let rec search node =
        function
        | [] ->
            match node with
            | D (isWord, _) -> isWord
        | item :: items ->
            let result =
                match item with
                | Char (c) -> step c node
                | Sentinel -> reverse node

            match result with
            | Some (_, child) -> search child items
            | None -> false

    search dict itemsWithSentinel
