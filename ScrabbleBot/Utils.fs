module internal Utils

// Taken directly from http://www.fssnip.net/qY/title/Rotate-List
let rotate (step: uint32) input =
    let ls = input |> List.ofSeq

    List.fold
        (fun (s, c) e ->
            if s <> 0u then
                (s - 1u, List.append c.Tail [ e ])
            else
                (0u, c))
        (step, ls)
        ls
    |> fun (x, y) -> y |> List.ofSeq

let addCoords (a: int * int) (b: int * int) = (fst a + fst b, snd a + snd b)

let coordsBetween a (bx, by) =
    // We know that either the horizontal or vertical distance is 0
    let dist =
        abs (fst (bx, by) - fst a + snd (bx, by) - snd a)
        + 1

    Seq.init (dist) (fun n ->
        addCoords
            a
            (match a with
             | (x, y) when x <> bx && y <> by ->
                 failwith (sprintf "coordinates are not on the same axes: %A and %A" a (bx, by))
             | (x, _) when x < bx -> (n, 0)
             | (x, _) when x > bx -> (-n, 0)
             | (_, y) when y < by -> (0, n)
             | (_, y) when y > by -> (0, -n)
             | _ -> (0, 0)))

let flatMap f opt =
    match opt with
    | Some x -> f x
    | _ -> None

let max a b = if a > b then a else b

let flattenList (lst: 'a list list) =
    List.foldBack (fun x acc -> x @ acc) lst []
