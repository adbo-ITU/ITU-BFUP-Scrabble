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

let coordsBetween a b =
    // We know that either the horizontal or vertical distance is 0
    let dist = abs (fst b - fst a + snd b - snd a) + 1

    Seq.init (dist) (fun n ->
        addCoords
            a
            (match a with
             | (x, _) when x < fst b -> (n, 0)
             | (x, _) when x > fst b -> (0, n)
             | (_, y) when y < snd b -> (-n, 0)
             | (_, y) when y > snd b -> (0, -n)
             | _ -> failwith "coordinates not on the same axis"))

let flatMap f opt =
    match opt with
    | Some x -> f x
    | _ -> None
