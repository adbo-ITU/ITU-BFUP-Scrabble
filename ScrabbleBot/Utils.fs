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
