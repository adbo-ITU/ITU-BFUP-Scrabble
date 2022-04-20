module internal ScrabbleBot

type coord = int * int
type tile = Set<char * int>

type gameState =
    { board: Parser.board
      dict: ScrabbleUtil.Dictionary.Dict
      hand: MultiSet.MultiSet<uint32>
      pieces: Map<uint32, tile>
      placedTiles: Map<coord, uint32 * (char * int)> }

let rec foo (state: gameState) wordAcc =
    ScrabbleUtil.DebugPrint.debugPrint (sprintf "current word acc: %A\n" wordAcc)

    let aux tileId =
        let lulw (ch, _) =
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "dict step: %A\n" (ScrabbleUtil.Dictionary.step ch state.dict))

            match ScrabbleUtil.Dictionary.step ch state.dict with
            | Some (isWord, _) when isWord -> Some(ch :: wordAcc)
            | Some (_, subDict) ->
                foo
                    { state with
                        dict = subDict
                        hand = MultiSet.removeSingle tileId state.hand }
                    (ch :: wordAcc)
            | None -> None

        let folder acc piece =
            match acc with
            | Some (_) -> acc
            | None -> lulw piece

        let tile = Map.find tileId state.pieces
        Set.fold folder None tile

    let folder acc tileId _ =
        match acc with
        | Some (_) -> acc
        | None -> aux tileId

    MultiSet.fold folder None state.hand

let findWord (state: gameState) firstLetter =
    let convertResultToWord result =
        let sentinelIndex =
            List.findIndex (fun ch -> ch = '#') result

        let seq = Seq.ofList result

        let first =
            Seq.take sentinelIndex seq
            |> Seq.rev
            |> Seq.toList

        let second =
            Seq.skip (sentinelIndex + 1) seq |> Seq.toList

        first @ second

    match ScrabbleUtil.Dictionary.step firstLetter state.dict with
    | Some (_, subDict) -> foo { state with dict = subDict } [ firstLetter ]
    | None -> None
    |> Option.map convertResultToWord
