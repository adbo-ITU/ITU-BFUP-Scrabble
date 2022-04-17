module internal ScrabbleBot

type coord = int * int
type tile = Set<char * int>

type gameState =
    { board: Parser.board
      dict: ScrabbleUtil.Dictionary.Dict
      hand: MultiSet.MultiSet<uint32>
      pieces: Map<uint32, tile>
      placedTiles: Map<coord, uint32 * (char * int)> }



let rec searchDict (state : gameState) (currentLetter : char) =
    let rec bar state acc (ch, pv) =
        match ScrabbleUtil.Dictionary.step ch state.dict with
        | Some (isWord, _) when isWord -> ch :: acc
        | Some (_, dict) -> searchDict { state with dict = dict} ch
        | None -> acc 

    let rec foo state acc tileID amount =
        let acc' = Set.fold (bar state) List.empty (Map.find tileID state.pieces)
        //if List.isEmpty acc then acc' :: acc else acc
    
    let hand' = MultiSet.toList state.hand
    List.find (foo state) hand'

    //MultiSet.fold (foo state) List.empty state.hand

let computeMove (state : gameState) =
    // Iterate through each tile
    // For each tile, find all valid words starting with this tile
    
    let firstTile = Map.toList state.placedTiles |> List.head
    let firstLetter = firstTile |> snd |> snd |> fst
    let move = searchDict state firstLetter

    List.empty // (coord, tile) list


