﻿module internal ScrabbleBot

type coord = int * int
type tile = Set<char * int>
type placedTilesMap = Map<coord, uint32 * (char * int)>

type gameState =
    { board: Parser.board
      dict: ScrabbleUtil.Dictionary.Dict
      hand: MultiSet.MultiSet<uint32>
      pieces: Map<uint32, tile>
      placedTiles: placedTilesMap }

type MoveState =
    { cursor: coord
      dict: ScrabbleUtil.Dictionary.Dict
      wordAcc: char list }

// TODO: We do not account for single-letter words as the first move on the board

let rec foo (state: gameState) wordAcc =
    ScrabbleUtil.DebugPrint.debugPrint (sprintf "current word acc: %A\n" wordAcc)

    let aux tileId =
        let lulw (ch, _) =
            ScrabbleUtil.DebugPrint.debugPrint (sprintf "dict step: %A\n" (ScrabbleUtil.Dictionary.step ch state.dict))

            match ScrabbleUtil.Dictionary.step ch state.dict with
            | Some (isWord, _) when
                isWord
                && not (
                    List.length wordAcc = 1
                    && ch = Dictionary.SentinelChar
                )
                ->
                Some(ch :: wordAcc)
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

let findWord (state: gameState) startLetter =
    let convertResultToWord result =
        let sentinelIndex =
            List.findIndex (fun ch -> ch = Dictionary.SentinelChar) result

        let seq = Seq.ofList result

        let second =
            Seq.take sentinelIndex seq
            |> Seq.rev
            |> Seq.toList

        let first =
            Seq.skip (sentinelIndex + 1) seq |> Seq.toList

        first @ second

    match ScrabbleUtil.Dictionary.step startLetter state.dict with
    | Some (_, subDict) ->
        let piecesWithSentinel =
            Map.add 12345u (Set.ofList [ (Dictionary.SentinelChar, 0) ]) state.pieces

        let handWithSentinel = MultiSet.addSingle 12345u state.hand

        foo
            { state with
                dict = subDict
                pieces = piecesWithSentinel
                hand = handWithSentinel }
            [ startLetter ]
    | None -> None
    |> Option.map convertResultToWord

let findAdjacentEmptySquares ((x, y): coord) (placedTiles: placedTilesMap) =
    let directionVectors = [ (0, 1); (1, 0); (0, -1); (-1, 0) ]

    List.fold
        (fun found (dx, dy) ->
            let adjacentPos = (x + dx, y + dy)

            match Map.containsKey adjacentPos placedTiles with
            | true -> found
            | false -> adjacentPos :: found)
        []
        directionVectors

/// Returns a set of all squares on the board that are empty and adjacent to a
/// placed tile.
// TODO: Handle outside of board, handle holes in board etc.
let findAllPossibleSpawnPositions (state: gameState) =
    Seq.fold
        (fun acc pos ->
            findAdjacentEmptySquares pos state.placedTiles
            |> List.fold (fun acc' adj -> Set.add adj acc') acc)
        Set.empty
        (Map.keys state.placedTiles)

/// Should be used to "start" a word with what is already placed on the board.
/// Should only be used when direction is right or down!
let initMoveWithExistingWord (pos: coord) (state: gameState) (directionVector: int * int) =
    let invertedDirectionVector =
        (-(fst directionVector), -(snd directionVector))

    let rec findStartOfWord curPos =
        let nextPos =
            Utils.addCoords curPos invertedDirectionVector

        match Map.containsKey nextPos state.placedTiles with
        | true -> findStartOfWord nextPos
        | false -> curPos

    let startPos = findStartOfWord pos

    let numStartLetters =
        match directionVector with
        | (x, _) when x > 0 -> (fst pos) - (fst startPos)
        | (_, y) when y > 0 -> (snd pos) - (snd startPos)
        | _ -> failwith "initMoveWithExistingWord: invalid direction vector"

    let firstLetter =
        Map.find startPos state.placedTiles |> snd |> fst

    // We insert the first letter, then do a reverse step in the dictionary. We
    // are guaranteed that this word exists, because if it does not, an illegal
    // move has been made.
    let startDict =
        ScrabbleUtil.Dictionary.step firstLetter state.dict
        |> Utils.flatMap (fun (_, subDict) -> ScrabbleUtil.Dictionary.reverse subDict)
        |> (function
        | Some (_, s) -> s
        | _ -> failwith "illegal initial board state")

    // We initialise our move state with the first letter of the existing word.
    let startMoveState =
        { cursor = Utils.addCoords startPos directionVector
          dict = startDict
          wordAcc = [ firstLetter ] }

    // We now add all the next letters in the word to the move state.
    let folder (moveState: MoveState) =
        let nextLetter =
            Map.find moveState.cursor state.placedTiles
            |> snd
            |> fst

        ScrabbleUtil.Dictionary.step nextLetter moveState.dict
        |> Option.map (fun (_, subDict) ->
            { moveState with
                dict = subDict
                cursor = Utils.addCoords moveState.cursor directionVector
                wordAcc = nextLetter :: moveState.wordAcc })

    let initialMoveState =
        Seq.fold (fun acc _ -> Utils.flatMap folder acc) (Some startMoveState) (Seq.init (numStartLetters - 1) id)
        |> (function
        | Some s -> s
        | _ -> failwith "illegal initial board state")

    initialMoveState

// TODO: Handle outside of board, handle holes in board etc.
// TODO: use useAllPossibleSpawnPositions to find all possible start locations,
//       then try to find move on each spawn location.
// TODO: Implement a function to find a move on a given square - will explore
//       up, down, left, right
let findPlay (state: gameState) =
    initMoveWithExistingWord (1, 3) state (0, 1)
