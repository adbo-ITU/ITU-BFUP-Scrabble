module internal ScrabbleBot

open ScrabbleUtil.DebugPrint

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
      moves: list<coord * (uint32 * (char * int))>
      createdWord: list<char> }

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

let rec findStartOfWord (curPos: coord) (state: gameState) (invertedDirectionVector: coord) =
    let nextPos =
        Utils.addCoords curPos invertedDirectionVector

    match Map.containsKey nextPos state.placedTiles with
    | true -> findStartOfWord nextPos state invertedDirectionVector
    | false -> curPos

let getLetter pos state =
    Map.find pos state.placedTiles |> snd |> fst

/// Should be used to "start" a word with what is already placed on the board.
let initMoveWithExistingWord (pos: coord) (state: gameState) (directionVector: int * int) =
    let invertedDirectionVector =
        (-(fst directionVector), -(snd directionVector))

    let startPos =
        findStartOfWord pos state invertedDirectionVector

    let numStartLetters =
        match directionVector with
        | (x, _) when x <> 0 -> (fst pos) - (fst startPos) |> abs
        | (_, y) when y <> 0 -> (snd pos) - (snd startPos) |> abs
        | _ -> failwith "initMoveWithExistingWord: invalid direction vector"

    if numStartLetters = 0 then
        { cursor = pos
          dict = state.dict
          moves = []
          createdWord = [] }
    else
        let firstLetter = getLetter startPos state

        // We insert the first letter, then do a reverse step in the dictionary. We
        // are guaranteed that this word exists, because if it does not, an illegal
        // move has been made.
        let startDict =
            ScrabbleUtil.Dictionary.step firstLetter state.dict
            |> Utils.flatMap (fun (isWord, subDict) ->
                // When going right or down, the first seen letter is the first
                // of the word. When going left or up, it is the last.
                match directionVector with
                | (x, y) when x > 0 || y > 0 -> ScrabbleUtil.Dictionary.reverse subDict
                | _ -> Some(isWord, subDict))
            |> (function
            | Some (_, s) -> s
            | _ -> failwith "illegal initial board state")

        // We initialise our move state with the first letter of the existing word.
        let startMoveState =
            { cursor = Utils.addCoords startPos directionVector
              dict = startDict
              moves = []
              createdWord = [ firstLetter ] }

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
                    createdWord = nextLetter :: moveState.createdWord })

        let initialMoveState =
            Seq.fold (fun acc _ -> Utils.flatMap folder acc) (Some startMoveState) (Seq.init (numStartLetters - 1) id)
            |> (function
            | Some s -> s
            | _ -> failwith "illegal initial board state")

        initialMoveState

let validateTilePlacement (pos: coord) (letter: char) (state: gameState) (direction: coord) =
    let tileExists pos = Map.containsKey pos state.placedTiles

    let perpendicularDirection (x, y) = (y, x)
    let (dx, dy) = perpendicularDirection direction

    let isValidWord startPos endPos =
        let folder acc curPos =
            match acc with
            | None -> None
            | Some (_, dict) ->
                let letter =
                    match curPos with
                    | curPos' when curPos' = pos -> letter
                    | curPos' -> getLetter curPos' state

                let res = ScrabbleUtil.Dictionary.step letter dict
                debugPrint (sprintf "Result from STEP with %A: %A\n" letter (res |> Option.map fst))
                res

        let coordsToCheckAbove = Utils.coordsBetween pos startPos

        debugPrint (sprintf "Coordinates above pos %A: %A\n" pos coordsToCheckAbove)

        let coordsToCheckBelow =
            Utils.coordsBetween pos endPos |> Seq.skip 1

        debugPrint (sprintf "Coordinates below pos %A: %A\n" pos coordsToCheckBelow)

        let acc =
            Seq.fold folder (Some(false, state.dict)) coordsToCheckAbove
            |> Utils.flatMap (fun (_, dict) -> ScrabbleUtil.Dictionary.reverse dict)

        let result = Seq.fold folder acc coordsToCheckBelow

        match result with
        | Some (isWord, _) -> isWord
        | None -> false

    // Check if the placed tile forms a crossword
    if
        tileExists (Utils.addCoords pos (dx, dy))
        || tileExists (Utils.addCoords pos (-dx, -dy))
    then
        let startPos = findStartOfWord pos state (0, -1)
        let endPos = findStartOfWord pos state (0, 1)

        debugPrint (sprintf "Validating word between %A and %A.\n" startPos endPos)

        isValidWord startPos endPos
    else
        true

let rec tryFindValidMove (state: gameState) (moveState: MoveState) (direction: coord) =
    let handleTileId tileId =
        let handleLetter (ch, points) =
            debugPrint (
                sprintf
                    "MOVE STATE Letter: %A. Cursor: %A. Word acc: %A\n"
                    ch
                    moveState.cursor
                    (List.map (fun (_, (_, (l, _))) -> l) moveState.moves)
            )

            // TODO: Optimise. Don't calculate this before dict.step gives Some
            let isLegalPlacement =
                let b =
                    validateTilePlacement moveState.cursor ch state direction

                debugPrint (sprintf "Found %s placement!\n" (if b then "a legal" else "an illegal"))
                b

            let stepAndContinue =
                let expandOption res' (moveState': MoveState) =
                    match res' with
                    | Some (isWord, dict) -> Some(isWord, { moveState' with dict = dict })
                    | None -> None

                let stepWithTile =
                    ScrabbleUtil.Dictionary.step ch moveState.dict
                    |> Utils.flatMap (fun res ->
                        // If this is the first tile to be looked at, we need to use reverse
                        match List.length moveState.createdWord with
                        | 0 -> expandOption (ScrabbleUtil.Dictionary.reverse (snd res)) moveState
                        | _ -> Some(fst res, { moveState with dict = snd res }))
                    |> Option.map (fun (isWord, ms) -> (isWord, { ms with createdWord = ch :: ms.createdWord }))

                let stepWithExisting (isWord, moveState') =
                    let endPos =
                        findStartOfWord moveState'.cursor state direction

                    let coords =
                        Utils.coordsBetween moveState'.cursor endPos
                        |> Seq.skip 1

                    let folder acc curPos =
                        match acc with
                        | Some (_, moveState'') ->
                            let letter = getLetter curPos state

                            let res' =
                                ScrabbleUtil.Dictionary.step letter moveState''.dict

                            debugPrint (sprintf "Hit already exisiting tile with letter %A.\n" letter)

                            expandOption
                                res'
                                { moveState'' with
                                    cursor = Utils.addCoords moveState''.cursor direction
                                    createdWord = letter :: moveState''.createdWord }
                        | None -> None

                    Seq.fold folder (Some(isWord, moveState')) coords

                let checkWordIfLeftOrUp (isWord, ms) =
                    match direction with
                    | (x, y) when x < 0 || y < 0 ->
                        match ScrabbleUtil.Dictionary.reverse ms.dict with
                        | Some (reverseIsWord, _) -> Some(reverseIsWord, ms)
                        | None -> Some(isWord, ms)
                    | _ -> Some(isWord, ms)

                stepWithTile
                |> Utils.flatMap stepWithExisting
                |> Utils.flatMap checkWordIfLeftOrUp

            let placement =
                (moveState.cursor, (tileId, (ch, points)))

            match stepAndContinue with
            | Some (isWord, moveState') when isWord && isLegalPlacement -> Some(placement :: moveState'.moves)
            | Some (_, moveState') when isLegalPlacement ->
                tryFindValidMove
                    { state with hand = MultiSet.removeSingle tileId state.hand }
                    { moveState' with
                        dict = moveState'.dict
                        cursor = Utils.addCoords moveState'.cursor direction
                        moves = placement :: moveState'.moves }
                    direction
            | _ -> None

        let folder acc piece =
            match acc with
            | Some (_) -> acc
            | None -> handleLetter piece

        let tile = Map.find tileId state.pieces
        Set.fold folder None tile

    let folder acc tileId _ =
        match acc with
        | Some (_) -> acc
        | None -> handleTileId tileId

    MultiSet.fold folder None state.hand

let findMoveOnSquare (pos: coord) (state: gameState) =
    let directions =
        [ (-1, 0) // Left
          (0, -1) // Up
          (1, 0) // Right
          (0, 1) ] // Down

    // TODO: remove me
    let directions = [ (0, 1) ]

    let result =
        List.fold
            (fun res direction ->
                match res with
                | None -> tryFindValidMove state (initMoveWithExistingWord pos state direction) direction
                | _ -> res)
            None
            directions

    // TODO: Some opposite direction logic in gaddag

    // pretty print result
    let prettifyResult =
        function
        | Some (placements) ->
            Seq.rev placements
            |> Seq.fold
                (fun acc placement ->
                    let (pos, (_tileId, (ch, _points))) = placement
                    sprintf "%s\n- %c at %A" acc ch pos)
                "Found move:"
        | None -> "No move found"

    debugPrint (sprintf "Result: %A" (prettifyResult result))

// TODO: Handle outside of board, handle holes in board etc.
// TODO: use useAllPossibleSpawnPositions to find all possible start locations,
//       then try to find move on each spawn location.
// TODO: Implement a function to find a move on a given square - will explore
//       up, down, left, right
// TODO: Handle first move on the board
let findPlay (state: gameState) = findMoveOnSquare (-1, -5) state
