module internal ScrabbleBot

open ScrabbleUtil.DebugPrint
open System.Threading.Tasks
open System.Threading

type coord = int * int
type tile = Set<char * int>
type placedTilesMap = Map<coord, uint32 * (char * int)>

type gameState =
    { board: Parser.board
      dict: ScrabbleUtil.Dictionary.Dict
      hand: MultiSet.MultiSet<uint32>
      pieces: Map<uint32, tile>
      placedTiles: placedTilesMap
      timeout: uint32 option }

type Move = coord * (uint32 * (char * int))

type MoveState =
    { cursor: coord
      dict: ScrabbleUtil.Dictionary.Dict
      moves: list<Move>
      createdWord: list<char>
      score: int }

type Result = { score: int; moves: list<Move> }

type ResultMsg =
    | Found of Result
    | Get of AsyncReplyChannel<Result option>

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
let findAllPossibleSpawnPositions (state: gameState) =
    let locations =
        Seq.fold
            (fun acc pos ->
                findAdjacentEmptySquares pos state.placedTiles
                |> List.fold (fun acc' adj -> Set.add adj acc') acc)
            Set.empty
            (Map.keys state.placedTiles)

    match Set.count locations with
    | s when s > 0 -> locations
    | _ -> Set.ofList [ (0, 0) ]

let rec findStartOfWord (curPos: coord) (state: gameState) (invertedDirectionVector: coord) =
    let nextPos = Utils.addCoords curPos invertedDirectionVector

    match Map.containsKey nextPos state.placedTiles with
    | true -> findStartOfWord nextPos state invertedDirectionVector
    | false -> curPos

let getLetter pos state = Map.find pos state.placedTiles |> snd

/// Should be used to "start" a word with what is already placed on the board.
let initMoveWithExistingWord (pos: coord) (state: gameState) (directionVector: int * int) =
    let invertedDirectionVector = (-(fst directionVector), -(snd directionVector))
    let startPos = findStartOfWord pos state invertedDirectionVector

    let numStartLetters =
        match directionVector with
        | (x, _) when x <> 0 -> (fst pos) - (fst startPos) |> abs
        | (_, y) when y <> 0 -> (snd pos) - (snd startPos) |> abs
        | _ -> failwith "initMoveWithExistingWord: invalid direction vector"

    if numStartLetters = 0 then
        { cursor = pos
          dict = state.dict
          moves = []
          createdWord = []
          score = 0 }
    else
        let firstLetter, firstScore = getLetter startPos state

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
              createdWord = [ firstLetter ]
              score = firstScore }

        // We now add all the next letters in the word to the move state.
        let folder (moveState: MoveState) =
            let nextLetter, nextScore = getLetter moveState.cursor state

            ScrabbleUtil.Dictionary.step nextLetter moveState.dict
            |> Option.map (fun (_, subDict) ->
                { moveState with
                    dict = subDict
                    cursor = Utils.addCoords moveState.cursor directionVector
                    createdWord = nextLetter :: moveState.createdWord
                    score = nextScore + moveState.score })

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
                    | curPos' -> getLetter curPos' state |> fst

                ScrabbleUtil.Dictionary.step letter dict

        let coordsToCheckAbove = Utils.coordsBetween pos startPos
        let coordsToCheckBelow = Utils.coordsBetween pos endPos |> Seq.skip 1

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
        let (startDir, endDir) =
            match ((dx, dy), (-dx, -dy)) with
            | (l, r) when l < r -> (l, r)
            | (l, r) -> (r, l)

        let startPos = findStartOfWord pos state startDir
        let endPos = findStartOfWord pos state endDir
        isValidWord startPos endPos
    else
        true

// TODO: Should use board.defaultSquare for already placed tiles (including adjacent words)
let getWordScore (moves: Move list) (state: gameState) =
    let getSquare (coord: coord) =
        match state.board.squares coord with
        | StateMonad.Result.Success item -> Option.get item
        | _ -> failwith "square has no squareFun"

    let getResult =
        function
        | StateMonad.Result.Success res -> res
        | _ -> failwith "StateMonad failure"

    let tiles = List.map (snd >> snd) moves

    // Get a list of squareFunctions and their priorities from each square
    List.mapi
        (fun pos (coord, _) ->
            getSquare coord
            |> Map.fold (fun acc priority sqFun -> (priority, sqFun tiles pos) :: acc) [])
        moves
    // Flatten the lists to one list
    |> Utils.flattenList
    // Sort the functions by priority
    |> List.sortBy fst
    // Accumulate the score
    |> List.fold (fun acc (_, sqFun) -> sqFun acc |> getResult) 0

let moveIsWorthMore (a: MoveState) (b: MoveState) (st: gameState) =
    // TODO: Should not use only placed tiles, but all tiles in the word
    getWordScore a.moves st > getWordScore b.moves st

let keepBestResult a b st =
    match (a, b) with
    | Some a', Some b' when moveIsWorthMore a' b' st -> a
    | Some _, None -> a
    | _ -> b

let rec tryFindValidMove
    (state: gameState)
    (moveState: MoveState)
    (direction: coord)
    (resultProcessor: MailboxProcessor<ResultMsg>)
    (cancellationToken: CancellationToken)
    =
    let handleTileId tileId =
        let handleLetter (ch, points) =
            let isWithinBounds =
                match state.board.squares moveState.cursor with
                | StateMonad.Failure sm ->
                    debugPrint $"StateMonad failure: {sm}\n"
                    false
                | StateMonad.Success sm ->
                    match sm with
                    | Some _ -> true
                    | None -> false

            let validatePlacement ms =
                let isValid = validateTilePlacement ms.cursor ch state direction

                isValid && isWithinBounds

            let stepAndContinue =
                let expandOption res' (moveState': MoveState) =
                    match res' with
                    | Some (isWord, dict) -> Some(isWord, { moveState' with dict = dict })
                    | None -> None

                let stepWithTile =
                    ScrabbleUtil.Dictionary.step ch moveState.dict
                    |> Utils.flatMap (fun res ->
                        let fallback = Some(fst res, { moveState with dict = snd res })
                        // If this is the first tile to be looked at (going right or down), we need to use reverse
                        match direction with
                        | (x, y) when x > 0 || y > 0 ->
                            match List.length moveState.createdWord with
                            | 0 -> expandOption (ScrabbleUtil.Dictionary.reverse (snd res)) moveState
                            | _ -> fallback
                        | _ -> fallback)
                    |> Option.map (fun (isWord, ms) ->
                        (isWord,
                         { ms with
                             createdWord = ch :: ms.createdWord
                             score = ms.score })) // TODO: Fix

                let stepWithExisting (isWord, moveState') =
                    let endPos = findStartOfWord moveState'.cursor state direction

                    let coords =
                        Utils.coordsBetween moveState'.cursor endPos
                        |> Seq.skip 1

                    let folder acc curPos =
                        match acc with
                        | Some (_, moveState'') ->
                            let letter, score = getLetter curPos state

                            expandOption
                                (ScrabbleUtil.Dictionary.step letter moveState''.dict)
                                { moveState'' with
                                    cursor = Utils.addCoords moveState''.cursor direction
                                    createdWord = letter :: moveState''.createdWord
                                    score = score + moveState''.score }
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
                |> Option.map (fun s -> (s, validatePlacement moveState))

            let placement = (moveState.cursor, (tileId, (ch, points)))

            let updateState s =
                { s with hand = MultiSet.removeSingle tileId s.hand }

            let updateMoveState s =
                { s with
                    dict = s.dict
                    cursor = Utils.addCoords s.cursor direction
                    moves = placement :: s.moves }

            match stepAndContinue with
            | Some ((isWord, moveState'), isLegalPlacement) when isWord && isLegalPlacement ->
                let foundResult =
                    { moves = placement :: moveState'.moves
                      score = List.length moveState'.createdWord + 1 }

                resultProcessor.Post(Found foundResult)

                let next =
                    tryFindValidMove
                        (updateState state)
                        (updateMoveState moveState')
                        direction
                        resultProcessor
                        cancellationToken

                match next with
                | Some ms when moveIsWorthMore ms moveState' state -> next
                | _ ->
                    Some
                        { moveState' with
                            moves = foundResult.moves
                            score = foundResult.score }

            | Some ((_, moveState'), isLegalPlacement) when isLegalPlacement ->
                tryFindValidMove
                    (updateState state)
                    (updateMoveState moveState')
                    direction
                    resultProcessor
                    cancellationToken
            | _ -> None

        let folder acc piece = keepBestResult (handleLetter piece) acc state

        let tile = Map.find tileId state.pieces
        Set.fold folder None tile

    let folder acc tileId _ =
        keepBestResult (handleTileId tileId) acc state

    if not cancellationToken.IsCancellationRequested then
        MultiSet.fold folder None state.hand
    else
        None

let findMoveOnSquare (pos: coord) (state: gameState) resultProcessor cancellationToken =
    let directions =
        [ (-1, 0) // Left
          (0, -1) // Up
          (1, 0) // Right
          (0, 1) ] // Down

    let result =
        List.fold
            (fun res direction ->
                keepBestResult
                    (tryFindValidMove
                        state
                        (initMoveWithExistingWord pos state direction)
                        direction
                        resultProcessor
                        cancellationToken) 
                    res
                    state)
            None
            directions

    result

let findPlay (state: gameState) =
    let timeout =
        // We remove 50 ms from the timeout to make space for things that might
        // take time aside from finding the moves.
        match Option.map ((int) >> (fun t -> t - 50) >> Utils.max 0) state.timeout with
        | Some t when t >= 0 -> t
        | _ -> -1

    let mutable bestMove = None

    let resultProcessor =
        MailboxProcessor.Start (fun inbox ->
            let rec loop () =
                async {
                    let! recv = inbox.Receive()

                    match (bestMove, recv) with
                    | (None, Found m) -> bestMove <- Some m
                    | (Some bm, Found m) when m.score > bm.score -> bestMove <- Some m
                    | (_, Get ch) -> ch.Reply(bestMove)
                    | _ -> ()

                    return! loop ()
                }

            loop ())

    let cancellationTokenSource =
        match timeout with
        | t when t >= 0 -> new CancellationTokenSource(t)
        | _ -> new CancellationTokenSource()

    let runWithTimeout (timeout: int) computations =
        // Task.WaitAll blocks the thread until timeout is reached. A boolean is
        // returned indicating if all tasks completed or not - but we don't need
        // that - we just take the hitherto best result anyway.
        Task.WaitAll(computations, timeout)

    findAllPossibleSpawnPositions state
    |> Seq.map (fun pos ->
        Task.Factory.StartNew(fun () -> findMoveOnSquare pos state resultProcessor cancellationTokenSource.Token))
    |> Seq.cast<Task>
    |> Seq.toArray
    |> runWithTimeout timeout
    |> ignore

    resultProcessor.PostAndReply(Get)
    |> Option.map (fun result -> result.moves)
