namespace TheGrafted

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern =
            @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun t ->
            match t.Value with
            | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
            | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state =
        { board: Parser.board
          dict: ScrabbleUtil.Dictionary.Dict
          ourPlayerNumber: uint32
          players: uint32 list
          hand: MultiSet.MultiSet<uint32>
          placedTiles: Map<coord, uint32 * (char * int)> }

    let mkState b d pn h currentPlayer numPlayers =
        let players =
            Utils.rotate (uint32 currentPlayer - 1u) [ 1u .. numPlayers ]

        { board = b
          dict = d
          players = players
          ourPlayerNumber = pn
          hand = h
          placedTiles = Map.empty }

    let board st = st.board
    let dict st = st.dict
    let ourPlayerNumber st = st.ourPlayerNumber
    let hand st = st.hand
    let numberOfPlayers st = List.length st.players
    let currentPlayer st = List.head st.players
    let nextPlayersList st = Utils.rotate 1u st.players
    // forefeitPlayer also advances the turn to the next player
    let forfeitPlayer st pid =
        List.filter (fun player -> player <> pid) (nextPlayersList st)

    let toBotGameState st pieces : ScrabbleBot.gameState =
        { board = st.board
          dict = st.dict
          hand = st.hand
          pieces = pieces
          placedTiles = st.placedTiles }

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st: State.state) =

        let rec aux (st: State.state) =
            let testHand =
                MultiSet.ofList [ 14u
                                  5u
                                  19u
                                  19u
                                  9u
                                  14u
                                  7u ]

            let testPlacedTiles =
                Map.ofList [ ((0, 0), (6u, ('F', 4)))
                             ((1, 0), (9u, ('I', 1)))
                             ((2, 0), (20u, ('T', 1)))
                             ((1, -1), (20u, ('T', 1)))
                             ((1, 1), (11u, ('K', 1)))
                             ((1, 2), (9u, ('I', 1)))
                             ((4, -1), (4u, ('S', 1)))
                             ((4, 0), (4u, ('E', 1)))
                             ((4, 1), (1u, ('A', 1)))]

            let botGameState =
                { State.toBotGameState st pieces with
                    hand = testHand
                    placedTiles = testPlacedTiles }

            // debugPrint (sprintf "STATE: %A\nRESULT: %A\n" botGameState (ScrabbleBot.findWord botGameState 'A'))
            debugPrint "\n"
            ScrabbleBot.findPlay botGameState
            debugPrint "\n"

            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint
                "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

            let input = System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.ourPlayerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.ourPlayerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            // A successful play was made by us
            | RCM (CMPlaySuccess (moves, points, newTiles)) ->
                // Remove old tiles
                let handWithRemoved =
                    List.fold (fun hand' (_, (tileId, _)) -> MultiSet.removeSingle tileId hand') st.hand moves

                // Add tiles placed
                let newHand =
                    List.fold
                        (fun hand' (tileId, numTiles) -> MultiSet.add tileId numTiles hand')
                        handWithRemoved
                        newTiles

                // Add squares placed
                let newTiles =
                    List.fold (fun tiles' (coord, tile) -> Map.add coord tile tiles') st.placedTiles moves

                let st' =
                    { st with
                        hand = newHand
                        players = State.nextPlayersList st
                        placedTiles = newTiles }

                aux st'

            // Successful play by other player.
            | RCM (CMPlayed (pid, moves, points)) ->
                // Add tiles placed
                let newTiles =
                    List.fold (fun tiles' (coord, tile) -> Map.add coord tile tiles') st.placedTiles moves

                let st' =
                    { st with
                        players = State.nextPlayersList st
                        placedTiles = newTiles }

                aux st'

            // Assuming that (uint32 * uint32) list, is a list of (removed, received) tiles.
            // Succesful change of tiles by us
            | RCM (CMChangeSuccess changedTiles) ->
                // Remove old tiles, and add new tiles
                let newHand =
                    List.fold
                        (fun hand' (removed, received) ->
                            MultiSet.removeSingle removed hand'
                            |> MultiSet.addSingle received)
                        st.hand
                        changedTiles

                let st' =
                    { st with
                        players = State.nextPlayersList st
                        hand = newHand }

                aux st'

            // RCM (CMTimeOut _) is not defined
            | RCM (CMPlayFailed _)
            | RCM (CMPassed _)
            | RCM (CMChange _) -> aux { st with players = State.nextPlayersList st }

            | RCM (CMForfeit pid) -> aux { st with players = State.forfeitPlayer st pid }

            | RCM (CMGameOver _) -> debugPrint "Game over"

            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st

        try
            aux st
        with
        | ex ->
            debugPrint (sprintf "Error: %A" ex)
            raise ex

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerNumber: uint32)
        (playerTurn: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                playerTurn
                hand
                timeout
        )

        let dict = dictf true
        let board = Parser.mkBoard boardP

        let handSet =
            List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playerTurn numPlayers)
