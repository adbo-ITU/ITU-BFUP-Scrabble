module internal Eval

open StateMonad

let emptyState = mkState [] [] []

let add a b =
    a >>= fun av -> b >>= fun bv -> ret (av + bv)

let div a b =
    let safeDiv av =
        function
        | 0 -> fail DivisionByZero
        | bv -> ret (av / bv)

    a >>= fun av -> b >>= safeDiv av

type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp
    | Div of aExp * aExp
    | Mod of aExp * aExp
    | CharToInt of cExp

and cExp =
    | C of char (* Character value *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp
    | ToLower of cExp
    | IntToChar of aExp

type bExp =
    | TT (* true *)
    | FF (* false *)

    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)

    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)

    | IsVowel of cExp (* check for vowel *)
    | IsLetter of cExp (* check for letter *)
    | IsDigit of cExp (* check for digit *)

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)
let (./.) a b = Div(a, b)
let (.%.) a b = Mod(a, b)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.->.) b1 b2 =
    (~~b1) .||. b2 (* boolean implication *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b)
    .&&. (a .>=. b) (* numeric greater than *)

let binOp f a b =
    a >>= fun av -> b >>= fun bv -> ret (f av bv)

let modSM a b =
    let safeMod av =
        function
        | 0 -> fail DivisionByZero
        | bv -> ret (av % bv)

    a >>= fun av -> b >>= safeMod av

let rec arithEval e : SM<int> =
    match e with
    | N n -> ret n
    | V v -> lookup v
    | WL -> wordLength
    | PV a -> arithEval a >>= pointValue
    | Add (a, b) -> binOp (+) (arithEval a) (arithEval b)
    | Sub (a, b) -> binOp (-) (arithEval a) (arithEval b)
    | Mul (a, b) -> binOp (*) (arithEval a) (arithEval b)
    | Div (a, b) -> div (arithEval a) (arithEval b)
    | Mod (a, b) -> modSM (arithEval a) (arithEval b)
    | CharToInt c -> charEval c >>= (int >> ret)

and charEval e : SM<char> =
    match e with
    | C ch -> ret ch
    | CV a -> arithEval a >>= characterValue
    | ToUpper c -> charEval c >>= (System.Char.ToUpper >> ret)
    | ToLower c -> charEval c >>= (System.Char.ToLower >> ret)
    | IntToChar a -> arithEval a >>= (char >> ret)

let isVowel c =
    List.contains c [ 'A'; 'E'; 'I'; 'O'; 'U' ]

let rec boolEval e : SM<bool> =
    match e with
    | TT -> ret true
    | FF -> ret false
    | AEq (a1, a2) -> binOp (=) (arithEval a1) (arithEval a2)
    | ALt (a1, a2) -> binOp (<) (arithEval a1) (arithEval a2)
    | Not b -> boolEval b >>= (not >> ret)
    | Conj (b1, b2) -> binOp (&&) (boolEval b1) (boolEval b2)
    | IsDigit c -> charEval c >>= (System.Char.IsDigit >> ret)
    | IsLetter c -> charEval c >>= (System.Char.IsLetter >> ret)
    | IsVowel c -> charEval c >>= (isVowel >> ret)

type stm =
    (* statements *)
    | Declare of string (* variable declaration *)
    | Ass of string * aExp (* variable assignment *)
    | Skip (* nop *)
    | Seq of stm * stm (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm (* while statement *)

let rec stmntEval stm : SM<unit> =
    match stm with
    | Declare v -> declare v
    | Ass (v, a) -> arithEval a >>= update v
    | Skip -> ret ()
    | Seq (s1, s2) -> stmntEval s1 >>>= stmntEval s2
    | ITE (guard, s1, s2) ->
        boolEval guard
        >>= fun bv -> stmntEvalWithNewScope (if bv then s1 else s2)
    | While (guard, s) ->
        boolEval guard
        >>= function
            | true ->
                stmntEvalWithNewScope s
                >>>= stmntEval (While(guard, s))
            | false -> ret ()

and stmntEvalWithNewScope stm = push >>>= stmntEval stm >>>= pop

type StateBuilder() =

    member this.Bind(f, x) = f >>= x
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f) = f ()
    member this.Combine(a, b) = a >>= (fun _ -> b)

let prog = new StateBuilder()

let rec arithEval2 e =
    prog {
        match e with
        | N n -> return n
        | V v -> return! lookup v
        | WL -> return! wordLength
        | PV a ->
            let! av = arithEval2 a
            let! pv = pointValue av
            return pv
        | Add (a, b) -> return! binOp (+) (arithEval2 a) (arithEval2 b)
        | Sub (a, b) -> return! binOp (-) (arithEval2 a) (arithEval2 b)
        | Mul (a, b) -> return! binOp (*) (arithEval2 a) (arithEval2 b)
        | Div (a, b) -> return! div (arithEval2 a) (arithEval2 b)
        | Mod (a, b) -> return! modSM (arithEval2 a) (arithEval2 b)
        | CharToInt c ->
            let! cv = charEval2 c
            return int cv
    }

and charEval2 e =
    prog {
        match e with
        | C ch -> return ch
        | CV a ->
            let! av = arithEval2 a
            let! cv = characterValue av
            return cv
        | ToUpper c ->
            let! cv = charEval2 c
            return System.Char.ToUpper cv
        | ToLower c ->
            let! cv = charEval2 c
            return System.Char.ToLower cv
        | IntToChar a ->
            let! av = arithEval2 a
            return char av
    }

let rec boolEval2 e =
    prog {
        match e with
        | TT -> return true
        | FF -> return false
        | AEq (a1, a2) -> return! binOp (=) (arithEval2 a1) (arithEval2 a2)
        | ALt (a1, a2) -> return! binOp (<) (arithEval2 a1) (arithEval2 a2)
        | Not b ->
            let! bv = boolEval2 b
            return not bv
        | Conj (b1, b2) -> return! binOp (&&) (boolEval2 b1) (boolEval2 b2)
        | IsDigit c ->
            let! cv = charEval2 c
            return System.Char.IsDigit cv
        | IsLetter c ->
            let! cv = charEval2 c
            return System.Char.IsLetter cv
        | IsVowel c ->
            let! cv = charEval2 c
            return isVowel cv
    }

let rec stmntEval2 stm =
    prog {
        match stm with
        | Declare v -> do! declare v
        | Ass (v, a) ->
            let! av = arithEval2 a
            do! update v av
        | Skip -> return ()
        | Seq (s1, s2) ->
            do! stmntEval2 s1
            do! stmntEval2 s2
        | ITE (guard, s1, s2) ->
            let! bv = boolEval2 guard
            do! push
            do! stmntEval2 (if bv then s1 else s2)
            do! pop
        | While (guard, s) ->
            let! bv = boolEval2 guard

            if bv then
                do! push
                do! stmntEval2 s
                do! pop
                do! stmntEval2 (While(guard, s))
            else
                return ()
    }

type word = (char * int) list
type squareFun = word -> int -> int -> Result<int, Error>

let stmntToSquareFun stm w pos acc =
    let initialState =
        mkState
            [ ("_pos_", pos)
              ("_acc_", acc)
              ("_result_", 0) ]
            w
            [ "_pos_"; "_acc_"; "_result_" ]

    stmntEval stm >>>= lookup "_result_"
    |> evalSM initialState

type coord = int * int

type boardFun = coord -> Result<squareFun option, Error>

let stmntToBoardFun stm m (x, y) =
    let initialState =
        mkState
            [ ("_x_", x)
              ("_y_", y)
              ("_result_", 0) ]
            []
            [ "_x_"; "_y_"; "_result_" ]

    stmntEval stm >>>= lookup "_result_"
    >>= fun sfId -> Map.tryFind sfId m |> ret
    |> evalSM initialState

type board =
    { center: coord
      defaultSquare: squareFun
      squares: boardFun }

let mkBoard c defaultSq boardStmnt ids =
    let m =
        List.map (fun (k, sq) -> (k, stmntToSquareFun sq)) ids
        |> Map.ofList

    let b = stmntToBoardFun boardStmnt m

    let d = stmntToSquareFun defaultSq

    { center = c
      defaultSquare = d
      squares = b }
