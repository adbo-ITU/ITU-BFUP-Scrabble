module internal StateMonad

type Error =
    | VarExists of string
    | VarNotFound of string
    | IndexOutOfBounds of int
    | DivisionByZero
    | ReservedName of string

type Result<'a, 'b> =
    | Success of 'a
    | Failure of 'b

type State =
    { vars: Map<string, int> list
      word: (char * int) list
      reserved: Set<string> }

type SM<'a> = S of (State -> Result<'a * State, Error>)

let mkState lst word reserved =
    { vars = [ Map.ofList lst ]
      word = word
      reserved = Set.ofList reserved }

let evalSM (s: State) (S a: SM<'a>) : Result<'a, Error> =
    match a s with
    | Success (result, _) -> Success result
    | Failure error -> Failure error

let bind (f: 'a -> SM<'b>) (S a: SM<'a>) : SM<'b> =
    S (fun s ->
        match a s with
        | Success (b, s') ->
            match f b with
            | S g -> g s'
        | Failure err -> Failure err)


let ret (v: 'a) : SM<'a> = S(fun s -> Success(v, s))
let fail err : SM<'a> = S(fun s -> Failure err)

let (>>=) x f = bind f x
let (>>>=) x f = x >>= (fun () -> f)

let push: SM<unit> =
    S(fun s -> Success((), { s with vars = Map.empty :: s.vars }))

let pop: SM<unit> =
    S(fun s -> Success((), { s with vars = List.tail s.vars }))

let wordLength: SM<int> = S(fun s -> Success(s.word.Length, s))

let wordPos (pos: int) : SM<(char * int)> =
    let getPos s =
        match pos with
        | i when i >= 0 && i < s.word.Length -> Success(s.word.[i], s)
        | i -> Failure(IndexOutOfBounds i)

    S(getPos)

let characterValue (pos: int) : SM<char> =
    let getChar wp = ret (wp |> fst)
    wordPos pos >>= getChar

let pointValue (pos: int) : SM<int> =
    let getPoints wp = ret (wp |> snd)
    wordPos pos >>= getPoints

let lookup (x: string) : SM<int> =
    let rec aux =
        function
        | [] -> None
        | m :: ms ->
            match Map.tryFind x m with
            | Some v -> Some v
            | None -> aux ms

    S (fun s ->
        match aux (s.vars) with
        | Some v -> Success(v, s)
        | None -> Failure(VarNotFound x))

let update (var: string) (value: int) : SM<unit> =
    let rec aux =
        function
        | [] -> None
        | m :: ms ->
            match Map.tryFind var m with
            | Some _ -> Some(Map.add var value m :: ms)
            | None -> aux ms |> Option.map (fun ms' -> m :: ms')

    S (fun s ->
        match aux s.vars with
        | Some vs -> Success((), { s with vars = vs })
        | None -> Failure(VarNotFound var))

let declare (var: string) : SM<unit> =
    let checkReserved =
        S (fun s ->
            match Set.contains var s.reserved with
            | false -> Success((), s)
            | true -> Failure(ReservedName var))

    let checkExists =
        S (fun s ->
            match Map.tryFind var (List.head s.vars) with
            | None -> Success((), s)
            | Some _ -> Failure(VarExists var))

    let initVar =
        S (fun s ->
            match s.vars with
            | m :: ms -> Success((), { s with vars = Map.add var 0 m :: ms })
            | [] -> failwith "panic because this state should never be reachable")

    checkReserved >>>= checkExists >>>= initVar
