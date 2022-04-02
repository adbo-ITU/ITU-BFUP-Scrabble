module internal CsMap

type CsMap<'a, 'b when 'a: equality> = System.Collections.Generic.Dictionary<'a, 'b>

let empty () =
    System.Collections.Generic.Dictionary<'a, 'b>()

let add k v (m: CsMap<'a, 'b>) =
    m.[k] <- v
    m

let tryFind k (m: CsMap<'a, 'b>) =
    match m.TryGetValue(k) with
    | (true, v) -> Some(v)
    | (false, _) -> None
