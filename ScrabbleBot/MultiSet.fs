module internal MultiSet

// F#'s Map uses a binary search tree internally, so operations are ordered.
type MultiSet<'T when 'T: comparison> = MS of Map<'T, uint32>

let empty = MS(Map.empty)

let isEmpty (MS (m)) = Map.isEmpty m

let size (MS (m)) =
    Map.fold (fun sum _ count -> sum + count) 0u m

let contains a (MS (m)) = Map.containsKey a m

let numItems a (MS (m)) =
    m |> Map.tryFind a |> Option.defaultValue 0u

let add a n s =
    match s with
    | MS (m) -> MS(Map.add a (numItems a s + n) m)

let addSingle a = add a 1u

let remove a n s =
    match s with
    | MS (m) when n > numItems a s -> MS(Map.remove a m)
    | MS (m) -> MS(Map.add a (numItems a s - n) m)

let removeSingle a = remove a 1u

let fold f acc (MS (m)) = Map.fold f acc m

let foldBack f (MS (m)) acc = Map.foldBack f m acc

// I get a compile error about IComparable when using partial application
let ofList lst =
    List.fold (fun s v -> addSingle v s) empty lst

// Utility functions for myself because i get a compile error when using
// Map.keys and Map.values (and m.Keys and m.Values)
let keys (MS (m)) = m |> Map.toList |> List.map fst
let mapValues m = m |> Map.toList |> List.map snd

// We need type signatures here avoid a confused compiler because of Map.map
let toList (MS (m): MultiSet<'a>) : 'a list =
    Map.map (fun k v -> List.replicate (int v) k) m
    |> mapValues
    |> List.fold (@) []

let map f = toList >> List.map f >> ofList

let union a (MS (s2)) =
    let (MS (s1)) = a
    MS(Map.fold (fun am k bv -> Map.add k (max bv (numItems k a)) am) s1 s2)

let sum a (MS (s2)) =
    let (MS (s1)) = a
    MS(Map.fold (fun am k bv -> Map.add k (bv + (numItems k a)) am) s1 s2)

let subtract a (MS (s2)) =
    let (MS (s1)) = a

    Map.fold (fun am k bv -> Map.add k ((numItems k a) - bv) am) s1 s2
    |> Map.filter (fun _ v -> v > 0u)
    |> MS

let intersection a b =
    keys a
    |> List.filter (fun k -> (contains k a) && (contains k b))
    |> List.fold (fun acc k -> add k (min (numItems k a) (numItems k b)) acc) empty
