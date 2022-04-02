module internal CsMap

type CsMap<'a, 'b when 'a: equality> = System.Collections.Generic.Dictionary<'a, 'b>

val empty : unit -> CsMap<'a, 'b>
val add : 'a -> 'b -> CsMap<'a, 'b> -> CsMap<'a, 'b>
val tryFind : 'a -> CsMap<'a, 'b> -> 'b option
