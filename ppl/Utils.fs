module Utils

let mergeMaps m1 m2 =
    Map.fold (fun acc k v -> Map.add k v acc) m2 m1

let rec isInList key lst =
    match lst with
    | [] -> false
    | h::_ when key = h -> true
    | _::t -> isInList key t

type StopValue<'t> = 't * bool

let FoldTillStop (value:'t) (lst:'u list) (f:'t -> 'u -> StopValue<'t>) : 't option =
    let st = StopValue(value, false)
    let folder x y =
        match x with
            | (_, true) as sv -> sv
            | (v, false) -> f v y
    match (List.fold folder st lst) with
    | (v, true) -> Some(v)
    | _ -> None

let pack2 a b = (a, b)
let pack3 a b c = (a, b, c)
let pack4 a b c d = (a, b, c, d)
let pack5 a b c d e = (a, b, c, d, e)
