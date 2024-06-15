module Utils

open FParsec

let valueParser, valueParserRef = createParserForwardedToRef<AST.Expression, unit>()

let private ws = spaces
let private pInt = pint32 .>> ws |>> AST.Int
let private pFloat  =
    pipe2 (manySatisfy (System.Char.IsDigit) .>> pstring ".") (manySatisfy (System.Char.IsDigit) .>> spaces) (
        fun x y -> AST.Float(float (x + "." + y))
    )

let private stringLiteral : Parser<string, unit> =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedChar)

let private pString = stringLiteral .>> ws |>> AST.String
let private pTuple = between (pstring "(" .>> ws) (pstring ")" .>> ws) (sepBy1 (valueParser .>> ws) (pstring "," .>> ws)) |>> AST.Tuple

do valueParserRef := ws >>. choice [
        attempt pFloat
        pInt
        pString
        pTuple
    ]

let parseValue x =
    match (run valueParser x) with
      | Failure(_) -> AST.None()
      | Success(x, _, _) -> x

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
