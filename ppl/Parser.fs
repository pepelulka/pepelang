module Parser

open FParsec
open AST

type UserState = unit
type Parser<'t> = Parser<'t, UserState> 

// For debug:
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let private wrap2 x y =
    (x, y)

let private wrap3 x y z =
    (x, y, z)

let private merge env1 env2 =
    Map.fold (fun m x y -> Map.add x y m) env1 env2

// =============================================================================
// util parsers
// =============================================================================

// TODO: add comments
let private ws : Parser<_> = spaces <|> skipNewline <|> skipChar '\t' <|> eof

let private identifier : Parser<_> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

let private stringLiteral : Parser<_> =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedChar)

// =============================================================================
// ppl parsers
// =============================================================================

let private exprParserVal, exprParserRef = createParserForwardedToRef<Expression, unit>()

let private parser_Int : Parser<_> = pint32 .>> ws |>> Int
let private parser_String : Parser<_> = stringLiteral .>> ws |>> String
let private parser_Unit : Parser<_> = pstring "unit" >>. ws |>> Unit
let private parser_Bool : Parser<_> = ((pstring "true" >>. ws) >>% Bool(true)) <|> ((pstring "false" >>. ws) >>% Bool(false))
let private parser_Var : Parser<_> = identifier .>> ws |>> Var

let private parser_unaryBuiltinOp : Parser<_> = List.reduce (<|>) (List.map (fun x -> pstring x) Utils.unaryOperators)
let private parser_binaryBuiltinOp : Parser<_> = List.reduce (<|>) (List.map (fun x -> pstring x) Utils.binaryOperators)

let private parser_UnaryBuiltinOpConstruction : Parser<_> = pipe2 (parser_unaryBuiltinOp .>> ws) (exprParserVal .>> ws) wrap2 |>> BuiltinUnaryOperator
let private parser_BinaryBuintinOpConstruction : Parser<_> = pipe3 (parser_binaryBuiltinOp .>> ws) (exprParserVal .>> ws) (exprParserVal .>> ws) wrap3 |>> BuiltinBinaryOperator
let private parser_If : Parser<_> = pipe3 (pstring "if" >>. ws >>. exprParserVal .>> ws) (pstring "then" >>. ws >>. exprParserVal .>> ws) (pstring "else" >>. ws >>. exprParserVal .>> ws) wrap3 |>> If
let private parser_DoAndReturn : Parser<_> = pipe2 (pstring "do" >>. ws >>. (sepBy exprParserVal (between ws ws (pstring "$"))) .>> ws) (pstring "return" >>. ws >>. exprParserVal .>> ws) wrap2 |>> DoAndReturn
let private parser_Let : Parser<_> = pipe2 (pstring "let" >>. ws >>. identifier .>> ws .>> pstring "=" .>> ws) (exprParserVal .>> ws) wrap2 |>> Let
let private parser_Lambda = pipe2 (identifier .>> ws .>> pstring "->" .>> ws) (exprParserVal .>> ws) wrap2 |>> Lambda
let private parser_Apply = between (pstring "<" .>> ws) (ws >>. pstring ">" .>> ws) (pipe2 (exprParserVal .>> ws) (between (pstring "(") (pstring ")") (sepBy1 exprParserVal (ws >>. pstring "," .>> ws))) wrap2) |>> Apply

do exprParserRef := choice [
    parser_Int
    parser_String
    attempt parser_Apply
    parser_UnaryBuiltinOpConstruction
    parser_BinaryBuintinOpConstruction
    attempt parser_Lambda
    attempt parser_If
    attempt parser_Unit
    attempt parser_Bool
    attempt parser_Let
    attempt parser_DoAndReturn
    parser_Var
]

let exprListParser = ws >>. many (exprParserVal .>> ws .>> pstring ";" .>> ws)
