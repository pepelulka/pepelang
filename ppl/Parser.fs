module Parser

open AST
open FParsec
open Utils

type UserState = unit
type Parser<'t> = Parser<'t, UserState> 

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parser, parserRef = createParserForwardedToRef<Expression, unit>()

let private ws = spaces <|> eof

let private identifier : Parser<_> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '.'
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

let private oper : Parser<_> =
   let isOperatorChar = isAnyOf (['+'; '-'; '='; '*'; '&'; '|'; '!'; '>'; '<'; '/'; '~']) in
   many1Satisfy isOperatorChar

let private pInt : Parser<_> = pint32 .>> ws |>> Int
let private pFloat : Parser<_> =
    pipe2 (manySatisfy (System.Char.IsDigit) .>> pstring ".") (manySatisfy (System.Char.IsDigit) .>> spaces) (
        fun x y -> Float(float (x + "." + y))
    )
let private pString : Parser<_> = stringLiteral .>> ws |>> String
let private pVar : Parser<_> = identifier .>> ws |>> Var
let private pLet : Parser<_> = 
    pipe3 (pstring "let" >>. ws >>. identifier .>> ws) (many (identifier .>> ws)) (pstring "=" >>. ws >>. parser) pack3 |>> Let
let private pLetIn : Parser<_> =
    pipe2 (pLet .>> ws .>> pstring "in" .>> ws) (parser .>> ws) pack2 |>> LetIn
let private pBool : Parser<_> = ((pstring "true" .>> ws) >>% Bool(true)) <|> 
                                ((pstring "false" .>> ws) >>% Bool(false))
let private pNone : Parser<_> = (pstring "None" .>> ws) >>% None()
let private pApply : Parser<_> = between (pstring "{" >>. ws) (pstring "}" >>. ws)
                                    (
                                        pipe2 (parser .>> ws) (many (parser .>> ws) ) pack2 |>> Apply
                                    )
let private pOperator : Parser<_> = oper .>> ws |>> Expression.Operator
let private pLetOperator : Parser<_> = 
    pipe3 (pstring "let" >>. ws >>. oper .>> ws) (many (identifier .>> ws)) (pstring "=" >>. ws >>. parser) pack3 |>> Let 
let private pIf : Parser<_> = pipe3
                                    (pstring "if" >>. ws >>. parser .>> ws) 
                                    (pstring "then" >>. ws >>. parser .>> ws) 
                                    (pstring "else" >>. ws >>. parser .>> ws) pack3 |>> If
let private pLambda : Parser<_> = pipe2
                                    (pstring "\\" >>. identifier .>> ws .>> pstring "->" .>> ws)
                                    (parser .>> ws) pack2 |>> Lambda
let private pLiteral : Parser<_> = (pstring "%" >>. identifier .>> ws) |>> Literal
let private pImport : Parser<_> = (pstring "import" >>. ws >>. identifier .>> ws) |>> Import
let private pTuple : Parser<_> = between (pstring "(" >>. ws) (pstring ")" >>. ws) (sepBy (parser .>> ws) (pstring "," .>> ws)) |>> Tuple
let private pLazy : Parser<_> = (pstring "Lazy" >>. ws >>. parser .>> ws) |>> Lazy
let private pForce : Parser<_> = (pstring "force" >>. ws >>. parser .>> ws) |>> Force
let private pFail : Parser<_> = (pstring "fail" >>. ws >>. parser .>> ws) |>> Fail
let private pDo : Parser<_> = (pstring "do" >>. ws >>.
                                  between (pstring "[" .>> ws) (pstring "]" .>> ws) (many1 (parser .>> ws .>> pstring ";" .>> ws))   
                              ) |>> Do

// TypeConstraints:
let pConstraint, pConstraintRef = createParserForwardedToRef<Expression, unit>()
let private pBaseType : Parser<_> = List.reduce (fun x y -> x <|> (attempt y) ) (List.map (fun x -> pstring x .>> ws ) BASE_TYPE_NAMES) |>> BaseType 
let private pOrConstraint : Parser<_> = pstring "choice" >>. ws >>. (sepBy1 (pConstraint .>> ws) (pstring "|" >>. ws) ) |>> ConstraintOr
let private pTupleConstraint : Parser<_> = between (pstring "(" .>> ws) (pstring ")" .>> ws) (sepBy (pConstraint .>> ws) (pstring "," .>> ws) ) |>> ConstraintTuple
let private pAnyTypeConstraint : Parser<_> = (pstring "_" .>> ws) >>% ConstraintAnyType

// Pattern matching:
let pMatchCandidate, pMatchCandidateRef = createParserForwardedToRef<Expression, unit>()
let private pCandidateWildCard : Parser<_> = (pstring "_" .>> ws) >>% MatchCandidateWildCard
let private pCandidateAny : Parser<_> = (identifier .>> ws) |>> (fun x -> MatchCandidateSingleConstraint(x, ConstraintAnyType))
let private pCandidateSingleCandidate : Parser<_> = pipe2 (identifier .>> ws .>> pstring "of" .>> ws) (pConstraint .>> ws) pack2 |>> MatchCandidateSingleConstraint
let private pCandidateTuple : Parser<_> = between (pstring "(" .>> ws) (pstring ")" .>> ws) (sepBy (pMatchCandidate .>> ws) (pstring "," .>> ws) ) |>> MatchCandidateTuple

do pMatchCandidateRef := choice [
    pCandidateTuple
    attempt pCandidateTuple
    attempt pCandidateSingleCandidate
    attempt pCandidateWildCard
    pCandidateAny
]

let pMatchEntry = pipe2 (pMatchCandidate .>> ws .>> pstring "->" .>> ws) (parser .>> ws) pack2 |>> MatchEntry
let pMatch = pipe2 (pstring "match" >>. ws >>. parser .>> ws .>> pstring "with" .>> ws) (many1 (pstring "|" >>. ws >>. pMatchEntry .>> ws)) pack2 |>> Match

do pConstraintRef := choice [
    attempt pOrConstraint
    attempt pAnyTypeConstraint
    pTupleConstraint
    pBaseType
    pVar
]

let pDefineConstraint : Parser<_> = pipe2 (pstring "type" >>. ws >>. identifier .>> ws .>> pstring "=" .>> ws) (pConstraint .>> ws) pack2 |>> DefineConstraint 

do parserRef := ws >>. choice [
    attempt pFloat
    pInt
    pString
    pApply
    pOperator
    pLambda
    pLiteral
    pTuple
    attempt pDo
    attempt pLetIn
    attempt pLetOperator
    attempt pLet
    attempt pBool
    attempt pIf
    attempt pMatch
    attempt pBaseType
    attempt pDefineConstraint
    attempt pLazy
    attempt pForce
    attempt pImport
    attempt pNone
    attempt pFail
    pVar
]

let FinalParser = many (parser .>> ws .>> pstring ";" .>> ws)