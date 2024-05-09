open Parser
open FParsec

// Add pairs...
let str = "

let a = 5;
let a = do
    let b = 5 $
    + 1 2 $
    * 8 9 
    return 3;
let c = x -> y -> + x y;
"

let str1 = "
let a = + 3 5;
"

type Monad<'T> =
    | Result of 'T
    | ErrorLog of string

let bind f (x:Monad<'T>) =
    match x with
    | Result(x') -> f x'
    | ErrorLog(str) -> ErrorLog(str)

let (>=>) f g = f >> (bind g)  

let parse s : Monad<AST.Expression list> =
    match (run exprListParser s) with
    | Success(r, _, _) -> Monad.Result(r)
    | Failure(msg, _, _) -> Monad.ErrorLog(msg)

let execute l =
    Monad<AST.Env>.Result(Interpreter.EvaluateList l Map.empty)

let print m =
    match m with
    | Monad.Result(x) -> printfn "Success: %A" x
    | Monad.ErrorLog(msg) -> printfn "Error: %s" msg

(parse >=> execute) str1 |> print

// Apply construction
