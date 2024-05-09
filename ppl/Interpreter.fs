module Interpreter

open AST

let mergeMaps m1 m2 =
    Map.fold (fun acc k v -> Map.add k v acc) m2 m1

// 
let extractErrorOrEvalLazy (listOfAst:Expression list) (lazyExp:Lazy<Expression>) : Expression =
    let isError = (function x -> match x with | Error(_) -> true | _ -> false) 
    let errors = List.filter isError listOfAst
    if (List.length errors) = 0 then lazyExp.Force()
    else List.head errors

let astToString ast =
    match ast with 
    | Int(_) -> "int"
    | Bool(_) -> "bool"
    | String(_) -> "string"
    | Unit() -> "unit"
    | Var(_) -> "var"
    | Lambda(_) -> "lambda"
    | PartiallyAppliedLambda(_) -> "lambda*"
    | If(_) -> "if"
    | BuiltinBinaryOperator(_) -> "operator/2"
    | BuiltinUnaryOperator(_) -> "operator/1"
    | _ -> "undefined"

let rec Evaluate expr env =
    match expr with
    | Int(x) -> Int(x)
    | String(x) -> String(x)
    | Bool(x) -> Bool(x)
    | Error(str) -> Error(str)
    | Unit() -> Unit()
    | Var(id) -> Map.find id env
    | BuiltinBinaryOperator(op, x', y') -> 
    let x = (Evaluate x' env)
    let y = (Evaluate y' env)
    extractErrorOrEvalLazy [x; y] (lazy (BinOp op x y))
    | BuiltinUnaryOperator(op, x') -> let x = (Evaluate x' env) in extractErrorOrEvalLazy [x] (lazy(UnOp op x))
    | If(cond', e1', e2') -> let cond = (Evaluate cond' env) in let e1 = (Evaluate e1' env) in let e2 = (Evaluate e2' env) in extractErrorOrEvalLazy [cond; e1; e2] (lazy(
        match cond with
        | Bool(true) -> Evaluate e1 env
        | Bool(false) -> Evaluate e2 env
        | _ -> Error(sprintf "If error: expected bool, got %s" (astToString cond))
    ))
    | Let(id, exp') -> let exp = (Evaluate exp' env) in extractErrorOrEvalLazy [exp] (lazy(
        Environment(Map.add id exp Map.empty)
    ))
and BinOp op x y =
    match (op, x, y) with
    // Ints:
    | ("+", Int(a), Int(b)) -> Int(a + b)
    | ("-", Int(a), Int(b)) -> Int(a - b)
    | ("*", Int(a), Int(b)) -> Int(a * b)
    | ("/", Int(a), Int(b)) -> Int(a / b)
    | ("<", Int(a), Int(b)) -> if a < b then Bool(true) else Bool(false)
    | (">", Int(a), Int(b)) -> if a > b then Bool(true) else Bool(false)
    | ("=", Int(a), Int(b)) -> if a = b then Bool(true) else Bool(false)
    | ("<=", Int(a), Int(b)) -> if a <= b then Bool(true) else Bool(false)
    | (">=", Int(a), Int(b)) -> if a >= b then Bool(true) else Bool(false)
    | ("!=", Int(a), Int(b)) -> if a <> b then Bool(true) else Bool(false)
    // Strings:
    | ("+", String(a), String(b)) -> String(a + b)
    | ("<", String(a), String(b)) -> if a < b then Bool(true) else Bool(false)
    | (">", String(a), String(b)) -> if a > b then Bool(true) else Bool(false)
    | ("=", String(a), String(b)) -> if a = b then Bool(true) else Bool(false)
    | ("<=", String(a), String(b)) -> if a <= b then Bool(true) else Bool(false)
    | (">=", String(a), String(b)) -> if a >= b then Bool(true) else Bool(false)
    | ("!=", String(a), String(b)) -> if a <> b then Bool(true) else Bool(false)
    // Bool
    | ("&&", Bool(a), Bool(b)) -> Bool(a && b)
    | ("||", Bool(a), Bool(b)) -> Bool(a || b)
    | _ -> Error(sprintf "Operator error: Can't apply operator %s to %s and %s" op (astToString x) (astToString y))
and UnOp op x =
    match (op, x) with
    | ("~", Int(a)) -> Int(-a)
    | ("~", Bool(a)) -> Bool(not a)
    | _ -> Error(sprintf "Operator error: Can't apply operator %s to %s" op (astToString x))
and EvaluateList l env =
    let folder e exp = 
        match (Evaluate exp e) with
        | Environment(e') -> mergeMaps e e'
        | _ -> e
    List.fold folder env l
