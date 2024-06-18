module Interpreter

open System
open System.IO

open AST
open Utils 
open FParsec

// Double environment ============================================================ Functions for double environment:
// first - dynamic environment, second - static
type DoubleEnvironment = Environment*Environment

let private IsIn key (de:DoubleEnvironment) : bool =
    match de with
    | (e1, e2) -> try Map.find key e1 |> ignore; true
                    with _ -> try Map.find key e2 |> ignore; true with _ -> false

let private Get key (de:DoubleEnvironment) : Expression =
    if (not (IsIn key de)) then failwith (sprintf "%s is not in environment" key)
    match de with
    | (e1, e2) -> try Map.find key e1 with _ -> try Map.find key e2 with _ -> failwith "Impossible"

let private Set key value (de:DoubleEnvironment) : DoubleEnvironment =
    match de with
    | (e1, e2) -> (Map.add key value e1, e2)

let private Dynamic (de:DoubleEnvironment) : Environment =
    match de with
    | (e1, _) -> e1

let private Static (de:DoubleEnvironment) : Environment =
    match de with
    | (_, e2) -> e2

let private Merge env (de:DoubleEnvironment) =
    match de with
    | (e1, e2) -> (mergeMaps env e1, e2)

let private Import (mdl:Module) (de:DoubleEnvironment) =
    match de with
    | (e1, e2) -> (e1, mergeMaps mdl e2)

let private ImportList (mdlLst:Module list) (de:DoubleEnvironment) =
    List.fold (fun x y -> Import y x) de mdlLst

// Double environment ============================================================

// Evaluate <expression-to-eval> <dynamic-environment> <global-static-environment>
let rec Evaluate expr env =
    match expr with
    | Int(_) as n -> n
    | Float(_) as n -> n
    | String(_) as s -> s
    | InternalFunction(_) as f -> f
    | Bool(_) as b -> b
    | None() -> None()
    | Var(name) -> try Get name env with _ -> failwith (sprintf "Undefined name: %s" name)
    | Operator(name) -> try Get name env with _ -> failwith (sprintf "Undefined operator: %s" name)
    | Lazy(e) -> LazyWithEnv(e, Dynamic env)
    | Literal(_) as l -> l
    
    | Type(_) as t -> t
    | BaseType(_) as t -> t
    | ConstraintOr(lst) -> ConstraintOr(evalList lst env)
    | ConstraintTuple(lst) -> ConstraintTuple(evalList lst env)
    | ConstraintAnyType as c -> c

    | Tuple(lst) -> Tuple(evalList lst env)

    | Force(e) -> let tmp = Evaluate e env in match tmp with 
                                                | Lazy(e) -> Evaluate e env
                                                | LazyWithEnv(e, en) -> Evaluate e (DoubleEnvironment(en, Static env))
                                                | _ as s -> failwith (sprintf "Trying to force %A which is not a lazy object" s)
    | If(cond, tr, fa) ->
        match (Evaluate cond env) with
        | Bool(true) -> Evaluate tr env
        | Bool(false) -> Evaluate fa env
        | _ -> failwith (sprintf "Condition is not bool")
    | DefineConstraint(name, c) -> DefineConstraint(name, Evaluate c env)
    | Let(name, lst, exp) ->
        match lst with
        | [] -> LetResult(name, Evaluate exp env)
        | h::t -> let lambdas = List.foldBack (fun newVar prevExpr -> Lambda(newVar, prevExpr)) t exp
                  in LetResult (
                        name,
                        Closure( // We create closure here and we must add recursive reference to itself to allow recursion
                            h,
                            lambdas,
                            Map.add name (Lambda(h, lambdas)) (Dynamic env) ) // Here are we creating recursive reference by adding its name
                        )
    | LetIn(l, exp) ->
        let el = Evaluate l env in
        match el with
        | LetResult(x, y) -> Evaluate exp (Set x y env)
        | _ -> failwith "Impossible"
    | Do(lst) ->
        List.fold (fun x y -> Evaluate y env) (None()) lst
    | Apply(exp, lst) -> apply (Evaluate exp env) (evalList lst env) env
    | Lambda(name, exp) -> Closure(name, exp, (Dynamic env))
    | Closure(_) as c -> c
    | Fail(msg) -> let evMsg = Evaluate msg env in match evMsg with | String(x) -> failwith x | any -> failwith (sprintf "%A" any)
    | Import(_) as imp -> imp
    
    // Pattern matching:
    | Match(exp, entr) ->
        let entries = evalList entr env in
        let evExp = Evaluate exp env in
            let sv = StopValue(evExp, false)
            let folder x y =
                match x with
                | (_, true) as k -> k
                | (v, false) -> 
                let chkd = checkEntry v y env 
                match chkd with
                  | Some(x') -> (x', true)
                  | _ -> x in
            match (List.fold folder (evExp, false) entries) with
                | (v, true) -> v
                | (v, false) -> failwith "Pattern matching failure..."
    | MatchEntry(mc, ex) -> MatchEntry(Evaluate mc env, ex)
    | MatchCandidateWildCard as mc -> mc
    | MatchCandidateTuple(lst) -> MatchCandidateTuple(evalList lst env)
    | MatchCandidateSingleConstraint(name, constr) -> MatchCandidateSingleConstraint(name, Evaluate constr env)

    | _ as und -> failwith (sprintf "Don't know how to handle %A" und)
and evalList lst env = List.map (fun x -> Evaluate x env) lst

and applyOne expToApply arg env =
    match expToApply with
    | Closure(name, exp, clEnv) -> let finalEnv = Set name arg (Merge clEnv env) in
                                    Evaluate exp finalEnv
    | Lambda(name, exp) -> Evaluate exp (Set name arg env)
    | _ -> failwith (sprintf "You can't apply %A to %A" exp arg)

and apply exp lst env =
    let isInternal x =
        match x with
        | InternalFunction(_) -> true
        | _ -> false
    if (List.length lst) = 0 && not (isInternal exp) then exp else
    match exp with
    | InternalFunction(f) -> f lst
    | Closure(_) as c -> 
                match lst with
                | [] -> failwith "Impossible"
                | h::t -> apply (applyOne c h env) t env
    | Lambda(_) as l ->
                match lst with
                | [] -> failwith "Impossible"
                | h::t -> apply (applyOne l h env) t env
    | _ -> failwith (sprintf "You can't apply %A to %A" exp lst) 
and checkEntry exp entry env : Expression option = // Checks if entry is satisfied and evaluate expression and return (result, true) or (_, false) if not satisfied
    let rec CheckMatchCandidate (state:Map<string, Expression> option) v mc =
        match state with
        | Some(st) ->  
            match mc with
              | MatchCandidateWildCard -> Some(st)
              | MatchCandidateSingleConstraint(name, e) ->
                    if (TypeSatisfyConstraint (TypeOf v) e) then (Some(Map.add name v st))
                    else Option.None
              | MatchCandidateTuple(lst) ->
                    let folder x y =
                        match x with
                        | Some(x') -> CheckMatchCandidate x (fst y) (snd y) 
                        | Option.None -> Option.None
                    match v with
                    | Tuple(lst') -> if (List.length lst) <> (List.length lst') then Option.None else List.fold folder (Some(st)) (List.zip lst' lst)
                    | _ -> Option.None
              | _ -> failwith "Internal logic error"
        | Option.None -> Option.None
    match entry with
    | MatchEntry(mc, mToEval) -> let v = CheckMatchCandidate (Some(Map.empty)) exp mc in
                                    match v with
                                    | Option.None -> Option.None
                                    | Some(extraEnv) -> Some(Evaluate mToEval (Merge extraEnv env))
    | _ -> failwith "Internal logic error"
        
        
let rec EvalList (lst:Expression list) (env:DoubleEnvironment) (modules:ModuleSet) (filename:string) : DoubleEnvironment =
    let folder en ex =
        let evalEx = (Evaluate ex en) in
        match evalEx with
        | LetResult(x, y) -> Set x y en
        | Import(x) -> try Import (Map.find x modules) en
                            with _ -> let fname = (Path.GetDirectoryName filename) + "\\" + x + ".ppl" in try let newEnv = (RunProgramFromFile fname modules ) 
                                                                                                               in (Merge (Dynamic newEnv) (Import (Static newEnv) en) )
                                                                                                                with _ -> failwith (sprintf "Can't run program from %s" fname)
                                                                        
        | DefineConstraint(name, c) -> Set name c en
        | _ -> en
    in
    List.fold folder env lst
and RunProgram (program:Program) (modules:ModuleSet) (filename:string) : DoubleEnvironment =
    let startEnv = DoubleEnvironment(Map.empty, Map.empty)
    EvalList program (Import StdModule.STD_MODULE startEnv) modules filename
and RunProgramFromFile (filename:string) (modules:ModuleSet) : DoubleEnvironment =
    let fileContent =
        File.ReadAllText filename
    match (run Parser.FinalParser fileContent) with
    | Failure(x, _, _) -> failwith (sprintf "%s" x)
    | Success(x, _, _) -> (RunProgram x modules filename)
