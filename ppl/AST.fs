module AST

type Expression =
    // Basic types:
    | Bool of bool
    | Char of char      // TODO
    | Float of float
    | Int of int
    | Lambda of string*Expression
    | Lazy of Expression
    | Literal of string
    | None of unit
    | String of string
    | Tuple of Expression list
    
    | Var of string

    // Language constructions:
    | Apply of Expression*Expression list
    | Do of Expression list
    | Fail of Expression
    | Force of Expression
    | If of Expression*Expression*Expression
    | Operator of string
    | Let of string*string list*Expression
    | DefineConstraint of string*Expression
    | LetIn of Expression*Expression
    | Import of string

    | InternalFunction of Internal
    | LazyWithEnv of Expression*Environment
    | LetResult of string*Expression
    | Closure of string*Expression*Environment

    // Type system:
    | BaseType of string
    | Type of Expression list

    // Type constraints:
    // * Type and BaseType also can be interpreted as constraints
    | ConstraintOr of Expression list // List of (types or typeConstraint)
    | ConstraintTuple of Expression list // List of (types or typeConstraint)
    | ConstraintAnyType

    // Pattern matching:
    | MatchCandidateSingleConstraint of string*Expression // | x of <type-constr>
    | MatchCandidateTuple of Expression list //              | ( <match-cand1>, <mc2>, <mc3>)
    | MatchCandidateWildCard //                              | _
    | MatchEntry of Expression*Expression // <Match-candidate> * <Expression-to-eval>
    | Match of Expression*(Expression list) // match <exp> with | <match-entry1> | <match-entry2> ...

and Environment = Map<string, Expression>
and Internal = Expression list -> Expression

type Module = Environment
type ModuleSet = Map<string, Module>
type Program = Expression list

let rec TypeOf exp =
    match exp with
    | Int(_) -> BaseType("int")
    | Char(_) -> BaseType("char")
    | String(_) -> BaseType("string")
    | Bool(_) -> BaseType("bool")
    | Float(_) -> BaseType("float")
    | None() -> BaseType("none")
    | Literal(_) -> BaseType("literal")

    | Closure(_) -> BaseType("lambda")
    | Lambda(_) -> BaseType("lambda")

    | InternalFunction(_) -> BaseType("internal")
    | Lazy(_) -> BaseType("lazy")
    | LazyWithEnv(_) -> BaseType("lazy")

    | Tuple(lst) -> Type(List.map (fun x -> TypeOf x) lst)

    | _ as s -> failwith (sprintf "Can't find out type of %A" s)

// Compare types
let rec TypesEqual exp1 exp2 =
    match (exp1, exp2) with
    | (BaseType(t1), BaseType(t2)) when t1 = t2 -> true
    | (BaseType(t1), Type(_)) -> false
    | (Type(_), BaseType(t2)) -> false
    | (Type(lst1), Type(lst2)) -> if (List.length lst1) <> (List.length lst2) then false
                                    else List.reduce (fun x y -> x && y) (List.map (fun (x, y) -> TypesEqual x y) (List.zip lst1 lst2) )
    | (BaseType(_), BaseType(_)) -> false
    | _ -> failwith (sprintf "Failed to cmp types: %A and %A" exp1 exp2)

let rec TypeSatisfyConstraint tp c =
    match c with
    | ConstraintAnyType -> true
    | BaseType(name) ->
        match tp with
          | BaseType(name') when name' = name -> true
          | _ -> false
    | ConstraintOr(lst) ->
        let folder x y =
            if x then x
            else TypeSatisfyConstraint tp y
        List.fold folder false lst
    | ConstraintTuple(lst) ->
        match tp with
        | Type(lst') -> (List.length lst) = (List.length lst') && List.reduce (&&) (List.map (fun (x, y) -> TypeSatisfyConstraint x y) (List.zip lst' lst) )
        | _ -> false
    | _ -> failwith (sprintf "Expected constraint, got: %A" c)

let BASE_TYPE_NAMES = ["int"; "float"; "bool"; "string"; "none"; "lambda"; "lazy"; "literal"; "internal"; "char"]
