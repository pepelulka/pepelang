module StdModule

open AST
open Utils

open FParsec

open System
open System.IO

let rec private _std_plus_impl lst = 
    match lst with
    | [] -> None()
    | [s] -> s
    | [Int(x); Int(y)] -> Int(x + y)
    | [Float(x); Float(y)] -> Float(x + y)
    | [String(x); String(y)] -> AST.String(x + y)
    | [None(); _] -> None()
    | [_; None()] -> None()
    | _ -> List.reduce (fun x y -> _std_plus_impl [x;y]) lst

let private _std_plus =
    InternalFunction(
        _std_plus_impl
    )

let rec private _std_mult_impl lst =
    match lst with 
    | [] -> None()
    | [s] -> s
    | [Int(x); Int(y)] -> Int(x * y)
    | [Float(x); Float(y)] -> Float(x * y)
    | [None(); _] -> None()
    | [_; None()] -> None()
    | _ -> List.reduce (fun x y -> _std_mult_impl [x;y]) lst

let private _std_mult = 
    InternalFunction(
        _std_mult_impl
    )

let rec private _std_minus_impl lst =
    match lst with
    | [] -> None()
    | [Int(x)] -> Int(-x)
    | [Float(x)] -> Float(-x)
    | [None()] -> None()
    | [Int(x); Int(y)] -> Int(x - y)
    | [Float(x); Float(y)] -> Float(x - y)
    | [None(); _] -> None()
    | [_; None()] -> None()
    | _ -> List.reduce (fun x y -> _std_minus_impl [x;y]) lst

let private _std_minus =
    InternalFunction(
        _std_minus_impl
    )

let rec private _std_div_impl lst =
    match lst with
    | [] -> None()
    | [s] -> s
    | [Int(x); Int(y)] -> Int(x / y)
    | [Float(x); Float(y)] -> Float(x / y)
    | [None(); _] -> None()
    | [_; None()] -> None()
    | _ as s -> failwith (sprintf "Can't divide %A" s)

let private _std_div =
    InternalFunction(
        _std_div_impl
    )

let rec private _std_mod_impl lst =
    match lst with
    | [] -> None()
    | [s] -> s
    | [Int(x); Int(y)] -> Int(x % y)
    | [Float(x); Float(y)] -> Float(x % y)
    | [None(); _] -> None()
    | [_; None()] -> None()
    | _ as s -> failwith (sprintf "Can't mod %A" s)

let private _std_mod =
    InternalFunction(
        _std_mod_impl
    )

let rec private _std_bool_oper_impl oper lst =
    match lst with
    | [] -> None()
    | [s] -> s
    | [Bool(x); Bool(y)] -> Bool(oper x y)
    | [None(); _] -> None()
    | [_; None()] -> None()
    | _ -> List.reduce (fun x y -> _std_bool_oper_impl oper [x;y]) lst

let private _std_or =
    InternalFunction(
        _std_bool_oper_impl (||)
    )

let private _std_and =
    InternalFunction(
        _std_bool_oper_impl (&&)
    )

let private _std_not_impl lst =
    match lst with
    | [] -> None()
    | [Bool(s)] -> Bool(not s)
    | _ as s -> failwith (sprintf "Can't apply not to %A" s)

let private _std_not =
    InternalFunction(
        _std_not_impl
    )

let private _std_cmp_opers = ["<", ">", "<=", ">=", "=", "!="]
let rec private _std_cmp_impl name (operInt, operFloat, operString, operBool) lst =
    match lst with
      | [Int(x); Int(y)] -> Bool(operInt x y)
      | [Float(x); Float(y)] -> Bool(operFloat x y)
      | [String(x); String(y)] -> Bool(operString x y)
      | [Bool(x); Bool(y)] -> Bool(operBool x y)
      | [None(); _] -> None()
      | [_; None()] -> None()
      | _ -> failwith ("Invalid call of operator " + name)

let rec private _std_lt =
    InternalFunction(
        _std_cmp_impl "<" ((<), (<), (<), (<))
    )

let rec private _std_gt =
    InternalFunction(
        _std_cmp_impl ">" ((>), (>), (>), (>))
    )

let rec private _std_lte =
    InternalFunction(
        _std_cmp_impl "<=" ((<=), (<=), (<=), (<=))
    )

let rec private _std_gte =
    InternalFunction(
        _std_cmp_impl ">=" ((>=), (>=), (>=), (>=))
    )

let rec private _std_eq =
    InternalFunction(
        _std_cmp_impl "=" ((=), (=), (=), (=))
    )

let rec private _std_neq =
    InternalFunction(
        _std_cmp_impl "!=" ((<>), (<>), (<>), (<>))
    )

let rec private _std_print_impl lst =
    match lst with
    | [] -> None()
    | h::t -> match h with
                | Int(x) -> printf "%d" x
                | String(x) -> printf "%s" x 
                | Bool(x) -> printf "%b" x
                | None() -> printf "none"
                | Float(x) -> printf "%f" x;
                | Literal("nl") -> printfn ""
                | Literal("ws") -> printf " "
                | Tuple(lst) -> let len = List.length lst in
                                    printf "("; List.mapi (fun i x -> _std_print_impl [x] |> ignore; if (i <> len - 1) then printf ", ") lst |> ignore; printf ")" 
                | _ -> failwith (sprintf "Error: Can't print %A" h)
              _std_print_impl t

let private _std_print =
    InternalFunction(
        _std_print_impl
    )

let rec private _std_composition_impl lst =
    match lst with
    | [] -> None()
    | [s] -> s
    | [Lambda(name, e1); _] as [l1; l2] -> Lambda(name, Apply( l2, [Apply(l1, [ Var(name) ])] ) )
    | [Closure(name, e1, en1); _] as [l1; l2] -> Closure(name, Apply(l2, [Apply(l1, [Var(name) ] ) ] ), en1)
    | [InternalFunction(f); _] as [l1; l2] -> Lambda("__smth__", Apply( l2, [Apply(l1, [ Var("__smth__") ])] ) )
    | _ as l -> List.reduce (fun x y -> _std_composition_impl [x; y]) l

let private _std_composition =
    InternalFunction(
        _std_composition_impl
    )

let private _std_id = 
    InternalFunction(
        fun x -> if List.isEmpty x then None() else List.head x
    )

let private _std_match_type_impl lst =
    match lst with
    | [x; y] -> Bool(TypeSatisfyConstraint (TypeOf x) y)
    | _ -> None()

let private _std_match_type =
    InternalFunction(
        _std_match_type_impl
    )

let private _std_read_line_impl lst = 
    AST.String(Console.ReadLine())

let private _std_read_line = 
    InternalFunction(
        _std_read_line_impl
    )

let private _std_to_int_impl lst =
    match lst with
    | [String(x)] -> try Int(int(x)) with | _ -> failwith (sprintf "Failed to convert %s to int" x)
    | [Int(x)] as [s] -> s
    | [Float(x)] -> try Int(int(x)) with | _ -> failwith (sprintf "Failed to convert %A to int" x)
    | [Bool(true)] -> Int(1)
    | [Bool(false)] -> Int(0)
    | _ -> None()

let private _std_to_int =
    InternalFunction(
        _std_to_int_impl
    )

let private _std_to_float_impl lst =
    match lst with
    | [String(x)] -> try Float(float(x)) with | _ -> failwith (sprintf "Failed to convert %s to float" x)
    | [Int(x)] -> Float(float(x))
    | [Float(x)] -> Float(x)
    | [Bool(true)] -> Float(1)
    | [Bool(false)] -> Float(0)
    | _ -> None()

let private _std_to_float =
    InternalFunction(
        _std_to_float_impl
    )

let private _std_parse_value_impl lst =
    match lst with
    | [String(x)] -> parseValue x
    | _ -> None()

let private _std_parse_value =
    InternalFunction(
        _std_parse_value_impl
    )

// Final

let STD_MODULE = Module(
    [
        "+", _std_plus;
        "-", _std_minus;
        "*", _std_mult;
        "/", _std_div;
        "<", _std_lt;
        ">", _std_gt;
        "<=", _std_lte;
        ">=", _std_gte;
        "=", _std_eq;
        "!=", _std_neq;
        "||", _std_or;
        "&&", _std_and;
        "!", _std_not;
        ">>", _std_composition;

        "std.mod", _std_mod;
        "std.match_type", _std_match_type;
        "std.print", _std_print;
        "std.read_line", _std_read_line;
        "std.to_int", _std_to_int;
        "std.to_float", _std_to_float;
        "std.parse", _std_parse_value;
        "std.id", _std_id;
    ]
)