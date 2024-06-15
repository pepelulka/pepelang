module Math

open AST

let private _math_unary_float_func_impl func lst =
    match lst with
    | [] -> None()
    | [Float(x)] -> Float(func x)
    | [Int(x)] -> Float(func (float x) )
    | _ as s -> failwith (sprintf "Can't apply sqrt to %A" s)

let private _math_sqrt =
    InternalFunction(
        _math_unary_float_func_impl sqrt
    )

let private _math_sin =
    InternalFunction(
        _math_unary_float_func_impl sin
    )

let private _math_cos =
    InternalFunction(
        _math_unary_float_func_impl sin
    )

let private _math_tan =
    InternalFunction(
        _math_unary_float_func_impl tan
    )
    

// Final:

let MathModule = Module [
    "sqrt", _math_sqrt;
    "sin", _math_sin;
    "cos", _math_cos;
    "tan", _math_tan;
]