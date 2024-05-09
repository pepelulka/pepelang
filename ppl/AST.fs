module AST

type Id = string
type Expression =
    // Basic data types:
    | Int of int
    | String of string
    | Bool of bool
    | Unit of unit
    // Abstract:
    | Var of Id
    | Error of string
    // Operators:
    | BuiltinUnaryOperator of string*Expression // ~ <exp>
    | BuiltinBinaryOperator of string*Expression*Expression // + <exp1> <exp2>
    // TODO: User defined operators
    // Language constructions:
    | If of Expression*Expression*Expression // if <expr1> then <expr2> else <expr3>
    | DoAndReturn of Expression list*Expression // do <exp[0]> $ <exp[1]> $ ... $ <exp[n]> return <exp2>
    | Let of Id*Expression // let <name> = <exp>
    | Environment of Env
    // Lambdas (lazy, evaluated only when applied):
    | Lambda of Id*Expression
    | PartiallyAppliedLambda of Expression*Env
    | Apply of Expression*Expression list // > <exp> [<exp list>] 
    // Delayed calls (evaluate only when called, just lambda with zero arguments):                                  (Need parser)
    | DelayedCall of Expression // () -> <exp>
    | Call of Expression // call <exp>
and Env = Map<Id, Expression>