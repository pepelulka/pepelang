open Parser
open AST
open FParsec

open System
open System.IO

let StdModulesSet = Map [
    "math", Math.MathModule
]

let args = System.Environment.GetCommandLineArgs()

let filename = 
    if (Array.length args < 2) then failwith "No source file was given"
    else Interpreter.RunProgramFromFile args[1] StdModulesSet
