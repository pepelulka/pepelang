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
    if (Array.length args < 2) then Option.None
    else Some(args[1])

let fileContent =
    match filename with
    | Option.None -> failwith "There's no source file given"; Option.None
    | Some(x) -> Some(File.ReadAllText x)

match fileContent with
    | Option.None -> failwith "Failed to read file"
    | Some(x) -> match (run FinalParser x) with
                    | Failure(x, _, _) -> printfn "%s" x
                    | Success(x, _, _) -> (Interpreter.RunProgram x StdModulesSet ) |> ignore
