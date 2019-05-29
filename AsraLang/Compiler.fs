module Compiler

open System.IO
open JsGenerator
open Typechecker

module Parameters =
    type T = {
        mainFile: string
        searchDirectories: string list
        outputFile: string
    }
    
    type T2 = CompilerParameters of T

    let create (mainFile: string) (dirs: string list) (outFile: string): Result<T2, string> =
        match File.Exists mainFile with
            | true ->
                let dirErrors = List.fold (fun errs dir -> match Directory.Exists dir with
                                                            | true -> errs
                                                            | false -> (sprintf "Directory: %s not found or not a directory" dir) :: errs) [] dirs
                match List.length dirErrors with
                    | 0 -> 
                        {
                            mainFile = mainFile
                            searchDirectories = dirs
                            outputFile = outFile
                        } |> CompilerParameters |> Ok
                    | _ -> 
                        "Search directories not valid: \n" + (String.concat "\n" dirErrors) |> Error
            | false ->
                Error (sprintf "File %s does not exist" mainFile)

open Parameters

type CompilerError = 
    | ParserError of string
    | TypeError of string
    | Warning of string

let compile (CompilerParameters parameters) =
    let parsed = File.ReadAllText parameters.mainFile |> Parser.defaultParser
    match parsed with
        | Error e ->
            [ ParserError e ]
        | Ok ast ->
            try
                let outFile = parameters.outputFile
                let typedAst, _, errors = Typechecker.typecheck ast JsLibrary.externs
                match List.length errors with
                    | 0 ->
                        let jsGen = genState (File.ReadAllText Config.currentConfig.preludePath) JsLibrary.externs
                        let generatedJs = generateJs jsGen typedAst
                        if (File.Exists outFile) then File.Delete outFile
                        File.WriteAllText (outFile, generatedJs)
                        []
                    | _ ->
                        List.map (fun e -> match e with
                                            | Message.TypeError s -> TypeError s
                                            | Message.Warning w -> Warning w) errors
            with
                | :? System.InvalidOperationException as ex -> [ TypeError ex.Message ]
            

