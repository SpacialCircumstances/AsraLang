module Compiler

open System.IO

module CompilerParameters =
    type T = {
        mainFile: string
        searchDirectories: string list
    }

    let create (mainFile: string) (dirs: string list): Result<T, string> =
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
                        } |> Ok
                    | _ -> 
                        "Search directories not valid: \n" + (String.concat "\n" dirErrors) |> Error
            | false ->
                Error (sprintf "File %s does not exist" mainFile)

type CompilerError = 
    | GeneralError
    | ParserError
    | TypeError

