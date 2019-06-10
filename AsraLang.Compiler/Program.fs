open Compiler

let comp (e1: CompilerError) (e2: CompilerError) =
    match e1 with
        | Warning _ ->
            match e2 with
                | Warning _ -> 0
                | _ -> 1
        | TypeError _ -> 
            match e2 with
                | Warning _ -> -1
                | TypeError _ -> 0
                | ParserError _ -> 1
        | ParserError _ ->
            match e2 with
                | ParserError _ -> 0
                | _ -> -1

[<EntryPoint>]
let main argv =
    let inFile = argv.[0]
    let outFile = argv.[1]
    match Parameters.create inFile [] outFile with
        | Ok parameters -> 
            compile parameters
                |> List.sortWith comp
                |> List.iter (fun err -> match err with
                                            | ParserError e -> printfn "Parser Error: %s" e
                                            | TypeError e -> printfn "Error: %s" e
                                            | Warning w -> printfn "Warning: %s" w)
        | Error err -> printfn "%s" err
    0
