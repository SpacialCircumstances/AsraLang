[<EntryPoint>]
let main argv =
    let inFile = argv.[0]
    let outFile = argv.[1]
    match Compiler.Parameters.create inFile [] outFile with
        | Ok parameters -> 
            let errs = Compiler.compile parameters
            List.iter (printfn "%A") errs
        | Error err -> printfn "%s" err
    0
