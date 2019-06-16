module Types

type FunType = {
    input: AType
    output: AType
}

and AType = 
    | Native of string
    | Generic of string
    | FunctionType of FunType
with
    override x.ToString() = match x with
                                | Native str -> str
                                | Generic tp -> "'" + tp
                                | FunctionType funt -> 
                                    match funt.input with
                                        | Generic tp ->
                                            sprintf "'%s -> %O" tp funt.output
                                        | Native tp ->
                                            sprintf "%s -> %O" tp funt.output
                                        | FunctionType ft ->
                                            sprintf "(%O -> %O) -> %O" ft.input ft.output funt.output

let aint = Native "Int"
let astring = Native "String"
let aunit = Native "Unit"
let afloat = Native "Float"
let abool = Native "Bool"

type Context = {
    resolvedGenerics: Map<string, AType>
}    

let rec resolveGeneric (t: string) (ctx: Context) = 
    match Map.tryFind t ctx.resolvedGenerics with
        | None -> None
        | Some t -> match t with
                        | Generic gt -> match resolveGeneric gt ctx with
                                        | None -> Some t
                                        | Some x -> Some x
                        | _ -> Some t

let private addGeneric (gn: string) (gt: AType) (ctx: Context) = { ctx with resolvedGenerics = Map.add gn gt ctx.resolvedGenerics }

let rec private genericEqFun (inT: AType) (paramT: AType) (ctx: Context) = 
    match inT with
        | Native _ ->
            match paramT with
                | Native _ ->
                    if inT = paramT then
                        Ok (), ctx
                    else
                        Error (sprintf "Expected argument of type: %A, but got: %A" inT paramT), ctx
                | FunctionType _ ->
                    Error (sprintf "Expected argument of type: %A, but got: %A" inT paramT), ctx
                | Generic _ ->
                    Error (sprintf "Due to expected type of %A, the argument of %A would be restricted" inT paramT), ctx
        | Generic g ->
            match resolveGeneric g ctx with
                | Some resolvedT -> 
                    match resolvedT with
                        | Generic gn ->
                            Ok (), addGeneric gn paramT ctx
                        | _ -> genericEqFun resolvedT paramT ctx
                | None ->
                    Ok (), addGeneric g paramT ctx
        | FunctionType ft ->
            match paramT with
                | FunctionType pft ->
                    let ir = genericEqFun ft.input pft.input ctx
                    match ir with
                        | Ok _, newCtx ->
                            genericEqFun ft.output pft.output newCtx
                        | Error e, newCtx -> Error e, newCtx
                | _ -> 
                    Error (sprintf "Expected argument of type: %A, but got: %A" inT paramT), ctx

let rec private genericEqFirst (inT: AType) (paramT: AType) (ctx: Context) =
    match inT with
        | Native _ ->
            match paramT with
                | Native _ ->
                    if inT = paramT then
                        Ok (), ctx
                    else
                        Error (sprintf "Expected argument of type: %A, but got: %A" inT paramT), ctx
                | FunctionType _ ->
                    Error (sprintf "Expected argument of type: %A, but got: %A" inT paramT), ctx
                | Generic _ ->
                    Error (sprintf "Due to expected type of %A, the argument of %A would be restricted" inT paramT), ctx
        | Generic g ->
            match resolveGeneric g ctx with
                | Some resolvedT -> 
                    match resolvedT with
                        | Generic gn ->
                            Ok (), addGeneric gn paramT ctx
                        | _ -> genericEqFirst resolvedT paramT ctx
                | None ->
                    Ok (), addGeneric g paramT ctx
        | FunctionType ft ->
            match paramT with
                | FunctionType pft ->
                    let ir = match ft.input with
                                | FunctionType _ ->
                                    genericEqFun ft.input pft.input ctx
                                | _ ->
                                    genericEqFirst ft.input pft.input ctx
                    match ir with
                        | Ok _, newCtx ->
                            genericEqFirst ft.output pft.output newCtx
                        | Error e, newCtx -> Error e, newCtx
                | _ -> 
                    Error (sprintf "Expected argument of type: %A, but got: %A" inT paramT), ctx

let rec private pReturnType (ctx: Context) (funcT: AType) (paramTs: AType list): Result<AType, string> * Context =
    match List.tryHead paramTs with
        | None -> Ok funcT, ctx
        | Some nextParam ->
            match funcT with
                | Native _ -> Error (sprintf "To many arguments: %A" paramTs), ctx
                | Generic _ -> Error (sprintf "To many arguments: %A" paramTs), ctx
                | FunctionType ft ->
                    let inT = ft.input
                    let outT = ft.output
                    match genericEqFirst inT nextParam ctx with
                        | Ok _, newCtx -> pReturnType newCtx outT (List.tail paramTs)
                        | Error e, newCtx -> Error e, newCtx

let returnType = pReturnType { resolvedGenerics = Map.empty }

let rec appliedType (funcT: AType) (paramsCount: int) =
    if paramsCount = 0 then
        Some funcT
    else
        match funcT with
            | Native _ -> None
            | Generic _ -> None
            | FunctionType ft -> appliedType ft.output (paramsCount - 1)

let rec genFunType (paramTypes: AType list) (retType: AType) = 
    match paramTypes with
        | [] -> FunctionType { input = Native "Unit"; output = retType }
        | [tp] -> FunctionType { input = tp; output = retType }
        | tp :: x -> FunctionType { input = tp; output = genFunType x retType }