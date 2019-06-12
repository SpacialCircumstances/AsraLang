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

let private resolveGeneric t ctx = Map.tryFind t ctx.resolvedGenerics
let private addGeneric gn gt ctx = { ctx with resolvedGenerics = Map.add gn gt ctx.resolvedGenerics }

let rec private pReturnType (ctx: Context) (funcT: AType) (paramTs: AType list): Result<AType, string> * Context =
    match funcT with
        | Generic t ->
            match List.length paramTs with
                | 0 -> 
                    match resolveGeneric t ctx with
                        | Some resG -> Ok resG, ctx
                        | None -> Ok funcT, ctx //Stays generic because it is not resolved
                | _ -> sprintf "Cannot apply argument of %A to value of type %A" (List.head paramTs) t |> Error, ctx
        | Native t -> 
            match List.length paramTs with
                | 0 -> Ok funcT, ctx
                | _ -> sprintf "Cannot apply argument of %A to value of type %A" (List.head paramTs) t |> Error, ctx
        | FunctionType t -> 
            match List.tryHead paramTs with
                | None -> Ok funcT, ctx //Currying
                | Some currentParamType ->
                    match t.input with
                        | Generic gt -> 
                            match resolveGeneric gt ctx with
                                | Some rt ->
                                    if currentParamType = rt then
                                        pReturnType ctx t.output (List.tail paramTs)
                                    else
                                        sprintf "Got %O instead of expected type for function application: %O, because '%s was resolved to it earlier" currentParamType rt gt |> Error, ctx
                                | None ->
                                    //Generic was not resolved yet, so we add it for future usage
                                    pReturnType (addGeneric gt currentParamType ctx) t.output (List.tail paramTs)
                                    
                        | _ ->
                            if currentParamType = t.input then
                                pReturnType ctx t.output (List.tail paramTs)
                            else
                                sprintf "Expected type for function application: %O, but got %O instead" t.input currentParamType |> Error, ctx

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