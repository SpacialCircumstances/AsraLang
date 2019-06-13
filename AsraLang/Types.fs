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
    match List.tryHead paramTs with
        | None -> Ok funcT, ctx
        | Some nextParam ->
            match funcT with
                | Native _ -> Error (sprintf "To many arguments: %A" paramTs), ctx
                | Generic _ -> Error (sprintf "To many arguments: %A" paramTs), ctx
                | FunctionType ft ->
                    let inT = ft.input
                    let outT = ft.output
                    if inT = nextParam then
                        pReturnType ctx outT (List.tail paramTs)
                    else
                        Error (sprintf "Expected argument of type: %A, but got: %A" inT nextParam), ctx

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