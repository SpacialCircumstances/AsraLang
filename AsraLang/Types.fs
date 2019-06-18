module Types

type FunType = {
    input: AType
    output: AType
}

and AType = 
    | Primitive of string
    | Generic of string
    | FunctionType of FunType
with
    override x.ToString() = match x with
                                | Primitive str -> str
                                | Generic tp -> "'" + tp
                                | FunctionType funt -> 
                                    match funt.input with
                                        | Generic tp ->
                                            sprintf "'%s -> %O" tp funt.output
                                        | Primitive tp ->
                                            sprintf "%s -> %O" tp funt.output
                                        | FunctionType ft ->
                                            sprintf "(%O -> %O) -> %O" ft.input ft.output funt.output

let anumber = Primitive "Number"
let astring = Primitive "String"
let aunit = Primitive "Unit"
let abool = Primitive "Bool"

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

let private addGeneric (gn: string) (gt: AType) (ctx: Context) = 
    { ctx with resolvedGenerics = Map.add gn gt ctx.resolvedGenerics }

let rec curryGeneric (f: AType) (ctx: Context) =
    match f with
        | Primitive _ -> f
        | Generic gt ->
            match resolveGeneric gt ctx with
                | None -> f
                | Some rt -> rt
        | FunctionType ft ->
            FunctionType {
                input = curryGeneric ft.input ctx
                output = curryGeneric ft.output ctx
            }

let private simpleTypeEq (inT: AType) (paramT: AType) (ctx: Context) =
    if inT = paramT then
        Ok (), ctx
    else
        Error (sprintf "Expected argument of type: %O, but got: %O" inT paramT), ctx

let rec private genericEqFun (inT: AType) (paramT: AType) (ctx: Context) =
    match inT, paramT with
        | Primitive _, Primitive _ ->
            simpleTypeEq inT paramT ctx
        | FunctionType fIT, FunctionType pIT ->
            match genericEqFun fIT.input pIT.input ctx with
                | Ok _, newCtx ->
                    genericEqFun fIT.output pIT.output newCtx
                | Error e, newCtx -> Error e, newCtx
        | Primitive _, Generic _ -> Ok (), ctx //Ok because the generic is in a function passed as argument
        | Generic iGT, Generic pGT ->
            Ok (), ctx
        | Generic iGT, Primitive pNT ->
            match resolveGeneric iGT ctx with
                | None -> Ok (),  addGeneric iGT paramT ctx
                | Some rIGT ->
                    match rIGT with 
                        | Generic _ -> Ok (), addGeneric iGT paramT ctx
                        | _ -> genericEqFun rIGT paramT ctx
        | _ -> Error (sprintf "Expected argument of type: %O, but got: %O" inT paramT), ctx            

let rec private genericEqFirst (inT: AType) (paramT: AType) (ctx: Context) =
    match inT, paramT with
        | Primitive iNT, Primitive pNT ->
            simpleTypeEq inT paramT ctx
        | Generic iGT, _ ->
            match resolveGeneric iGT ctx with
                | None ->
                    Ok (), addGeneric iGT paramT ctx
                | Some rIT ->
                    genericEqFirst rIT paramT ctx
        | Primitive iNT, Generic pGT ->
            Error (sprintf "Expected argument of type: %O, but got: %O. %O would be restricted." inT paramT paramT), ctx
        | FunctionType fIT, FunctionType pIT ->
            genericEqFun inT paramT ctx
        | _ -> Error (sprintf "Expected argument of type: %O, but got: %O" inT paramT), ctx

let rec private pReturnType (ctx: Context) (funcT: AType) (paramTs: AType list): Result<AType, string> * Context =
    match List.tryHead paramTs with
        | None -> 
            match funcT with
                | Generic gt -> 
                    match resolveGeneric gt ctx with
                        | None -> Ok funcT, ctx
                        | Some genT -> Ok genT, ctx
                | _ -> Ok funcT, ctx
        | Some nextParam ->
            match funcT with
                | Primitive _ -> Error (sprintf "To many arguments: %O" paramTs), ctx
                | Generic _ -> Error (sprintf "To many arguments: %O" paramTs), ctx
                | FunctionType ft ->
                    let inT = ft.input
                    let outT = ft.output
                    match genericEqFirst inT nextParam ctx with
                        | Ok _, newCtx -> pReturnType newCtx outT (List.tail paramTs)
                        | Error e, newCtx -> Error e, newCtx

let returnType (funcT: AType) (paramTs: AType list) = 
    let (rt, ctx) = pReturnType { resolvedGenerics = Map.empty } funcT paramTs
    match rt with
        | Ok rt -> curryGeneric rt ctx |> Ok, ctx
        | Error e -> Error e, ctx

let rec appliedType (funcT: AType) (paramsCount: int) =
    if paramsCount = 0 then
        Some funcT
    else
        match funcT with
            | Primitive _ -> None
            | Generic _ -> None
            | FunctionType ft -> appliedType ft.output (paramsCount - 1)

let rec genFunType (paramTypes: AType list) (retType: AType) = 
    match paramTypes with
        | [] -> FunctionType { input = Primitive "Unit"; output = retType }
        | [tp] -> FunctionType { input = tp; output = retType }
        | tp :: x -> FunctionType { input = tp; output = genFunType x retType }