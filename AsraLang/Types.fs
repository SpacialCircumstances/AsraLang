module Types

type FunType = {
    input: AType
    output: AType
}

and AType = 
    | Native of string
    | FunctionType of FunType
with
    override x.ToString() = match x with
                                | Native str -> str
                                | FunctionType funt -> 
                                    match funt.input with
                                        | Native tp ->
                                            sprintf "%s -> %O" tp funt.output
                                        | FunctionType ft ->
                                            sprintf "(%O -> %O) -> %O" ft.input ft.output funt.output

let aint = Native "Int"
let astring = Native "String"
let aunit = Native "Unit"
let afloat = Native "Float"
let abool = Native "Bool"

let rec returnType (funcT: AType) (paramTs: AType list): Result<AType, string> =
    match funcT with
        | Native t -> 
            match List.length paramTs with
                | 0 -> Ok funcT
                | _ -> sprintf "Cannot apply argument of %A to value of type %A" (List.head paramTs) t |> Error
        | FunctionType t -> 
            match List.tryHead paramTs with
                | None -> Ok funcT //Currying
                | Some pt ->
                    if pt = t.input then
                        returnType t.output (List.tail paramTs)
                    else
                        sprintf "Expected type for function application: %A, but got %A instead" t.input pt |> Error

let rec appliedType (funcT: AType) (paramsCount: int) =
    if paramsCount = 0 then
        Some funcT
    else
        match funcT with
            | Native i -> None
            | FunctionType ft -> appliedType ft.output (paramsCount - 1)

let rec genFunType (paramTypes: AType list) (retType: AType) = 
    match paramTypes with
        | [] -> FunctionType { input = Native "Unit"; output = retType }
        | [tp] -> FunctionType { input = tp; output = retType }
        | tp :: x -> FunctionType { input = tp; output = genFunType x retType }