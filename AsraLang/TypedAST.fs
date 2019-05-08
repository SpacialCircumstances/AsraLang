module TypedAST

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
                                            sprintf "(%A -> %A) %A" ft.input ft.output funt.output

type LiteralValue = 
    | Int of int64
    | String of string
    | Float of float

type Literal = {
    atype: AType
    literalValue: LiteralValue
}

type VariableBinding = {
    varName: string
    varType: AType
    value: Expression
}

and FunctionCall = {
    func: Expression
    funcType: AType
    args: (Expression * AType) list
    returnType: AType
}

and Block = {
    parameters: (Expression * AType) list
    body: (Expression * AType) list
    blockType: AType
}

and Expression =
    | LiteralExpression of Literal
    | VariableBindingExpression of VariableBinding
    | FunctionCallExpression of FunctionCall
    | VariableExpression of string * AType
    | BlockExpression of Block
    | GroupExpression of Expression

let rec getType (expr: Expression) =
    match expr with
        | LiteralExpression lit -> lit.atype
        | VariableBindingExpression bind -> bind.varType
        | FunctionCallExpression fc -> fc.returnType
        | VariableExpression (_ , t) -> t
        | BlockExpression block -> block.blockType
        | GroupExpression expr -> getType expr

let rec returnType (funcT: AType) (paramTs: AType list): Result<AType, string> =
    match funcT with
        | Native t -> 
            match List.length paramTs with
                | 0 -> Ok funcT
                | _ -> sprintf "Type %A is not a function" t |> Error
        | FunctionType t -> 
            match List.tryHead paramTs with
                | None -> Ok funcT //Currying
                | Some pt ->
                    if pt = t.input then
                        returnType t.output (List.tail paramTs)
                    else
                        sprintf "Expected type: %A, but got %A instead" t.input pt |> Error