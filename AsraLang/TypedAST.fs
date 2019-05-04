module TypedAST

type FunType = {
    input: AType
    output: AType
}

and AType = 
    | Native of string
    | FunctionType of FunType

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