module ParseTree

type Literal = 
    | StringLiteral of string
    | IntLiteral of int64
    | FloatLiteral of float

type TypeAnnotated = {
    typeName: string
    varName: string
}

type Declaration = 
    | Simple of string
    | Annotated of TypeAnnotated

type VariableDefinition = {
    variableName: Declaration
    value: Expression
}

and FunctionCall = {
    func: Expression
    arguments: Expression list
}

and Block = {
    parameters: (Declaration list) option
    body: Expression list
}

and Expression =
    | LiteralExpression of Literal
    | DefineVariableExpression of VariableDefinition
    | FunctionCallExpression of FunctionCall
    | VariableExpression of string
    | BlockExpression of Block
    | GroupExpression of Expression