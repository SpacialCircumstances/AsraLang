module Ast

type LiteralValue = 
    | Int of int64
    | String of string
    | Float of float

type Literal<'data> = {
    data: 'data
    literalValue: LiteralValue
}

type VariableBinding<'data> = {
    varName: string
    varData: 'data
    value: Expression<'data>
}

and FunctionCall<'data> = {
    func: Expression<'data>
    args: Expression<'data> list
    data: 'data
}

and Block<'data> = {
    parameters: string list
    body: Expression<'data> list
    blockType: 'data
}

and Expression<'data> =
    | LiteralExpression of Literal<'data>
    | VariableBindingExpression of VariableBinding<'data>
    | FunctionCallExpression of FunctionCall<'data>
    | VariableExpression of string * 'data
    | BlockExpression of Block<'data>
    | GroupExpression of Expression<'data>

type UntypedExpression = Expression<unit>

type TypedExpression = Expression<Types.AType>