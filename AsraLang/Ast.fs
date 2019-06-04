module Ast

open FParsec

type LiteralValue = 
    | Int of int64
    | String of string
    | Float of float

type Literal<'data> = {
    data: 'data
    literalValue: LiteralValue
}

type TypeDeclaration =
    | Name of string
    | Function of TypeDeclaration * TypeDeclaration

type TypeAnnotated = { //TODO: Support complex types
    typeName: TypeDeclaration
    varName: string
}

type Declaration = 
    | Simple of string
    | Annotated of TypeAnnotated

let toVarName (decl: Declaration) = match decl with
                                        | Simple s -> s
                                        | Annotated ta -> ta.varName

type VariableBinding<'data> = {
    varName: Declaration
    varData: 'data
    value: Expression<'data>
}

and FunctionCall<'data> = {
    func: Expression<'data>
    args: Expression<'data> list
    data: 'data
}

and Block<'data> = {
    parameters: Declaration list
    body: Expression<'data> list
    data: 'data
}

and Expression<'data> =
    | LiteralExpression of Literal<'data>
    | VariableBindingExpression of VariableBinding<'data>
    | FunctionCallExpression of FunctionCall<'data>
    | VariableExpression of string * 'data
    | BlockExpression of Block<'data>
    | GroupExpression of Expression<'data>

type UntypedExpression = Expression<Position>

type UntypedTestExpression = Expression<unit>

type TypedExpression = Expression<Types.AType>

let rec getType (expr: TypedExpression) =
    match expr with
        | LiteralExpression lit -> lit.data
        | VariableBindingExpression bind -> bind.varData
        | FunctionCallExpression fc -> fc.data
        | VariableExpression (_ , t) -> t
        | BlockExpression block -> block.data
        | GroupExpression expr -> getType expr