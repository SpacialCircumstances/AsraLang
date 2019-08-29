module Ast

open FParsec

type LiteralValue = 
    | Int of int64
    | String of string
    | Float of float
    | Unit

type Literal<'data> = {
    data: 'data
    literalValue: LiteralValue
}

type ParameterizedTypeDeclaration = {
    name: string
    genericParameters: TypeDeclaration list
}

and TypeDeclaration =
    | Parameterized of ParameterizedTypeDeclaration
    | Name of string
    | Generic of string
    | Function of TypeDeclaration * TypeDeclaration

let rec funTypeDecl (tds: TypeDeclaration list) =
    match List.length tds with
        | 1 -> List.head tds
        | _ -> Function (List.head tds, funTypeDecl (List.tail tds))

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
    | ArrayLiteralExpression of 'data * Expression<'data> list

type UntypedExpression = Expression<Position>

type UntypedTestExpression = Expression<unit>

type TypedExpression = Expression<Types.AType>

let rec getData (expr: Expression<'a>) =
    match expr with
        | LiteralExpression lit -> lit.data
        | VariableBindingExpression bind -> bind.varData
        | FunctionCallExpression fc -> fc.data
        | VariableExpression (_ , t) -> t
        | BlockExpression block -> block.data
        | GroupExpression expr -> getData expr
        | ArrayLiteralExpression (t, _) -> t

let rec map (mapper: 'a -> 'b) (expr: Expression<'a>) = 
    match expr with
        | LiteralExpression lit -> LiteralExpression { literalValue = lit.literalValue; data = mapper lit.data }
        | VariableBindingExpression bind -> VariableBindingExpression { varName = bind.varName; value = map mapper bind.value; varData = mapper bind.varData }
        | FunctionCallExpression fc -> FunctionCallExpression { func = map mapper fc.func; args = List.map (map mapper) fc.args; data = mapper fc.data }
        | VariableExpression (n, t) -> VariableExpression (n, mapper t)
        | BlockExpression block -> BlockExpression { data = mapper block.data; parameters = block.parameters; body = List.map (map mapper) block.body }
        | GroupExpression expr -> map mapper expr
        | ArrayLiteralExpression (d, el) -> ArrayLiteralExpression (mapper d, List.map (map mapper) el)