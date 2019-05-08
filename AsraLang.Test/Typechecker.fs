module Typechecker

open UntypedAST
open Xunit
module T = TypedAST

let assertContextContains (state: Typechecker.State) (name: string) (atype: T.AType) =
    match Map.tryFind name state.context with
        | None -> Assert.True(false, sprintf "Variable %s not found" name)
        | Some tp -> Assert.Equal(atype, tp)

[<Fact>]
let ``Variable declaration`` () =
    let ast = [
        DefineVariableExpression {
            variableName = Simple "a";
            value = LiteralExpression (IntLiteral 12L)
        }
        DefineVariableExpression {
            variableName = Annotated {
                varName = "b"
                typeName = "String"
            }
            value = LiteralExpression (StringLiteral "test")
        }
    ]
    let typedAst, ctx = Typechecker.typecheck ast
    Assert.Equal(ast.Length, Seq.length typedAst)
    assertContextContains ctx "a" (T.Native "Int")
    assertContextContains ctx "b" (T.Native "String")