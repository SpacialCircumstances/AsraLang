module Token

type TokenType =
    | FloatLiteral of float
    | IntLiteral of int64
    | StringLiteral of string
    | Identifier of string
    | Equal
    | Dot
    | Colon
    | BlockOpen //[
    | BlockClose //]
    | LeftParen //(
    | RightParen //)
    | Comma
    | Arrow //->
    | Comment

type Position = {
    line: int
    column: int
}

type Token = {
    token: TokenType
    position: Position
}