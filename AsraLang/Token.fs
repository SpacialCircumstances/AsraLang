module Token

type TokenType =
    | FloatLiteral of float
    | IntLiteral of int64
    | StringLiteral of string
    | Identifier of string
    | Equal
    | Separator
    | Colon
    | BlockOpen //[
    | BlockClose //]
    | LeftParen //(
    | RightParen //)
    | Comma
    | Arrow //->

type Token = {
    token: TokenType
    col: int
    line: int
}

let token (t: TokenType) (c: int) (l: int) = { token = t; col = c; line = l }