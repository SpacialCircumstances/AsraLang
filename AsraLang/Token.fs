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
    | Unrecognized of string

type Token = {
    token: TokenType
    col: int
    line: int
    length: int
    lineSpan: int
}

let token (t: TokenType) (c: int) (l: int) (len: int) = { token = t; col = c; line = l; length = len; lineSpan = 0 }