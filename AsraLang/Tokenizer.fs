module Tokenizer

open Token
open System
open System.Globalization

type State = {
    line: int
    col: int
    comment: bool
    separator: bool
}

let keywords = dict [
    "->", Arrow
    ";", Separator
    ",", Comma
    "=", Equal
    ":", Colon
    "[", BlockOpen
    "]", BlockClose
    "(", LeftParen
    ")", RightParen
]

let isDelimiter (c: Char) = Char.IsWhiteSpace c || c = ';' || c = '(' || c = ')' || c = '[' || c = ']' || c = ',' || c = ':' || c = '#'

let findNext (source: string) (start: int) (pred: char -> bool) =
    let afterStart = Seq.skip start source
    match Seq.tryFindIndex pred afterStart with
        | Some index -> index + start
        | None -> source.Length

let split (code: string) =
    Seq.unfold (fun start -> 
        if start >= code.Length then
            None
        else
            let startChar = code.[start]
            if isDelimiter startChar then
                Some (code.Substring (start, 1), start + 1)
            else
                let next = findNext code start isDelimiter
                let lexeme = code.Substring (start, next - start)
                Some (lexeme, next)) 0

let parseNumber (parseNumber: string * CultureInfo -> 'a) (lexeme: string) (literal: 'a -> TokenType) =
    try
        let fl = parseNumber (lexeme, CultureInfo.InvariantCulture)
        Some (literal fl)
    with
        | :? FormatException -> Some (Unrecognized (sprintf "%s is not a valid number" lexeme))
        | :? OverflowException -> Some (Unrecognized (sprintf "%s is to large" lexeme))
        | _ -> invalidOp "Exception occured parsing number"

let lexemeToToken (state: State) (lexeme: string): (Token option * State) =
    match lexeme with
        | "\n" -> 
            if state.separator then
                (None, { state with col = 1; line = state.line + 1; comment = false })
            else
                (Some (token Separator state.col state.line), { state with col = 1; line = state.line + 1; comment = false; separator = true })
        | ";" -> 
            if state.separator then
                (None, { state with col = 1; comment = false })
            else
                (Some (token Separator state.col state.line), { state with col = 1; separator = true })
        | " " -> (None, { state with col = state.col + 1 })
        | "\r" -> (None, { state with col = state.col + 1 })
        | "#" -> (None, { state with col = state.col + 1; comment = true })
        | _ ->
            let tokenType = 
                if state.comment then
                    None
                else
                    if keywords.ContainsKey lexeme then
                            keywords.[lexeme] |> Some
                        else
                            if lexeme.StartsWith "\"" && lexeme.EndsWith "\"" then
                                StringLiteral (lexeme.Substring (1, lexeme.Length - 2)) |> Some
                            else if Char.IsNumber lexeme.[0] || (lexeme.[0] = '-' && lexeme.Length > 1 && Char.IsNumber lexeme.[1]) then
                                if lexeme.Contains "." then
                                    parseNumber Double.Parse lexeme FloatLiteral
                                else
                                    parseNumber Int64.Parse lexeme IntLiteral
                            else
                                Some (Identifier lexeme)
            let token = match tokenType with
                            | Some tt -> Some (token tt state.col state.line)
                            | None -> None
            (token, { state with col = state.col + lexeme.Length; separator = false })

let mapToTokens (delimited: string seq) =
    let init = {
        line = 1
        col = 1
        comment = false
        separator = true //Parse whitespace on file start
    }
    let tokens, _ = Seq.mapFold lexemeToToken init delimited
    Seq.choose id tokens

let private tokenize (code: string): Token seq =
    code 
       |> split
       |> mapToTokens
    
let tokenizer = fun s -> tokenize s |> Seq.cache