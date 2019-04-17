module Tokenizer

open Token

type State = {
    source: string
    start: int
    current: int
    line: int
    col: int
}

let tokenize (code: string): Token seq =
    let init = {
        source = code;
        start = 0;
        current = 0;
        line = 1;
        col = 1;
    }
    Seq.unfold (fun state -> None) init
