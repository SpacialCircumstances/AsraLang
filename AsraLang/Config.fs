module Config

type Config = {
    parserTracing: bool
    preludePath: string
}

let currentConfig = {
    parserTracing = false
    preludePath = "../Stdlib/prelude.js"
}