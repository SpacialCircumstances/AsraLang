module Config

type Config = {
    parserTracing: bool
    preludePath: string
}

let currentConfig = {
    parserTracing = false
    preludePath = "Prelude/prelude.js"
}