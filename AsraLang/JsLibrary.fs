module JsLibrary

open Types
open Typechecker

let externs = 
    [
        { asraName = "println"; asraType = genFunType [ Generic "a" ] (aunit); externName = "println" }
        { asraName = "+"; asraType = genFunType [ aint; aint ] (aint); externName = "add" }
        { asraName = "-"; asraType = genFunType [ aint; aint ] (aint); externName = "subtract" }
        { asraName = "*"; asraType = genFunType [ aint; aint ] (aint); externName = "multiply" }
        { asraName = "/"; asraType = genFunType [ aint; aint ] (aint); externName = "divide" }
        { asraName = "if"; asraType = genFunType [
            genFunType [ aunit ] (abool)
            genFunType [ aunit ] (Generic "a")
            genFunType [ aunit ] (Generic "a")
        ] (Generic "a"); externName = "iffn" }
        { asraName = "=="; asraType = genFunType [ Generic "a"; Generic "a" ] (abool); externName = "eq" }
        { asraName = "!="; asraType = genFunType [ Generic "a"; Generic "a" ] (abool); externName = "neq" }
        { asraName = "not"; asraType = genFunType [ abool ] (abool); externName = "not" }
        { asraName = "&"; asraType = genFunType [ abool; abool ] (abool); externName = "and" }
        { asraName = "|"; asraType = genFunType [ abool; abool ] (abool); externName = "or" }
        { asraName = "true"; asraType = abool; externName = "trueVal" }
        { asraName = "false"; asraType = abool; externName = "falseVal" }
        { asraName = "toString"; asraType = genFunType [ Generic "a" ] astring; externName = "toStr" }
    ]