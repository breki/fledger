module fledger.ParsingDefaultCommodity

open FParsec

open fledger.ParsingAmounts

// default commodity = "D", amount
let pDefaultCommodity =
    pstring "D" >>. pAmount
