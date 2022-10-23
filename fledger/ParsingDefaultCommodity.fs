module fledger.ParsingDefaultCommodity

open FParsec

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingAmounts

// default commodity = "D", amount
let pDefaultCommodity: Parser<JournalItem, UserState> =
    pstring "D" >>. whitespace1 >>. pAmount
    .>> whitespace
    .>> newlineOrEof
    |>> DefaultCommodity
    <??> "default commodity"
