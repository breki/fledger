module fledger.ParsingDefaultCommodityDirective

open FParsec

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingAmounts

// default commodity = "D", amount
let pDefaultCommodity<'T> : Parser<JournalItem, 'T> =
    pstring "D" >>. whitespace1 >>. pAmount
    .>> whitespace
    .>> newlineOrEof
    |>> DefaultCommodity
    <??> "default commodity"
