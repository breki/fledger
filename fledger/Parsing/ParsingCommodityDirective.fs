module fledger.Parsing.ParsingCommodityDirective

open FParsec

open fledger.Journal
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingAmounts

// commodity = "commodity", whitespace1, commodity symbol
let pCommodityDirective<'T> : Parser<JournalItem, 'T> =
    pstring "commodity" >>. whitespace1 >>. pCommodity
    .>> whitespace
    .>> newlineOrEof
    |>> Commodity
    <??> "commodity directive"
