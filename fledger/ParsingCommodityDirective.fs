module fledger.ParsingCommodityDirective

open FParsec

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingAmounts

// commodity = "commodity", whitespace1, currency
let pCommodityDirective<'T> : Parser<JournalItem, 'T> =
    pstring "commodity" >>. whitespace1 >>. pCurrency
    .>> whitespace
    .>> newlineOrEof
    |>> Commodity
    <??> "commodity directive"
