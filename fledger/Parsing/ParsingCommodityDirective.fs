module fledger.Parsing.ParsingCommodityDirective

open FParsec

open fledger.Journal
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingAmounts
open fledger.Parsing.ParsingUtils

// commodity = "commodity", whitespace1, commodity symbol
let pCommodityDirective<'T> : Parser<int64 * JournalItem, 'T> =
    pstring "commodity" >>. whitespace1 >>. pCommodity
    .>> whitespace
    .>> newlineOrEof
    |> withPos
    |>> (fun x -> x.Start.Line, Commodity x.Value)
    <??> "commodity directive"
