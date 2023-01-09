module fledger.Parsing.ParsingDefaultCommodityDirective

open FParsec

open fledger.Journal
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingAmounts
open fledger.Parsing.ParsingUtils

// default commodity = "D", amount
let pDefaultCommodity<'T> : Parser<int64 * JournalItem, 'T> =
    pstring "D" >>. whitespace1 >>. pAmount .>> whitespace .>> newlineOrEof
    |> withPos
    |>> (fun x -> x.Start.Line, DefaultCommodity x.Value)
    <??> "default commodity"
