module fledger.Parsing.ParsingMarketPriceDirective

open FParsec

open fledger.Journal
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingAmounts
open fledger.Parsing.ParsingUtils

// market price
//   = "P", whitespace1, date, whitespace1, commodity, whitespace1, amount
let pMarketPrice<'T> : Parser<int64 * JournalItem, 'T> =
    pstring "P" >>. whitespace1 >>. pDate .>> whitespace1 .>>. pCommodity
    .>> whitespace1
    .>>. pAmount
    .>> endOfLineWhitespace
    |>> (fun ((date, commodity), amount) ->
        { Date = date
          Commodity = commodity
          Price = amount })
    |> withPos
    |>> (fun x -> x.Start.Line, MarketPrice x.Value)
    <??> "market price"
