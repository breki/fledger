module fledger.Parsing.ParsingMarketPriceDirective

open FParsec

open fledger.Journal
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingAmounts

// market price
//   = "P", whitespace1, date, whitespace1, commodity, whitespace1, amount
let pMarketPrice<'T> : Parser<JournalItem, 'T> =
    pstring "P" >>. whitespace1 >>. pDate
    .>> whitespace1
    .>>. pCommodity
    .>> whitespace1
    .>>. pAmount
    .>> endOfLineWhitespace
    |>> (fun ((date, commodity), amount) ->
        { Date = date
          Commodity = commodity
          Price = amount }
        |> MarketPrice)
    <??> "market price"
