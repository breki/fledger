module fledger.ParsingJournal

open FParsec

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingCommodityDirective
open fledger.ParsingDefaultCommodityDirective
open fledger.ParsingMarketPriceDirective
open fledger.ParsingTransactions

// todo 70: add support for account directives
// todo 100: add support for account directives
// todo 110: add a test to parse my own ledger file

let pJournalItem<'T> : Parser<JournalItem option, 'T> =
    (pTx |>> Transaction |>> Some)
    <|> (pCommodityDirective |>> Some)
    <|> (pDefaultCommodity |>> Some)
    <|> (pMarketPrice |>> Some)
    <|> (pEmptyLine |>> fun _ -> None)
    <??> "journal item"

let pJournal<'T> : Parser<Journal, 'T> =
    many pJournalItem .>> eof
    |>> fun items ->
            let actualItems = filterOutNone items
            { Items = actualItems }
    <??> "journal"
