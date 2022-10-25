module fledger.ParsingJournal

open FParsec

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingAccountDirective
open fledger.ParsingComment
open fledger.ParsingCommodityDirective
open fledger.ParsingDefaultCommodityDirective
open fledger.ParsingMarketPriceDirective
open fledger.ParsingTransactions

let pJournalItem<'T> : Parser<JournalItem option, 'T> =
    (pTx |>> Transaction |>> Some)
    <|> (pComment |>> Some)
    <|> (pCommodityDirective |>> Some)
    <|> (pDefaultCommodity |>> Some)
    <|> (pAccountDirective |>> Some)
    <|> (pMarketPrice |>> Some)
    <|> (pEmptyLine |>> fun _ -> None)
    <??> "journal item"

let pJournal<'T> : Parser<Journal, 'T> =
    many pJournalItem .>> eof
    |>> fun items ->
            let actualItems = filterOutNone items
            { Items = actualItems }
    <??> "journal"
