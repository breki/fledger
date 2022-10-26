module fledger.Parsing.ParsingJournal

open FParsec

open fledger.Journal
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingAccountDirective
open fledger.Parsing.ParsingComment
open fledger.Parsing.ParsingCommodityDirective
open fledger.Parsing.ParsingDefaultCommodityDirective
open fledger.Parsing.ParsingMarketPriceDirective
open fledger.Parsing.ParsingTransactions

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
