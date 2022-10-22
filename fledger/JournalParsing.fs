module fledger.JournalParsing

open FParsec

open fledger.Journal
open fledger.ParsingDefaultCommodity
open fledger.ParsingTransactions


let pJournalItem: Parser<JournalItem, unit> =
    (pTx |>> Transaction)
    <|> (pDefaultCommodity |>> DefaultCommodity)

let pJournal: Parser<Journal, unit> =
    many pJournalItem
    |>> fun items -> { Items = items }
    <?> "journal"
