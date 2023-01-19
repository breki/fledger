module fledger.AccountingFuncs

open fledger.BalanceTypes
open fledger.BasicTypes
open fledger.LedgerTypes

/// Returns the balances of all the accounts.
let accountsBalances (ledger: Ledger) =
    let processPosting
        (balances: AccountsBalances)
        (posting: Posting)
        : AccountsBalances =
        let account = posting.Account
        let amount = posting.Amount
        let commodity = amount.Commodity

        let accountBalance =
            balances.Balances
            |> Map.tryFind account
            |> Option.defaultValue
                { Account = account
                  Balance = MultiCommodityBalance.Empty }

        let newCommodityBalance =
            accountBalance.Balance.Commodities
            |> Map.tryFind commodity
            |> Option.defaultValue (commodity |> Amount.Zero)
            |> (+) amount

        let newAccountBalance =
            { accountBalance with
                Balance =
                    accountBalance.Balance.AddCommodity
                        commodity
                        newCommodityBalance }

        { balances with
            Balances = balances.Balances |> Map.add account newAccountBalance }

    let processTx balances (transaction: Transaction) =
        transaction.Postings |> List.fold processPosting balances

    let initialState = { Balances = Map.empty }

    ledger.Transactions |> List.fold processTx initialState

/// Adds an amount to the balance-by-date structure.
let addAmountToBalance
    date
    (amount: Amount)
    (balances: BalanceByDate)
    : BalanceByDate =
    let commodity = amount.Commodity

    let balance =
        balances
        |> Map.tryFind date
        |> Option.defaultValue MultiCommodityBalance.Empty

    let newBalance =
        balance.Commodities
        |> Map.tryFind commodity
        |> Option.defaultValue (commodity |> Amount.Zero)
        |> (+) amount

    balances |> Map.add date (balance.AddCommodity commodity newBalance)


type PostingFilter = BalanceByDate -> Transaction -> Posting -> BalanceByDate

/// A generic function to calculate balance change history based on the
/// provided processPosting function.
let calculateBalanceChangeHistory
    (processPosting: PostingFilter)
    (ledger: Ledger)
    : BalanceHistory =
    let processTx balances (transaction: Transaction) =
        transaction.Postings
        |> List.fold
            (fun balances -> processPosting balances transaction)
            balances

    let initialState = Map.empty

    let balancesByDates =
        ledger.Transactions |> List.fold processTx initialState

    // sort balances by date
    balancesByDates |> Map.toSeq |> Seq.sortBy fst |> Seq.toList


/// Returns the total multi-currency balance change for each day.
let totalBalanceChangeHistory =
    calculateBalanceChangeHistory (fun balances transaction posting ->
        let date = transaction.Date

        match posting.Account.NameParts[0] with
        | "assets" -> addAmountToBalance date posting.Amount balances
        | "liabilities" -> addAmountToBalance date posting.Amount balances
        | _ -> balances)


/// Returns the total multi-currency balance for each day.
let absoluteTotalBalanceHistory
    (totalBalanceHistory: BalanceHistory)
    : BalanceHistory =
    let folder
        ((_, currentTotalBalance): BalanceOnDate)
        ((date, totalBalanceChangeForDate): BalanceOnDate)
        : BalanceOnDate =
        (date, currentTotalBalance + totalBalanceChangeForDate)

    let emptyBalance = (Date.MinValue, MultiCommodityBalance.Empty)

    totalBalanceHistory
    |> List.scan folder emptyBalance
    // we don't want the first entry, which is the empty balance
    |> List.tail


/// Converts the multi-commodity balance history to a single-commodity
/// balance history.
let toSingleCommodityBalanceHistory
    marketPrices
    commodity
    (multiCommodityBalanceHistory: BalanceHistory)
    =
    multiCommodityBalanceHistory
    |> List.map (fun (date, multiCommodityBalance) ->
        (date,
         convertToSingleCommodity
             marketPrices
             commodity
             date
             multiCommodityBalance))

/// Lists transactions involving a specific account.
let listAccountTransactions accountName ledger =
    ledger.Transactions
    |> List.filter (fun tx ->
        tx.Postings
        |> List.exists (fun posting -> posting.Account.FullName = accountName))


/// Fills the in-between days of the balance history that do not contain any
/// transactions. This function is useful to calculate running averages.
let fullDatesBalanceHistory (balanceHistory: BalanceHistory) : BalanceHistory =
    match balanceHistory with
    | [] -> []
    | _ ->
        let balancesExceptLast =
            balanceHistory
            |> List.pairwise
            |> List.map (fun ((dateBefore, balanceBefore), (dateAfter, _)) ->
                let daysBetween = (dateAfter.Date - dateBefore.Date).Days

                let dates = Seq.init daysBetween dateBefore.AddDays

                (dates
                 |> Seq.map (fun date -> (date, balanceBefore))
                 |> Seq.toList))
            |> List.concat

        balancesExceptLast @ [ balanceHistory |> List.last ]


/// Calculates the moving averages of the balance history.
let balanceHistoryMovingAverage
    (movingAverageDays: int)
    (balanceHistory: BalanceHistory)
    : BalanceHistory =

    if movingAverageDays > 0 then
        let daysBefore = movingAverageDays / 2

        balanceHistory
        |> List.windowed movingAverageDays
        |> List.map (fun window ->
            let dateOfAverage, _ = window[daysBefore]

            let averageBalance =
                window
                |> List.map snd
                |> List.fold (+) MultiCommodityBalance.Empty
                |> divideMultiCommodityBalance movingAverageDays

            (dateOfAverage, averageBalance))
    else
        failwith "movingAverageDays must be positive"

/// Calculates the moving averages of the single-commodity balance history.
let commodityBalanceHistoryMovingAverage
    (movingAverageDays: int)
    (balanceHistory: CommodityBalanceHistory)
    : CommodityBalanceHistory =

    if movingAverageDays > 0 then
        let commodity = (balanceHistory[0] |> snd).Commodity

        let daysBefore = movingAverageDays / 2

        balanceHistory
        |> List.windowed movingAverageDays
        |> List.map (fun window ->
            let dateOfAverage, _ = window[daysBefore]

            let averageBalance: Amount =
                window
                |> List.map snd
                |> List.fold (+) (Amount.Zero commodity)
                |> (fun amount -> amount / movingAverageDays)

            (dateOfAverage, averageBalance))
    else
        failwith "movingAverageDays must be positive"


let dailyIncome =
    calculateBalanceChangeHistory (fun balances transaction posting ->
        let date = transaction.Date

        match posting.Account.NameParts[0] with
        | "income" -> addAmountToBalance date posting.Amount balances
        | _ -> balances)


let dailyExpenses =
    calculateBalanceChangeHistory (fun balances transaction posting ->
        let date = transaction.Date

        match posting.Account.NameParts[0] with
        | "expenses" -> addAmountToBalance date posting.Amount balances
        | _ -> balances)

type PostingRouter = Posting -> int option

let balancesChangeHistories
    (postingRouter: PostingRouter)
    (slotsCount: int)
    (ledger: Ledger)
    : BalanceHistory[] =

    let routePosting
        (date: Date)
        (balancesHistories: BalanceByDate[])
        (posting: Posting)
        : BalanceByDate[] =
        postingRouter posting
        |> function
            | Some slotIndex ->
                let balanceHistory = balancesHistories[slotIndex]

                let newBalance =
                    addAmountToBalance date posting.Amount balanceHistory

                Array.set balancesHistories slotIndex newBalance
                balancesHistories
            | None -> balancesHistories

    let processTx
        (balances: BalanceByDate[])
        (transaction: Transaction)
        : BalanceByDate[] =
        transaction.Postings
        |> List.fold (routePosting transaction.Date) balances

    let initialState: BalanceByDate[] =
        Array.init slotsCount (fun _ -> Map.empty)

    ledger.Transactions
    |> List.fold processTx initialState
    |> Array.map Map.toSeq
    |> Array.map (Seq.sortBy fst)
    |> Array.map Seq.toList
