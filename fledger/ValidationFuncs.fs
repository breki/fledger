module fledger.ValidationFuncs

open fledger.Ledger

type ValidationError = { Message: string }

/// Function that validates the ledger and returns the first ValidationError
/// it found, if any.
type ValidationFunc = Ledger -> Result<Ledger, ValidationError>

// todo 18: validateLedger should use all the available validator functions
/// Validates the ledger and returns the first ValidationError it found, if any.
let validateLedger ledger = Ok ledger
