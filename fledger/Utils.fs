namespace fledger

[<RequireQualifiedAccess>]
module Map =
    /// Returns a union of two maps, using the specified function to
    /// resolve conflicts.
    let union
        resolveConflict
        (source1: Map<'Key, 'Value>)
        (source2: Map<'Key, 'Value>)
        =
        Map.fold
            (fun m k v' ->
                Map.add
                    k
                    (match Map.tryFind k m with
                     | Some v -> resolveConflict v v'
                     | None -> v')
                    m)
            source1
            source2
