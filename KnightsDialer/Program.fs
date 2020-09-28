open System

let neighbours number =
    match number with
    | 1 -> [ 8; 6 ]
    | 2 -> [ 7; 9 ]
    | 3 -> [ 4; 8 ]
    | 4 -> [ 3; 9; 0 ]
    | 5 -> []
    | 6 -> [ 0; 1; 7 ]
    | 7 -> [ 2; 6 ]
    | 8 -> [ 1; 3 ]
    | 9 -> [ 4; 2 ]
    | 0 -> [ 4; 6 ]
    | _ -> raise (IndexOutOfRangeException())

// Level 1. Recursion
let rec distinctNumbers number hops =
    match hops with
    | 0 -> bigint 1
    | _ -> neighbours number |> List.sumBy (fun neighbour -> distinctNumbers neighbour (hops - 1))

// Level 2. Memoization
let rec distinctNumbersMemo number hops memory =
    match hops with
    | 0 -> (bigint 1, memory)
    | _ ->
        neighbours number
        |> List.fold (fun (sum, memory') neighbour ->
            match Map.tryFind (neighbour, hops) memory' with
            | Some v -> (sum + v, memory')
            | None ->
                let (value, memory'') = distinctNumbersMemo neighbour (hops - 1) memory'
                (sum + value, Map.add (neighbour, hops) value memory'')) (bigint 0, memory)

// Level 3. Dynamic programming
let distinctNumbersDP number hops =
    let next (prior: bigint list) =
        [ 0 .. 9 ] |> List.map (fun x -> neighbours x |> Seq.sumBy (fun neighbour -> prior.[neighbour]))

    let first = List.init 10 (fun _ -> bigint 1)
    let result = [ 1 .. hops ] |> Seq.fold (fun prior _ -> next prior) first
    
    result.[number]

[<EntryPoint>]
let main argv =
    let (totalMemo, _) = distinctNumbersMemo 1 1000 Map.empty<int * int, bigint>
    let totalDp = distinctNumbersDP 1 1000

    printf "Distinct numbers: { memo: %A, dp: %A }" totalMemo totalDp
    0
