module Polymorphism

open Tree


let canCastTo (fromNA:NDA) toNA =
    Seq.exists (fun nA -> nA = toNA) fromNA.AllInterfaces


let getBestOverload nAname (cirefs:Map<NPKey,'a>) (name:string) (argTypeNAs:list<NDA>) =

    // Format method as a string for error reporting
    let fmtMethod (args:seq<string>) = sprintf "%s(%s)" name <| System.String.Join(", ", args)

    // Get all procs which have the same name, length and number of type params
    let procArgsSeq = 
        cirefs
        |> Map.filter (fun k v -> match k with ProcKey (n, args, nTPs) -> n = name && args.Length = argTypeNAs.Length && nTPs = 0 | _ -> false)
        |> Map.toSeq
        |> Seq.map (fun (ProcKey (_, args, _), _) -> args)

    // If there are no args with that name, error
    if Seq.length procArgsSeq = 0 then
        failwithf "No method %s could be found in %s" name nAname

    // Score 1 if the arg is an interface
    // Score 2 if the arg is the exact object
    // score -65536 if wrong
    let results = Seq.groupBy fst <| seq {
        for procArgs in procArgsSeq do
            let score = 
                seq {
                    for (testNA, Type procArgNA) in Seq.zip argTypeNAs procArgs do
                        yield (if procArgNA = testNA then Some 2
                        elif List.exists (fun (iface:NDA) -> iface = procArgNA) testNA.AllInterfaces then Some 1
                        else None)
                }
                |> Seq.fold (fun state xo -> match state with
                    | None -> None
                    | Some sum -> match xo with 
                        | None -> None
                        | Some x -> Some (sum + x)) (Some 0)

            match score with
                | None -> ()
                | Some sc -> yield (sc, procArgs)
    }

    // No results mean no possibles with matching arg types
    if Seq.length results = 0 then
        failwithf "No method %s with correct args could be found in %s" name nAname

    let bestResult =
        results
        |> Seq.maxBy fst
        |> snd
        |> Seq.map snd
        |> List.ofSeq

    let argTypeStrs = seq { for argNA in argTypeNAs do yield argNA.QName }

    // Check that any of things match
    if bestResult.Length = 0 then
        failwithf "Cannot find any method in %s that matches the signature %s" nAname (fmtMethod argTypeStrs)

    // Check to see that there are 2 methods which score the same
    if bestResult.Length > 1 then
        let methods = Util.joinMap ", " fmtMethod <| Seq.map (Seq.map userTypeToString) bestResult
        failwithf "Cannot choose between methods %s for %s in %s" methods (fmtMethod argTypeStrs) nAname

    let bestSignature = Seq.head bestResult
    let bestKey = ProcKey (name, bestSignature, 0)

    Map.find bestKey cirefs

let getBestOverloadNA (nA:NDA) dotName argTypeNAs = getBestOverload nA.QName nA.Refs dotName argTypeNAs