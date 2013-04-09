module Polymorphism

open Tree

let getBestOverload (nA:NDA) (name:string) (argTypeNAs:list<NDA>) =

    // Get all procs which have the same name
    let procArgsSeq = 
        nA.Refs
        |> Map.filter (fun k v -> match k with ProcKey (n, args) -> n = name && args.Length = argTypeNAs.Length | _ -> false)
        |> Map.toSeq
        |> Seq.map (fun (ProcKey (_, args), _) -> args)

    // Score 1 if the arg is an interface
    // Score 2 if the arg is the exact object
    // score -65536 if wrong
    let results = Seq.groupBy snd <| seq {
        for procArgs in procArgsSeq do
            yield procArgs, seq {
                for (testNA, procArg) in Seq.zip argTypeNAs procArgs do
                    yield (if procArg = testNA.QName then 2
                    elif List.exists (fun (iface:NDA) -> iface.QName = procArg) testNA.AllInterfaces then 1
                    else -65536)                
            }
            |> Seq.sum
    }

    let bestResult =
        results
        |> Seq.maxBy fst
        |> snd
        |> Seq.map fst
        |> List.ofSeq

    // Format method as a string for error reporting
    let fmtMethod (args:seq<string>) = sprintf "%s(%s)" name <| System.String.Join(", ", args)

    let argTypeStrs = seq { for argNA in argTypeNAs do yield argNA.QName }

    // Check that any of things match
    if bestResult.Length = 0 then
        failwithf "Cannot find any method in %s that matches the signature %s" nA.QName (fmtMethod argTypeStrs)

    // Check to see that there are 2 methods which score the same
    if bestResult.Length > 1 then
        let methods = Util.joinMap ", " fmtMethod bestResult
        failwithf "Cannot choose between methods %s for %s in %s" methods (fmtMethod argTypeStrs) nA.QName

    let bestSignature = Seq.head bestResult
    let bestKey = ProcKey (name, bestSignature)

    match nA.GetRef(bestKey) with Some ciref -> ciref