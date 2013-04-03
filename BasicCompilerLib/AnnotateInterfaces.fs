module AnnotateInterfaces

(*

Work out interface inheritance/check there are no cycles etc

*)

open Tree
open System.Collections.Generic

let annotateInterfaces (globals:GlobalStore) (program:list<NDA>) =

    ///////////
    let setImplementedBy (nA:NDA) =
        for iface in nA.Interfaces do
            iface.ImplementedBy <- nA :: iface.ImplementedBy

    //////////
    let checkNotImplementingSelf (nA:NDA) =
        if Seq.exists (fun (ifaceA:NDA) -> ifaceA.QName = nA.QName) nA.Interfaces then
            failwithf "Interface %s cannot implement itself!" nA.QName

    //////////
    let checkNotImplementingClass (nA:NDA) =
        for ifaceA in nA.Interfaces do
            if ifaceA.IsClass then
                failwithf "%s inherits %s, which is a class!" nA.QName ifaceA.QName

    //////////
    let checkDuplicateInterfaces (nA:NDA) =
        let dups = List.ofSeq <| Util.findDuplicates (fun (nA:NDA) -> nA.QName) nA.Interfaces

        if dups.Length > 0 then
            Util.joinMap ", " (fun (nAList:list<NDA>) -> (List.head nAList).QName) dups
            |> failwithf "Duplicate interfaces for %s: %s" nA.QName

    /////////
    let detectIfaceLoop (program:list<NDA>) =
        let SCCs = Util.stronglyConnectedComponents (fun (nA:NDA) -> Seq.ofList nA.Interfaces) program
        let badSCCs = Seq.filter (fun (hs:HashSet<_>) -> hs.Count > 1) SCCs

        if Seq.length badSCCs > 0 then
            let errors = seq { for scc in badSCCs do
                yield "Loop detected in interface inheritance hierarchy: " + System.String.Join(", ", Seq.map (fun (nA:NDA) -> nA.QName) scc) }
            failwithf "%s" <| System.String.Join("\n", errors)

        ()

    //////////
    let rec setAllNAsForIfaces (nAs:list<NDA>) =
        seq { for nA in nAs do
            if nA.AllInterfaces = [] then
                nA.AllInterfaces <- nA.Interfaces @ (setAllNAsForIfaces nA.Interfaces |> List.ofSeq)
            yield nA.AllInterfaces }
        |> List.concat
    
    //////////
    let checkForConflicts (nA:NDA) =
        let dups = List.ofSeq <| Util.findDuplicates interfaceNPKey nA.AllInterfaceProcs

        if dups.Length > 0 then
            Util.joinMap ", " (fun (iAList:list<IDA>) ->
                let iA = List.head iAList
                let func = sprintf "%s(%s)" iA.Name <| System.String.Join(", ", paramsToPtypeString iA.Params)
                let from = Util.joinMap ", " (fun (iA:IDA) -> iA.NamespaceDecl.Value.QName) iAList
                sprintf "%s from %s in %s" func from nA.QName) dups
            |> failwithf "Multiple definitions of an interface member function exist: %s"
    
    //////////
    let checkProcsAreImplemented (classNA:NDA) =
        for iA in classNA.AllInterfaceProcs do
            ()




    let classes, ifaces = Util.splitSeq (fun (nA:NDA) -> nA.IsClass) program

    Seq.iter setImplementedBy program
    Seq.iter checkNotImplementingSelf ifaces
    Seq.iter checkNotImplementingClass program
    Seq.iter checkDuplicateInterfaces program
    detectIfaceLoop program
    setAllNAsForIfaces program |> ignore
    Seq.iter checkForConflicts program
    Seq.iter checkProcsAreImplemented classes
