module AnnotateInterfaces

(*

Work out interface inheritance/check there are no cycles etc

*)

open Tree
open System.Collections.Generic

let annotateInterfaces (globals:GlobalStore) (program:list<NamespaceDeclA>) =

    ///////////
    let setNAsForIfaces (nA:NamespaceDeclA) =
        nA.Interfaces <- List.map (fun name ->
            match getGlobal globals nA.Namespace nA.Usings name with
                | Some iface ->
                    iface.ImplementedBy <- nA :: iface.ImplementedBy
                    iface
                | None -> failwithf "Can't find interface %s to inherit from" name) nA.ImplementingInterfaces

    //////////
    let detectIfaceLoop (program:list<NamespaceDeclA>) =
        let SCCs = Util.stronglyConnectedComponents (fun (nA:NamespaceDeclA) -> Seq.ofList nA.Interfaces) program
        let badSCCs = Seq.filter (fun (hs:HashSet<_>) -> hs.Count > 1) SCCs

        if Seq.length badSCCs > 0 then
            let errors = seq { for scc in badSCCs do
                yield "Loop detected in interface inheritance hierarchy: " + System.String.Join(", ", Seq.map (fun (nA:NamespaceDeclA) -> nA.QName) scc) }
            failwithf "%s" <| System.String.Join("\n", errors)

        ()

    /////////
    let rec setAllNAsForIfaces (nAs:list<NamespaceDeclA>) =
        seq { for nA in nAs do
            if not (nA.AllInterfaces = []) then
                nA.AllInterfaces <- nA.Interfaces @ (setAllNAsForIfaces nA.Interfaces |> List.ofSeq)
            yield nA.AllInterfaces }
        |> List.concat
    

    Seq.iter setNAsForIfaces program
    detectIfaceLoop program
    setAllNAsForIfaces program |> ignore

