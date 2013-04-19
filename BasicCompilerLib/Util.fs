module Util

open System.Collections.Generic

let id x = x

let getSomes xs = Seq.filter Option.isSome xs |> Seq.map Option.get

let difference (xs:seq<'a>) (ys:seq<'a>) =
    let set = new HashSet<'a>(ys)
    seq {
        for x in xs do
            if not <| set.Contains x then
                yield x
    }

let intersection (xs:list<'a>) (ys:list<'a>) =
    let set = HashSet<'a>(xs, HashIdentity.Structural)
    ys |> List.filter (fun x -> not (set.Contains x))

let findDuplicates (key:'a -> 'k) (xs:seq<'a>) =
    let map = new Dictionary<'k,list<'a>>()
    for x in xs do
        let k = key x
        if not <| map.ContainsKey k then
            map.Add(k, [x])
        else
            map.[k] <- x :: map.[k]

    seq {
        for kvp in map do
            if kvp.Value.Length > 1 then
                yield kvp.Value
    }
            
let joinMap sep f items =
    System.String.Join(sep, Seq.map f items)

let concatMap f items =
    Seq.map f items
    |> Seq.concat


type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b
    
let either cond item = if cond then Left item else Right item

let isLeft a = match a with
    | Left x -> Some x
    | Right x -> None

let isRight a = match a with
    | Left x -> None
    | Right x -> Some x

let allEithers xs = Seq.map (fun x -> 
    match x with
        | Left x -> x
        | Right x -> x) xs

let lefts xs = Seq.map isLeft xs |> getSomes
let rights xs = Seq.map isRight xs |> getSomes


let splitSeq pred items =
    let es = Seq.map (fun x -> either (pred x) x) items
    (lefts es, rights es)


type SCC<'a>(item: 'a) =
    member x.Item = item
    member val Index = -1 with get, set
    member val Lowlink = -1 with get, set

let stronglyConnectedComponents<'a when 'a:equality> (succ:'a -> seq<'a>) (graph:seq<'a>) : seq<HashSet<'a>> =
    let ns = Seq.map (fun a -> (a,SCC(a))) graph
    let nodes = Seq.map snd ns
    let nodeMap = new Dictionary<'a,SCC<'a>>()
    for k, v in ns do nodeMap.Add(k,v)     
    
    let index = ref 0
    let stack = new Stack<SCC<'a>>()
    let SCCs = new List<HashSet<'a>>()

    let rec strongConnect (node:SCC<'a>) =
        node.Index <- !index
        node.Lowlink <- !index
        index := !index + 1
        stack.Push(node)

        for s in succ node.Item do
            let successor = nodeMap.[s]
            if successor.Index = -1 then
                strongConnect successor
                node.Lowlink <- min node.Lowlink successor.Lowlink
            elif stack.Contains(successor) then
                node.Lowlink <- min node.Lowlink successor.Index

        if node.Lowlink = node.Index then
            let list = new List<'a>()
            let mutable w = stack.Pop()
            list.Add(w.Item)
            while not (w.Item = node.Item) do
                w <- stack.Pop()
                list.Add(w.Item)
            SCCs.Add(new HashSet<'a>(list))


    for node in nodes do
        if node.Index = -1 then
            strongConnect node

    System.Linq.Enumerable.AsEnumerable(SCCs)