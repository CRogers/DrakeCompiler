module Util

let getSomes xs = Seq.filter Option.isSome xs |> Seq.map Option.get


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