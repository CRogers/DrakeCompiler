module LLVMTypes

open LLVM.Generated.Core

let i1 = int1Type ()
let i8 = int8Type ()
let i16 = int16Type ()
let i32 = int32Type ()
let i64 = int64Type ()
let tyVoid = voidType ()