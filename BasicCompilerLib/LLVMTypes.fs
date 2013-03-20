module LLVMTypes

open LLVM.Generated.Core

let i1 = int1Type ()
let i8 = int8Type ()
let i16 = int16Type ()
let i32 = int32Type ()
let i64 = int64Type ()

let i1p = pointerType i1 0u
let i8p = pointerType i8 0u
let i16p = pointerType i16 0u
let i32p = pointerType i32 0u
let i64p = pointerType i64 0u

let tyVoid = voidType ()


let genConstInt ty x = constInt ty x false

let i1zero = genConstInt i1 0UL
let i8zero = genConstInt i8 0UL
let i16zero = genConstInt i16 0UL
let i32zero = genConstInt i32 0UL
let i64zero = genConstInt i64 0UL

let i1one = genConstInt i1 1UL
let i8one = genConstInt i8 1UL
let i16one = genConstInt i16 1UL
let i32one = genConstInt i32 1UL
let i64one = genConstInt i64 1UL

let genConstBool b = match b with
    | true -> i1one
    | false -> i1zero

let intSizeToTy s = match s with
    | 1  -> i1
    | 8  -> i8
    | 16 -> i16
    | 32 -> i32
    | 64 -> i64


let uninitValueRef = new ValueRef(nativeint 0xDED)

let isUninitValueRef (vr:ValueRef) = vr.Ptr.ToInt32() = 0xDED