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


let intSizeToTy s = match s with
    | 1  -> i1
    | 8  -> i8
    | 16 -> i16
    | 32 -> i32
    | 64 -> i64


let uninitValueRef = new ValueRef(nativeint 0xDED)

let isUninitValueRef (vr:ValueRef) = vr.Ptr.ToInt32() = 0xDED