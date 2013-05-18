module LLVMFSExtensions

open LLVM.FFIUtil
open LLVM.Generated.Core
open LLVM.Core

let constStruct (constVals:ValueRef array) (packed:bool) : ValueRef =
    use constPtrs = new NativePtrs([|for constVal in constVals -> constVal.Ptr|])
    let valCount = uint32 constVals.Length
    ValueRef(constStructNative(constPtrs.Ptrs, valCount, packed))
let constStructInContext (c:ContextRef) (constVals:ValueRef array) (packed:bool) : ValueRef =
    use constPtrs = new NativePtrs([|for constVal in constVals -> constVal.Ptr|])
    let valCount = uint32 constVals.Length
    ValueRef(constStructInContextNative(c.Ptr, constPtrs.Ptrs, valCount, packed))
let constNamedStruct (structTy:TypeRef) (constVals:ValueRef array) =
    use constPtrs = new NativePtrs([|for constVal in constVals -> constVal.Ptr|])
    let valCount = uint32 constVals.Length
    ValueRef(constNamedStructNative(structTy.Ptr, constPtrs.Ptrs, valCount))

let constGEP (constantVal:ValueRef) (constantIndicies:ValueRef array) =
    use indexPtrs = new NativePtrs([|for i in constantIndicies -> i.Ptr|])
    ValueRef(constGEPNative (constantVal.Ptr, indexPtrs.Ptrs, uint32 constantIndicies.Length))  