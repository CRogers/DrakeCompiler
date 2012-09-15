module LLVM.Extras

open LLVM.Core
open LLVM.Generated.Core
open LLVM.Generated.ExecutionEngine

let createJITCompilerForModule (_M:ModuleRef) _OptLevel =
    let outJitPtr = nativeint 0
    let outErrorPtr = nativeint 0
    createJITCompilerForModuleNative(outJitPtr, _M.Ptr, _OptLevel, outErrorPtr) |> ignore
    new ExecutionEngineRef(outJitPtr)

