module backend.llvm.codegen_mod;

import backend.llvm.llvm;
import middle.mir.mir;
import frontend.types.type;
import std.string : toStringz;
import std.stdio;

mixin template CodeGenModule() {
    void emitProgram(MirProgram prog)
    {
        foreach (func; prog.functions)
            declareFunction(func);
            
        foreach (func; prog.functions)
            emitFunction(func);
    }

    private void declareFunction(MirFunction func) 
    {
        LLVMValueRef llvmFunc = LLVMGetNamedFunction(module_, toStringz(func.name));
        LLVMTypeRef retType = toLLVMType(func.returnType);
        LLVMTypeRef[] paramTypes;

        foreach (i, paramType; func.paramTypes) 
        {
            if (paramType is null) 
                break;
            paramTypes ~= toLLVMType(paramType);
        }

        LLVMTypeRef funcTy = LLVMFunctionType(
            retType, 
            paramTypes.ptr, 
            cast(uint)paramTypes.length, 
            (func.isVarArg && !func.isOrnVarArgs) ? 1 : 0
        );

        if (llvmFunc is null)
            llvmFunc = LLVMAddFunction(module_, toStringz(func.name), funcTy);

        funcMap[func.name] = llvmFunc;
        funcTypeMap[func.name] = funcTy;

        if (LLVMCountParams(llvmFunc) == paramTypes.length)
        {
            for (int i = 0; i < paramTypes.length; i++)
                if ((func.isVarArg && !func.isOrnVarArgs) && i == func.isVarArgAt)
                    LLVMSetValueName2(LLVMGetParam(llvmFunc, i), "_vacount", 8);
        }
    }
}
