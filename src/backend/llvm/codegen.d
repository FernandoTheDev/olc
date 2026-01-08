module backend.llvm.codegen;

import backend.llvm.llvm;
import middle.mir.mir, target, middle.hir.hir;
import frontend.types.type;
import std.string : toStringz;
import std.stdio, std.conv, std.format;

import backend.llvm.codegen_mod;
import backend.llvm.codegen_func;
import backend.llvm.codegen_instr; 

class LLVMBackend 
{
    LLVMContextRef context;
    LLVMModuleRef module_;
    LLVMBuilderRef builder;
    LLVMTargetDataRef targetData;
    LLVMValueRef currentFuncVal;

    // Estado Local da Função Atual
    LLVMValueRef[] vregMap;
    LLVMBasicBlockRef[string] blockMap; // Mapa de Label -> LLVM Block
    LLVMTypeRef[string] structTypes;
    LLVMTypeRef[string] unionTypes;
    LLVMValueRef[string] funcMap;
    LLVMTypeRef[string] funcTypeMap;
    LLVMValueRef[string] valueMap;

    mixin CodeGenModule;
    mixin CodeGenFunc;
    mixin CodeGenInstr;

    this() 
    {
        context = LLVMContextCreate();
        module_ = LLVMModuleCreateWithName(toStringz("x_module"));
        builder = LLVMCreateBuilderInContext(context);
        
        if (this.context is null || this.module_ is null || this.builder is null)
        {
            printf("ERRO: Falha ao inicializar componentes LLVM\n");
            return;
        }
    }

    ~this() 
    {
        if (this.builder !is null)
            LLVMDisposeBuilder(this.builder);
        
        if (this.module_ !is null)
            LLVMDisposeModule(this.module_);
        
        if (this.context !is null)
            LLVMContextDispose(this.context);
    }

    void generate(MirProgram prog, HirProgram hir)
    {
        foreach(g; hir.globals)
            if (HirStructDecl s = cast(HirStructDecl)g)
                structTypes[s.name] = LLVMStructCreateNamed(context, toStringz(s.name));
            else if (HirUnionDecl s = cast(HirUnionDecl)g)
                unionTypes[s.name] = LLVMStructCreateNamed(context, toStringz(s.name));
        
        foreach(g; prog.globals)
        {
            LLVMTypeRef type = toLLVMType(g.type);
            
            // Adiciona a global ao módulo
            LLVMValueRef gVar = LLVMAddGlobal(module_, type, toStringz(g.name));
            
            // Inicialização
            if (g.initVal.type !is null) // Tem valor inicial?
            {
                // Precisamos converter o MirValue em LLVMValueRef constante
                LLVMValueRef initConst = getLLVMValue(g.initVal);
                LLVMSetInitializer(gVar, initConst);
            }
            else
                // Inicializa com Zero (padrão C)
                LLVMSetInitializer(gVar, LLVMConstNull(type));
            
            // Registra no mapa de valores para que funções possam usar
            // Importante: O valor no mapa é o PONTEIRO para a global (@x)
            valueMap[g.name] = gVar; 
        }

        foreach(g; hir.globals) {
            if (auto s = cast(HirStructDecl)g) 
            {
                LLVMTypeRef[] fields;
                foreach(ft; s.fieldTypes) fields ~= toLLVMType(ft);
                LLVMStructSetBody(structTypes[s.name], fields.ptr, cast(uint)fields.length, 0);
            } 
            // else if (auto u = cast(HirUnionDecl)g)
            // {
            //     // Estratégia para Union no LLVM:
            //     // Criamos uma struct com UM ÚNICO campo, que é o maior membro da union.
            //     // Isso garante o tamanho correto de alocação.
                
            //     LLVMTypeRef largestMemberType = null;
            //     ulong maxSize = 0;

            //     foreach(ft; u.fieldTypes)
            //     {
            //         LLVMTypeRef lType = toLLVMType(ft);
                    
            //         // Se temos TargetData, calculamos o tamanho real
            //         ulong size = 0;
            //         if (targetData !is null)
            //             size = LLVMStoreSizeOfType(targetData, lType);
            //         else
            //             // Fallback sem TargetData (assumimos primitiva basica)
            //             size = 1; 

            //         if (size >= maxSize)
            //         {
            //             maxSize = size;
            //             largestMemberType = lType;
            //         }
            //     }

            //     if (largestMemberType is null)
            //         largestMemberType = LLVMInt8TypeInContext(context); // Fallback para union vazia

            //     LLVMTypeRef[] body_ = [largestMemberType];
            //     LLVMStructSetBody(unionTypes[u.name], body_.ptr, 1, 0); // Packed = 0
            // }
            else if (auto u = cast(HirUnionDecl)g)
            {
                // ESTRATÉGIA CORRETA (Opaque Storage):
                // Geramos a Union como uma struct contendo um único array de bytes: { [MaxSize x i8] }
                // Isso força o LLVM a alocar o espaço correto sem tentar alinhar campos internos incorretamente.
                
                ulong maxSize = 0;
                // Alinhamento é importante: a union deve ter o alinhamento do seu membro mais restritivo.
                // Porém, no LLVM, structs "packed" ou arrays de i8 geralmente alinham em 1.
                // O ideal é confiar no frontend para ter calculado o tamanho já alinhado (padding final).

                foreach(ft; u.fieldTypes)
                {
                    LLVMTypeRef lType = toLLVMType(ft);
                    ulong size = 0;
                    
                    if (targetData !is null)
                        size = LLVMStoreSizeOfType(targetData, lType);
                    else
                        size = 8; // Fallback seguro se targetData não estiver pronto

                    if (size > maxSize) maxSize = size;
                }

                if (maxSize == 0) maxSize = 1; // Evita struct vazia

                // Cria o array [MaxSize x i8]
                LLVMTypeRef byteType = LLVMInt8TypeInContext(context);
                LLVMTypeRef storageType = LLVMArrayType(byteType, cast(uint)maxSize);
                
                LLVMTypeRef[] body_ = [storageType];
                
                // Define o corpo da struct da Union. 
                // packed=false permite que o LLVM aplique alinhamento natural se necessário no wrapper.
                LLVMStructSetBody(unionTypes[u.name], body_.ptr, 1, false); 
            }
        }

        emitProgram(prog);
    }

    void generate(MirProgram prog) 
    {
        emitProgram(prog);
    }

    void dumpToFile(string filename) 
    {
        char* error = null;
        if (LLVMPrintModuleToFile(module_, toStringz(filename), &error) != 0)
            writeln("Error LLVM: ", error);
    }

    LLVMTypeRef toLLVMFunctionType(FunctionType ft) 
    {
        LLVMTypeRef retType = toLLVMType(ft.returnType);
        LLVMTypeRef[] paramTypes;
        
        foreach(argT; ft.paramTypes)
            paramTypes ~= toLLVMType(argT);
            
        return LLVMFunctionType(retType, paramTypes.ptr, cast(uint)paramTypes.length, 0);
    }

    LLVMTypeRef toLLVMType(Type t)
    {
        if (t is null) 
        {
            writeln("CRITICAL TYPE ERROR: MirValue passed to the backend has a null type!");
            // Retorna um tipo inofensivo ou um ponteiro de byte para continuar
            return LLVMInt8TypeInContext(context);
        }
        
        if (auto prim = cast(PrimitiveType) t) 
        switch(prim.baseType) 
        {
            case BaseType.Void:   return LLVMVoidTypeInContext(context);
            case BaseType.Bool:   return LLVMInt1TypeInContext(context);
            
            // Tipos de 8 bits
            case BaseType.Char:   return LLVMInt8TypeInContext(context);
            case BaseType.Byte:   return LLVMInt8TypeInContext(context);
            case BaseType.Ubyte:  return LLVMInt8TypeInContext(context);
            
            // Tipos de 16 bits
            case BaseType.Short:  return LLVMInt16TypeInContext(context);
            case BaseType.Ushort: return LLVMInt16TypeInContext(context);
            
            // Tipos de 32 bits
            case BaseType.Int:    return LLVMInt32TypeInContext(context);
            case BaseType.Uint:   return LLVMInt32TypeInContext(context);
            
            // Tipos de 64 bits
            case BaseType.Long:   return LLVMInt64TypeInContext(context);
            case BaseType.Ulong:  return LLVMInt64TypeInContext(context);
            
            // Tipos de ponto flutuante
            case BaseType.Float:  return LLVMFloatTypeInContext(context);
            case BaseType.Double: return LLVMDoubleTypeInContext(context);
            
            // String como ponteiro
            case BaseType.String: return LLVMPointerTypeInContext(context, 0);
            
            default: return LLVMVoidTypeInContext(context);
        }
        
        if (auto structTy = cast(StructType) t) 
        {
            // Se já registramos a struct, retorne o tipo LLVM
            if (structTy.mangledName in structTypes)
                return structTypes[structTy.mangledName];
            writeln("Critical Error: Struct ", structTy.mangledName, " was not pre-declared in the backend!");
            return LLVMVoidTypeInContext(context);
        }
        
        if (UnionType un = cast(UnionType) t) 
        {
            if (un.mangledName in unionTypes)
                return unionTypes[un.mangledName];
            writeln("Critical Error: Union ", un.name, " was not pre-declared in the backend!");
            return LLVMVoidTypeInContext(context);
        }

        if (ArrayType arr = cast(ArrayType) t) {
            // Converte o tipo do elemento (ex: int -> i32)
            LLVMTypeRef elemType = toLLVMType(arr.elementType);
            // Cria o tipo array do LLVM [N x T]
            // Nota: O teu ArrayType precisa ter um campo 'length' ou 'dim' conhecido
            return LLVMArrayType(elemType, cast(uint) arr.length); 
        }
        
        if (cast(PointerType) t)
            return LLVMPointerTypeInContext(context, 0); // Opaque Ptr
        
        if (cast(FunctionType) t)
            return LLVMPointerTypeInContext(context, 0); 

        if (cast(EnumType) t)
            return LLVMInt32TypeInContext(context); 
        
        return LLVMVoidTypeInContext(context);
    }

    LLVMValueRef getLLVMValue(MirValue val)
    {
        if (val.isArgument)
            return LLVMGetParam(currentFuncVal, val.argIndex);
        
        if (val.isArrayLiteral) {
            LLVMValueRef[] consts;
            foreach(elem; val.elements) consts ~= getLLVMValue(elem);

            // Pega o tipo do elemento do array
            auto arrType = cast(ArrayType) val.type;
            auto elemType = toLLVMType(arrType.elementType);

            return LLVMConstArray2(elemType, consts.ptr, cast(uint)consts.length);
        }

        if (val.isGlobal)
        {
            // Se for global, pegamos diretamente do módulo pelo nome
            // (Ou usamos um mapa global se preferir, mas LLVMGetNamedGlobal funciona)
            LLVMValueRef glob = LLVMGetNamedGlobal(module_, toStringz(val.constStr));
            return glob;
        }

        if (val.isConst) 
        {
            if (val.isBlockLabel) return null; // Labels não são Values

            // Null pointer check - DEVE VIR PRIMEIRO!
            if (auto ptrType = cast(PointerType)val.type)
                if (val.constInt == 0)
                    return LLVMConstNull(toLLVMType(val.type));

            LLVMTypeRef ty = toLLVMType(val.type);

            if (cast(FunctionType) val.type)
            {
                string funcName = val.constStr;

                if (funcName in funcMap)
                    return funcMap[funcName];
    
                if (val.regIndex < vregMap.length && vregMap[val.regIndex] !is null)
                    return vregMap[val.regIndex];
    
                return LLVMConstNull(LLVMPointerTypeInContext(context, 0));
            }

            // Verifica tipo primitivo
            if (auto prim = cast(PrimitiveType)val.type) 
            {
                switch (prim.baseType) 
                {
                    case BaseType.String:
                        if (LLVMGetInsertBlock(builder) is null) 
                        {
                            auto strData = LLVMConstStringInContext(
                                context, 
                                toStringz(val.constStr), 
                                cast(uint)val.constStr.length, 
                                1 // null terminate
                            );

                            auto globalStr = LLVMAddGlobal(
                                module_, 
                                LLVMTypeOf(strData), 
                                toStringz(".str")
                            );
                            LLVMSetInitializer(globalStr, strData);
                            LLVMSetLinkage(globalStr, LLVMLinkage.LLVMPrivateLinkage);
                            LLVMSetGlobalConstant(globalStr, 1);

                            LLVMValueRef[2] indices = [
                                LLVMConstInt(LLVMInt32TypeInContext(context), 0, 0),
                                LLVMConstInt(LLVMInt32TypeInContext(context), 0, 0)
                            ];
                            return LLVMConstGEP2(
                                LLVMTypeOf(strData),
                                globalStr, 
                                indices.ptr, 
                                2
                            );
                        }
                        else
                            return LLVMBuildGlobalStringPtr(builder, toStringz(val.constStr), "str_lit");

                    case BaseType.Float:
                    case BaseType.Double:
                        return LLVMConstReal(ty, val.constFloat);

                    case BaseType.Bool:
                        return LLVMConstInt(LLVMInt1TypeInContext(context), val.constInt, 0);

                    case BaseType.Byte:
                    case BaseType.Short:
                    case BaseType.Int:
                    case BaseType.Long:
                    case BaseType.Char:
                        return LLVMConstInt(ty, val.constInt, 1); // signExtend = 1

                    case BaseType.Ubyte:
                    case BaseType.Ushort:
                    case BaseType.Uint:
                    case BaseType.Ulong:
                    return LLVMConstInt(ty, val.constInt, 0); // signExtend = 0

                    default:
                        break;
                }
            }

            // Fallback: constante inteira genérica
            return LLVMConstInt(ty, val.constInt, 0);
        }

        // writeln(val.regIndex, " ",vregMap);

        if (val.regIndex >= vregMap.length) 
            throw new Exception("Invalid register in the backend.");

        return vregMap[val.regIndex];
    }

    void setReg(MirValue dest, LLVMValueRef val) 
    {
        if (dest.regIndex >= vregMap.length) vregMap.length = dest.regIndex + 1;
        vregMap[dest.regIndex] = val;
    }

    void initializeTargetForPlatform(string[] targetTypes)
    {
        foreach (targetType; targetTypes)
        {
            switch (targetType)
            {
            case "X86":
                LLVMInitializeX86Target();
                LLVMInitializeX86TargetInfo();
                LLVMInitializeX86TargetMC();
                LLVMInitializeX86AsmPrinter();
                LLVMInitializeX86AsmParser();
                break;
            case "AArch64":
                LLVMInitializeAArch64Target();
                LLVMInitializeAArch64TargetInfo();
                LLVMInitializeAArch64TargetMC();
                LLVMInitializeAArch64AsmPrinter();
                LLVMInitializeAArch64AsmParser();
                break;
            case "ARM":
                LLVMInitializeARMTarget();
                LLVMInitializeARMTargetInfo();
                LLVMInitializeARMTargetMC();
                LLVMInitializeARMAsmPrinter();
                LLVMInitializeARMAsmParser();
                break;
            default:
                throw new Exception(format("Unknown target architecture: %s", targetType));
            }
        }
    }

     void initializeTargetData(TargetInfo _target)
     {
        initializeTargetForPlatform(_target.initFunctions);
        const char* targetTriple = _target.triple.ptr;

        LLVMTargetRef target;
        const(char)* errorMessage;
        if (LLVMGetTargetFromTriple(targetTriple, &target, &errorMessage))
        {
            writeln("Error: ", errorMessage.to!string);
            LLVMDisposeMessage(cast(char*) errorMessage);
        }

        auto targetMachine = LLVMCreateTargetMachine(
            target,
            targetTriple,
            "", // CPU
            "", // Features
            LLVMCodeGenOptLevel.LLVMCodeGenLevelDefault,
            LLVMRelocMode.LLVMRelocDefault,
            LLVMCodeModel.LLVMCodeModelDefault
        );

        auto dataLayout = LLVMCreateTargetDataLayout(targetMachine);
        auto dataLayoutStr = LLVMCopyStringRepOfTargetData(dataLayout);

        LLVMSetDataLayout(module_, dataLayoutStr);
        LLVMSetTarget(module_, targetTriple);

        targetData = LLVMCreateTargetData(dataLayoutStr);

        LLVMDisposeMessage(cast(char*) dataLayoutStr);
        LLVMDisposeTargetData(dataLayout);
        LLVMDisposeTargetMachine(targetMachine);
     }
}
