module backend.vm.builder;

@nogc:
extern(C):

import core.stdc.stdio, core.stdc.stdlib, core.stdc.string;
import backend.vm.virtual_machine;

struct CodeBuilder {
@nogc:
    uint* program;
    uint programSize;
    uint programCapacity;
    
    IValue* constants;
    uint constantsSize;
    uint constantsCapacity;
    
    // Label system
    Label* labels;
    uint labelsSize;
    uint labelsCapacity;
    
    // Call patches
    CallPatch* patches;
    uint patchesSize;
    uint patchesCapacity;
    
    struct Label {
        char* name;
        uint address;
    }
    
    struct CallPatch {
        char* labelName;
        uint patchIndex;
    }
    
    this(uint initialCapacity) 
    {
        programCapacity = initialCapacity;
        program = cast(uint*) malloc(uint.sizeof * programCapacity);
        
        constantsCapacity = 64;
        constants = cast(IValue*) malloc(IValue.sizeof * constantsCapacity);
        
        labelsCapacity = 32;
        labels = cast(Label*) malloc(Label.sizeof * labelsCapacity);
        
        patchesCapacity = 32;
        patches = cast(CallPatch*) malloc(CallPatch.sizeof * patchesCapacity);
        
        if (!program || !constants || !labels || !patches) {
            printf("BUILDER ERROR: Memory allocation failed\n");
            exit(1);
        }
    }
    
    void destroy() 
    {
        if (program) free(program);
        if (constants) free(constants);
        
        // Free label names
        for (uint i = 0; i < labelsSize; i++) {
            if (labels[i].name) free(labels[i].name);
        }
        if (labels) free(labels);
        
        // Free patch names
        for (uint i = 0; i < patchesSize; i++) {
            if (patches[i].labelName) free(patches[i].labelName);
        }
        if (patches) free(patches);
    }
    
    // --- Core emit ---
    
    CodeBuilder* emit(uint instruction) 
    {
        if (programSize >= programCapacity) {
            programCapacity *= 2;
            program = cast(uint*) realloc(program, uint.sizeof * programCapacity);
            if (!program) {
                printf("BUILDER ERROR: Program realloc failed\n");
                exit(1);
            }
        }
        program[programSize++] = instruction;
        return &this;
    }
    
    // --- Constants pool ---
    
    ushort addConstant(IValue val) {
        if (constantsSize >= constantsCapacity) 
        {
            constantsCapacity *= 2;
            constants = cast(IValue*) realloc(constants, IValue.sizeof * constantsCapacity);
            if (!constants) {
                printf("BUILDER ERROR: Constants realloc failed\n");
                exit(1);
            }
        }
        constants[constantsSize] = val;
        return cast(ushort) constantsSize++;
    }
    
    ushort constI32(int val) 
    {
        IValue v;
        v.type = VType.I32;
        v.value.i32 = val;
        return addConstant(v);
    }
    
    ushort constI64(long val) 
    {
        IValue v;
        v.type = VType.I64;
        v.value.i64 = val;
        return addConstant(v);
    }
    
    ushort constF64(double val) 
    {
        IValue v;
        v.type = VType.F64;
        v.value.f64 = val;
        return addConstant(v);
    }
    
    ushort constStr(const char* str) 
    {
        IValue v;
        v.type = VType.Str;
        v.value.str = cast(char*) str;
        return addConstant(v);
    }
    
    CodeBuilder* label(const char* name) 
    {
        // Check if exists
        for (uint i = 0; i < labelsSize; i++) {
            if (strcmp(labels[i].name, name) == 0) {
                printf("BUILDER ERROR: Label '%s' already exists\n", name);
                exit(1);
            }
        }
        
        if (labelsSize >= labelsCapacity) 
        {
            labelsCapacity *= 2;
            labels = cast(Label*) realloc(labels, Label.sizeof * labelsCapacity);
            if (!labels) {
                printf("BUILDER ERROR: Labels realloc failed\n");
                exit(1);
            }
        }
        
        // Duplicate name
        size_t len = strlen(name);
        char* nameCopy = cast(char*) malloc(len + 1);
        strcpy(nameCopy, name);
        
        labels[labelsSize].name = nameCopy;
        labels[labelsSize].address = programSize;
        labelsSize++;
        
        return &this;
    }
    
    uint getLabelAddress(const char* name) 
    {
        for (uint i = 0; i < labelsSize; i++)
            if (strcmp(labels[i].name, name) == 0) 
                return labels[i].address;
        printf("BUILDER ERROR: Undefined label '%s'\n", name);
        exit(1);
    }
    
    void addPatch(const char* labelName, uint patchIndex) 
    {
        if (patchesSize >= patchesCapacity) 
        {
            patchesCapacity *= 2;
            patches = cast(CallPatch*) realloc(patches, CallPatch.sizeof * patchesCapacity);
            if (!patches) {
                printf("BUILDER ERROR: Patches realloc failed\n");
                exit(1);
            }
        }
        
        // Duplicate name
        size_t len = strlen(labelName);
        char* nameCopy = cast(char*) malloc(len + 1);
        strcpy(nameCopy, labelName);
        
        patches[patchesSize].labelName = nameCopy;
        patches[patchesSize].patchIndex = patchIndex;
        patchesSize++;
    }
    
    // --- Instruction builders ---
    
    CodeBuilder* push(ushort constIdx) 
    {
        return emit(encode(IKind.Push, constIdx));
    }
    
    CodeBuilder* pushI32(int val) 
    {
        ushort idx = constI32(val);
        return push(idx);
    }
    
    CodeBuilder* pushF64(double val) 
    {
        ushort idx = constF64(val);
        return push(idx);
    }
    
    CodeBuilder* pushStr(const char* str) 
    {
        ushort idx = constStr(str);
        return push(idx);
    }
    
    CodeBuilder* pop() 
    {
        return emit(encode(IKind.Pop));
    }
    
    CodeBuilder* dup() 
    {
        return emit(encode(IKind.Dup));
    }
    
    // Arithmetic
    CodeBuilder* add() 
    {
        return emit(encode(IKind.Add));
    }
    
    CodeBuilder* sub() 
    {
        return emit(encode(IKind.Sub));
    }
    
    CodeBuilder* mul() 
    {
        return emit(encode(IKind.Mul));
    }
    
    CodeBuilder* div() 
    {
        return emit(encode(IKind.Div));
    }
    
    CodeBuilder* mod() 
    {
        return emit(encode(IKind.Mod));
    }
    
    // Comparison
    CodeBuilder* eq() 
    {
        return emit(encode(IKind.Eq));
    }
    
    CodeBuilder* neq() 
    {
        return emit(encode(IKind.Neq));
    }
    
    CodeBuilder* lt() 
    {
        return emit(encode(IKind.Lt));
    }
    
    CodeBuilder* gt() 
    {
        return emit(encode(IKind.Gt));
    }
    
    CodeBuilder* le() 
    {
        return emit(encode(IKind.Le));
    }
    
    CodeBuilder* ge() 
    {
        return emit(encode(IKind.Ge));
    }
    
    // Locals
    CodeBuilder* loadLocal(ubyte localIdx) 
    {
        return emit(encode(IKind.LoadL, localIdx));
    }
    
    CodeBuilder* storeLocal(ubyte localIdx) 
    {
        return emit(encode(IKind.StoreL, localIdx));
    }
    
    // Control flow
    CodeBuilder* jmp(uint targetPc) 
    {
        return emit(encode(IKind.Jmp, targetPc));
    }
    
    CodeBuilder* jmpLabel(const char* labelName) 
    {
        addPatch(labelName, programSize);
        return emit(encode(IKind.Jmp, 0)); // Placeholder
    }
    
    CodeBuilder* jz(uint targetPc) 
    {
        return emit(encode(IKind.Jz, targetPc));
    }
    
    CodeBuilder* jzLabel(const char* labelName) 
    {
        addPatch(labelName, programSize);
        return emit(encode(IKind.Jz, 0)); // Placeholder
    }
    
    CodeBuilder* jnz(uint targetPc) 
    {
        return emit(encode(IKind.Jnz, targetPc));
    }
    
    CodeBuilder* jnzLabel(const char* labelName) 
    {
        addPatch(labelName, programSize);
        return emit(encode(IKind.Jnz, 0)); // Placeholder
    }
    
    // Functions
    CodeBuilder* call(ushort targetPc, ubyte numLocals) 
    {
        return emit(encodeCall(targetPc, numLocals));
    }
    
    CodeBuilder* callLabel(const char* labelName, ubyte numLocals) 
    {
        addPatch(labelName, programSize);
        return emit(encodeCall(0, numLocals)); // Placeholder targetPc
    }
    
    CodeBuilder* ret() 
    {
        return emit(encode(IKind.Ret));
    }
    
    // I/O
    CodeBuilder* print() 
    {
        return emit(encode(IKind.Print));
    }
    
    CodeBuilder* halt() 
    {
        return emit(encode(IKind.Halt));
    }
    
    // --- Build & patch ---
    
    void build() {
        // Patch all jumps and calls
        for (uint i = 0; i < patchesSize; i++) 
        {
            uint addr = getLabelAddress(patches[i].labelName);
            uint patchIdx = patches[i].patchIndex;
            
            uint instr = program[patchIdx];
            ubyte opcode = instr & 0xFF;
            
            // Reconstruct instruction with correct address
            if (opcode == IKind.Jmp || opcode == IKind.Jz || opcode == IKind.Jnz)
                program[patchIdx] = encode(opcode, addr);
            else if (opcode == IKind.Call) 
            {
                ubyte numLocals = (instr >> 24) & 0xFF;
                program[patchIdx] = encodeCall(cast(ushort)addr, numLocals);
            }
        }
    }
    
    VM buildVM(uint startPc = 0) 
    {
        build();
        return VM(program, programSize, constants, constantsSize, startPc, false);
    }
}
