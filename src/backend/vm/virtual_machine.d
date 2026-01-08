module backend.vm.virtual_machine;

// VM Baseada em Stack - Completa com todos os tipos
// Operações empurram e puxam valores da stack
// Sem registradores, apenas stack operations

@nogc:
extern(C):

import core.stdc.stdio, core.stdc.stdlib;
import backend.builder;

// Instruction Set
enum IKind : ubyte {
    // Arithmetic (pop 2, push 1)
    Add    = 0x0,
    Sub    = 0x1,
    Mul    = 0x2,
    Div    = 0x3,
    Mod    = 0x4,
    
    // Comparison (pop 2, push 1 boolean)
    Eq     = 0x5,
    Neq    = 0x6,
    Lt     = 0x7,
    Gt     = 0x8,
    Ge     = 0x9,
    Le     = 0x10,
    
    // Stack operations
    Push   = 0x11,  // Push constant to stack
    Pop    = 0x12,  // Pop and discard top
    Dup    = 0x13,  // Duplicate top
    
    // Local variables
    LoadL  = 0x14,  // Load local variable (push)
    StoreL = 0x15,  // Store to local variable (pop)
    
    // Control flow
    Jmp    = 0x16,  // Unconditional jump
    Jz     = 0x17,  // Jump if zero (pop)
    Jnz    = 0x18,  // Jump if not zero (pop)
    
    // Functions
    Call   = 0x19,  // Call function
    Ret    = 0x20,  // Return from function
    
    // I/O
    Print  = 0x21,  // Print top (no pop)
    
    // Control
    Halt   = 0x22,
}

enum VType : ubyte {
    I32  = 0x0,
    I64  = 0x1,
    U32  = 0x2,
    U64  = 0x3,
    I8   = 0x4,
    U8   = 0x5,
    Str  = 0x6,
    Char = 0x7,
    F32  = 0x8,
    F64  = 0x9,
}

union UValue {
    int i32;
    long i64;
    uint u32;
    ulong u64;
    byte i8;
    ubyte u8;
    char* str;
    char ch;
    float f32;
    double f64;
}

struct IValue {
    VType type;
    UValue value;
}

struct CallFrame {
    uint returnPc;
    uint frameBase;
}

struct VM {
@nogc:
    uint pc = 0;
    uint* program;
    ulong programSize;
    
    IValue* constants;
    uint constantsSize;
    
    IValue* stack;
    uint sp = 0;
    uint stackLimit = 1000;
    uint stackMax = 1_000_000;
    
    CallFrame* callStack;
    uint fp = 0;
    uint frameLimit = 10_000;
    
    uint frameBase = 0;
    
    bool shouldFree;

    this(uint* program, ulong programSize, IValue* constants, uint constantsSize, uint startPc = 0, 
        bool shouldFree = false) 
    {
        this.program = program;
        this.programSize = programSize;
        this.constants = constants;
        this.constantsSize = constantsSize;
        this.pc = startPc;
        this.shouldFree = shouldFree;
        
        this.stack = cast(IValue*) malloc(IValue.sizeof * stackLimit);
        this.callStack = cast(CallFrame*) malloc(CallFrame.sizeof * frameLimit);
        
        if (this.stack is null || this.callStack is null) {
            printf("VM ERROR: Memory allocation failed\n");
            exit(2);
        }
    }

    ~this() 
    {
        if (shouldFree)
            destroy();
    }

    void destroy() {
        if (program !is null) free(program);
        if (constants !is null) free(constants);
        if (stack !is null) free(stack);
        if (callStack !is null) free(callStack);
    }

    pragma(inline, true)
    uint advance() 
    {
        if (pc >= programSize) 
        {
            printf("VM ERROR: PC out of bounds\n");
            exit(1);
        }
        return program[pc++];
    }

    pragma(inline, true)
    void push(IValue val) 
    {
        if (sp >= stackLimit) 
        {
            stackLimit = sp * 2;
            if (stackLimit > stackMax) 
            {
                printf("VM ERROR: Stack overflow\n");
                exit(1);
            }
            stack = cast(IValue*) realloc(stack, stackLimit * IValue.sizeof);
            if (stack is null) 
            {
                printf("VM ERROR: Stack realloc failed\n");
                exit(1);
            }
        }
        stack[sp++] = val;
    }

    pragma(inline, true)
    IValue pop() 
    {
        if (sp == 0) {
            printf("VM ERROR: Stack underflow\n");
            exit(1);
        }
        return stack[--sp];
    }

    pragma(inline, true)
    IValue* peek() 
    {
        if (sp == 0) {
            printf("VM ERROR: Stack empty\n");
            exit(1);
        }
        return &stack[sp - 1];
    }

    pragma(inline, true)
    int toInt(IValue* val) 
    {
        final switch (val.type) {
            case VType.I32: return val.value.i32;
            case VType.I64: return cast(int) val.value.i64;
            case VType.U32: return cast(int) val.value.u32;
            case VType.U64: return cast(int) val.value.u64;
            case VType.I8:  return cast(int) val.value.i8;
            case VType.U8:  return cast(int) val.value.u8;
            case VType.Char: return cast(int) val.value.ch;
            case VType.F32: return cast(int) val.value.f32;
            case VType.F64: return cast(int) val.value.f64;
            case VType.Str:
                printf("VM ERROR: Cannot convert string to int\n");
                exit(1);
        }
    }

    void run() 
    {
        while (pc < programSize) 
        {
            uint instr = advance();
            ubyte opcode = instr & 0xFF;
            switch (opcode) {
                case IKind.Add: opAdd(); break;
                case IKind.Sub: opSub(); break;
                case IKind.Mul: opMul(); break;
                case IKind.Div: opDiv(); break;
                case IKind.Mod: opMod(); break;
                case IKind.Eq:  opEq(); break;
                case IKind.Neq: opNeq(); break;
                case IKind.Lt:  opLt(); break;
                case IKind.Gt:  opGt(); break;
                case IKind.Ge:  opGe(); break;
                case IKind.Le:  opLe(); break;
                case IKind.Push:   opPush((instr >> 8) & 0xFFFF); break;
                case IKind.Pop:    opPopOp(); break;
                case IKind.Dup:    opDup(); break;
                case IKind.LoadL:  opLoadL((instr >> 8) & 0xFF); break;
                case IKind.StoreL: opStoreL((instr >> 8) & 0xFF); break;
                case IKind.Jmp:    opJmp((instr >> 8) & 0xFFFFFF); break;
                case IKind.Jz:     opJz((instr >> 8) & 0xFFFFFF); break;
                case IKind.Jnz:    opJnz((instr >> 8) & 0xFFFFFF); break;
                case IKind.Call:   opCall((instr >> 8) & 0xFFFF, (instr >> 24) & 0xFF); break;
                case IKind.Ret:    opRet(); break;
                case IKind.Print:  opPrint(); break;
                case IKind.Halt:   return;
                default:
                    printf("VM ERROR: Invalid opcode 0x%x at PC %d\n", opcode, pc-1);
                    exit(1);
            }
        }
    }

    // Arithmetic operations - FAST PATH
    void opAdd() {
        IValue right = pop();
        IValue left = pop();
        
        if (left.type == right.type) 
        {
            IValue result;
            result.type = left.type;
            final switch (left.type) 
            {
                case VType.I32:
                    result.value.i32 = left.value.i32 + right.value.i32;
                    break;
                case VType.I64:
                    result.value.i64 = left.value.i64 + right.value.i64;
                    break;
                case VType.U32:
                    result.value.u32 = left.value.u32 + right.value.u32;
                    break;
                case VType.U64:
                    result.value.u64 = left.value.u64 + right.value.u64;
                    break;
                case VType.I8:
                    result.type = VType.I32;
                    result.value.i32 = left.value.i8 + right.value.i8;
                    break;
                case VType.U8:
                    result.type = VType.U32;
                    result.value.u32 = left.value.u8 + right.value.u8;
                    break;
                case VType.Char:
                    result.type = VType.U32;
                    result.value.u32 = left.value.ch + right.value.ch;
                    break;
                case VType.F32:
                    result.value.f32 = left.value.f32 + right.value.f32;
                    break;
                case VType.F64:
                    result.value.f64 = left.value.f64 + right.value.f64;
                    break;
                case VType.Str:
                    printf("VM ERROR: Cannot add strings\n");
                    exit(1);
            }
            push(result);
            return;
        }
        
        IValue result = binaryOp(left, right, "+");
        push(result);
    }

    void opSub() {
        IValue right = pop();
        IValue left = pop();
        
        if (left.type == right.type) 
        {
            IValue result;
            result.type = left.type;
            final switch (left.type) 
            {
                case VType.I32:
                    result.value.i32 = left.value.i32 - right.value.i32;
                    break;
                case VType.I64:
                    result.value.i64 = left.value.i64 - right.value.i64;
                    break;
                case VType.U32:
                    result.value.u32 = left.value.u32 - right.value.u32;
                    break;
                case VType.U64:
                    result.value.u64 = left.value.u64 - right.value.u64;
                    break;
                case VType.I8:
                    result.type = VType.I32;
                    result.value.i32 = left.value.i8 - right.value.i8;
                    break;
                case VType.U8:
                    result.type = VType.U32;
                    result.value.u32 = left.value.u8 - right.value.u8;
                    break;
                case VType.Char:
                    result.type = VType.U32;
                    result.value.u32 = left.value.ch - right.value.ch;
                    break;
                case VType.F32:
                    result.value.f32 = left.value.f32 - right.value.f32;
                    break;
                case VType.F64:
                    result.value.f64 = left.value.f64 - right.value.f64;
                    break;
                case VType.Str:
                    printf("VM ERROR: Cannot subtract strings\n");
                    exit(1);
            }
            push(result);
            return;
        }
        
        IValue result = binaryOp(left, right, "-");
        push(result);
    }

    void opMul() 
    {
        IValue right = pop();
        IValue left = pop();
        
        if (left.type == right.type) {
            IValue result;
            result.type = left.type;
            final switch (left.type) 
            {
                case VType.I32:
                    result.value.i32 = left.value.i32 * right.value.i32;
                    break;
                case VType.I64:
                    result.value.i64 = left.value.i64 * right.value.i64;
                    break;
                case VType.U32:
                    result.value.u32 = left.value.u32 * right.value.u32;
                    break;
                case VType.U64:
                    result.value.u64 = left.value.u64 * right.value.u64;
                    break;
                case VType.I8:
                    result.type = VType.I32;
                    result.value.i32 = left.value.i8 * right.value.i8;
                    break;
                case VType.U8:
                    result.type = VType.U32;
                    result.value.u32 = left.value.u8 * right.value.u8;
                    break;
                case VType.Char:
                    result.type = VType.U32;
                    result.value.u32 = left.value.ch * right.value.ch;
                    break;
                case VType.F32:
                    result.value.f32 = left.value.f32 * right.value.f32;
                    break;
                case VType.F64:
                    result.value.f64 = left.value.f64 * right.value.f64;
                    break;
                case VType.Str:
                    printf("VM ERROR: Cannot multiply strings\n");
                    exit(1);
            }
            push(result);
            return;
        }
        
        IValue result = binaryOp(left, right, "*");
        push(result);
    }

    void opDiv() {
        IValue right = pop();
        IValue left = pop();
        
        if (left.type == right.type) 
        {
            IValue result;
            result.type = left.type;
            final switch (left.type) 
            {
                case VType.I32:
                    result.value.i32 = left.value.i32 / right.value.i32;
                    break;
                case VType.I64:
                    result.value.i64 = left.value.i64 / right.value.i64;
                    break;
                case VType.U32:
                    result.value.u32 = left.value.u32 / right.value.u32;
                    break;
                case VType.U64:
                    result.value.u64 = left.value.u64 / right.value.u64;
                    break;
                case VType.I8:
                    result.type = VType.I32;
                    result.value.i32 = left.value.i8 / right.value.i8;
                    break;
                case VType.U8:
                    result.type = VType.U32;
                    result.value.u32 = left.value.u8 / right.value.u8;
                    break;
                case VType.Char:
                    result.type = VType.U32;
                    result.value.u32 = left.value.ch / right.value.ch;
                    break;
                case VType.F32:
                    result.value.f32 = left.value.f32 / right.value.f32;
                    break;
                case VType.F64:
                    result.value.f64 = left.value.f64 / right.value.f64;
                    break;
                case VType.Str:
                    printf("VM ERROR: Cannot divide strings\n");
                    exit(1);
            }
            push(result);
            return;
        }
        
        IValue result = binaryOp(left, right, "/");
        push(result);
    }

    void opMod() {
        IValue right = pop();
        IValue left = pop();
        
        if (left.type == right.type) 
        {
            IValue result;
            result.type = left.type;
            final switch (left.type) 
            {
                case VType.I32:
                    result.value.i32 = left.value.i32 % right.value.i32;
                    break;
                case VType.I64:
                    result.value.i64 = left.value.i64 % right.value.i64;
                    break;
                case VType.U32:
                    result.value.u32 = left.value.u32 % right.value.u32;
                    break;
                case VType.U64:
                    result.value.u64 = left.value.u64 % right.value.u64;
                    break;
                case VType.I8:
                    result.type = VType.I32;
                    result.value.i32 = left.value.i8 % right.value.i8;
                    break;
                case VType.U8:
                    result.type = VType.U32;
                    result.value.u32 = left.value.u8 % right.value.u8;
                    break;
                case VType.Char:
                    result.type = VType.U32;
                    result.value.u32 = left.value.ch % right.value.ch;
                    break;
                case VType.F32:
                case VType.F64:
                    printf("VM ERROR: Cannot mod floats\n");
                    exit(1);
                case VType.Str:
                    printf("VM ERROR: Cannot mod strings\n");
                    exit(1);
            }
            push(result);
            return;
        }
        
        IValue result = binaryOp(left, right, "%");
        push(result);
    }

    void opEq() 
    {
        IValue right = pop();
        IValue left = pop();
        IValue result = compareOp(left, right, "==");
        push(result);
    }

    void opNeq() 
    {
        IValue right = pop();
        IValue left = pop();
        IValue result = compareOp(left, right, "!=");
        push(result);
    }

    void opLt() 
    {
        IValue right = pop();
        IValue left = pop();
        IValue result = compareOp(left, right, "<");
        push(result);
    }

    void opGt() 
    {
        IValue right = pop();
        IValue left = pop();
        IValue result = compareOp(left, right, ">");
        push(result);
    }

    void opGe() 
    {
        IValue right = pop();
        IValue left = pop();
        IValue result = compareOp(left, right, ">=");
        push(result);
    }

    void opLe() 
    {
        IValue right = pop();
        IValue left = pop();
        IValue result = compareOp(left, right, "<=");
        push(result);
    }

    // SLOW PATH - conversão completa de tipos
    IValue binaryOp(IValue left, IValue right, string op) 
    {
        IValue result;
        VType targetType = promoteType(left.type, right.type);
        result.type = targetType;
        
        final switch (targetType) {
            case VType.I32:
                int l = castToI32(left);
                int r = castToI32(right);
                switch (op) {
                    case "+": result.value.i32 = l + r; break;
                    case "-": result.value.i32 = l - r; break;
                    case "*": result.value.i32 = l * r; break;
                    case "/": result.value.i32 = l / r; break;
                    case "%": result.value.i32 = l % r; break;
                    default: break;
                }
                break;
            case VType.I64:
                long l = castToI64(left);
                long r = castToI64(right);
                switch (op) {
                    case "+": result.value.i64 = l + r; break;
                    case "-": result.value.i64 = l - r; break;
                    case "*": result.value.i64 = l * r; break;
                    case "/": result.value.i64 = l / r; break;
                    case "%": result.value.i64 = l % r; break;
                    default: break;
                }
                break;
            case VType.U32:
                uint l = castToU32(left);
                uint r = castToU32(right);
                switch (op) {
                    case "+": result.value.u32 = l + r; break;
                    case "-": result.value.u32 = l - r; break;
                    case "*": result.value.u32 = l * r; break;
                    case "/": result.value.u32 = l / r; break;
                    case "%": result.value.u32 = l % r; break;
                    default: break;
                }
                break;
            case VType.U64:
                ulong l = castToU64(left);
                ulong r = castToU64(right);
                switch (op) {
                    case "+": result.value.u64 = l + r; break;
                    case "-": result.value.u64 = l - r; break;
                    case "*": result.value.u64 = l * r; break;
                    case "/": result.value.u64 = l / r; break;
                    case "%": result.value.u64 = l % r; break;
                    default: break;
                }
                break;
            case VType.F32:
                float l = castToF32(left);
                float r = castToF32(right);
                switch (op) {
                    case "+": result.value.f32 = l + r; break;
                    case "-": result.value.f32 = l - r; break;
                    case "*": result.value.f32 = l * r; break;
                    case "/": result.value.f32 = l / r; break;
                    default: break;
                }
                break;
            case VType.F64:
                double l = castToF64(left);
                double r = castToF64(right);
                switch (op) {
                    case "+": result.value.f64 = l + r; break;
                    case "-": result.value.f64 = l - r; break;
                    case "*": result.value.f64 = l * r; break;
                    case "/": result.value.f64 = l / r; break;
                    default: break;
                }
                break;
            case VType.I8:
            case VType.U8:
            case VType.Char:
            case VType.Str:
                printf("VM ERROR: Invalid binary operation\n");
                exit(1);
        }
        
        return result;
    }

    IValue compareOp(IValue left, IValue right, string op) 
    {
        IValue result;
        result.type = VType.I32;
        
        if (left.type == right.type)
         {
            int cmp = 0;
            final switch (left.type) 
            {
                case VType.I32:
                    switch (op) 
                    {
                        case "==": cmp = left.value.i32 == right.value.i32; break;
                        case "!=": cmp = left.value.i32 != right.value.i32; break;
                        case "<":  cmp = left.value.i32 < right.value.i32; break;
                        case ">":  cmp = left.value.i32 > right.value.i32; break;
                        case "<=": cmp = left.value.i32 <= right.value.i32; break;
                        case ">=": cmp = left.value.i32 >= right.value.i32; break;
                        default: break;
                    }
                    break;
                case VType.I64:
                    switch (op) 
                    {
                        case "==": cmp = left.value.i64 == right.value.i64; break;
                        case "!=": cmp = left.value.i64 != right.value.i64; break;
                        case "<":  cmp = left.value.i64 < right.value.i64; break;
                        case ">":  cmp = left.value.i64 > right.value.i64; break;
                        case "<=": cmp = left.value.i64 <= right.value.i64; break;
                        case ">=": cmp = left.value.i64 >= right.value.i64; break;
                        default: break;
                    }
                    break;
                case VType.U32:
                    switch (op) 
                    {
                        case "==": cmp = left.value.u32 == right.value.u32; break;
                        case "!=": cmp = left.value.u32 != right.value.u32; break;
                        case "<":  cmp = left.value.u32 < right.value.u32; break;
                        case ">":  cmp = left.value.u32 > right.value.u32; break;
                        case "<=": cmp = left.value.u32 <= right.value.u32; break;
                        case ">=": cmp = left.value.u32 >= right.value.u32; break;
                        default: break;
                    }
                    break;
                case VType.U64:
                    switch (op) 
                    {
                        case "==": cmp = left.value.u64 == right.value.u64; break;
                        case "!=": cmp = left.value.u64 != right.value.u64; break;
                        case "<":  cmp = left.value.u64 < right.value.u64; break;
                        case ">":  cmp = left.value.u64 > right.value.u64; break;
                        case "<=": cmp = left.value.u64 <= right.value.u64; break;
                        case ">=": cmp = left.value.u64 >= right.value.u64; break;
                        default: break;
                    }
                    break;
                case VType.I8:
                    switch (op) 
                    {
                        case "==": cmp = left.value.i8 == right.value.i8; break;
                        case "!=": cmp = left.value.i8 != right.value.i8; break;
                        case "<":  cmp = left.value.i8 < right.value.i8; break;
                        case ">":  cmp = left.value.i8 > right.value.i8; break;
                        case "<=": cmp = left.value.i8 <= right.value.i8; break;
                        case ">=": cmp = left.value.i8 >= right.value.i8; break;
                        default: break;
                    }
                    break;
                case VType.U8:
                    switch (op) 
                    {
                        case "==": cmp = left.value.u8 == right.value.u8; break;
                        case "!=": cmp = left.value.u8 != right.value.u8; break;
                        case "<":  cmp = left.value.u8 < right.value.u8; break;
                        case ">":  cmp = left.value.u8 > right.value.u8; break;
                        case "<=": cmp = left.value.u8 <= right.value.u8; break;
                        case ">=": cmp = left.value.u8 >= right.value.u8; break;
                        default: break;
                    }
                    break;
                case VType.Char:
                    switch (op) 
                    {
                        case "==": cmp = left.value.ch == right.value.ch; break;
                        case "!=": cmp = left.value.ch != right.value.ch; break;
                        case "<":  cmp = left.value.ch < right.value.ch; break;
                        case ">":  cmp = left.value.ch > right.value.ch; break;
                        case "<=": cmp = left.value.ch <= right.value.ch; break;
                        case ">=": cmp = left.value.ch >= right.value.ch; break;
                        default: break;
                    }
                    break;
                case VType.F32:
                    switch (op) 
                    {
                        case "==": cmp = left.value.f32 == right.value.f32; break;
                        case "!=": cmp = left.value.f32 != right.value.f32; break;
                        case "<":  cmp = left.value.f32 < right.value.f32; break;
                        case ">":  cmp = left.value.f32 > right.value.f32; break;
                        case "<=": cmp = left.value.f32 <= right.value.f32; break;
                        case ">=": cmp = left.value.f32 >= right.value.f32; break;
                        default: break;
                    }
                    break;
                case VType.F64:
                    switch (op) 
                    {
                        case "==": cmp = left.value.f64 == right.value.f64; break;
                        case "!=": cmp = left.value.f64 != right.value.f64; break;
                        case "<":  cmp = left.value.f64 < right.value.f64; break;
                        case ">":  cmp = left.value.f64 > right.value.f64; break;
                        case "<=": cmp = left.value.f64 <= right.value.f64; break;
                        case ">=": cmp = left.value.f64 >= right.value.f64; break;
                        default: break;
                    }
                    break;
                case VType.Str:
                    printf("VM ERROR: Cannot compare strings\n");
                    exit(1);
            }
            result.value.i32 = cmp;
            return result;
        }
        
        // Slow path: conversão necessária
        double l = castToF64(left);
        double r = castToF64(right);
        int cmp = 0;
        switch (op) 
        {
            case "==": cmp = l == r; break;
            case "!=": cmp = l != r; break;
            case "<":  cmp = l < r; break;
            case ">":  cmp = l > r; break;
            case "<=": cmp = l <= r; break;
            case ">=": cmp = l >= r; break;
            default: break;
        }
        result.value.i32 = cmp;
        return result;
    }

    // Funções de promoção e conversão de tipos
    VType promoteType(VType a, VType b) 
    {
        if (a == VType.Str || b == VType.Str)
            return VType.Str;
        
        if (a == VType.F64 || b == VType.F64)
            return VType.F64;
        if (a == VType.F32 || b == VType.F32)
            return VType.F32;
        
        if (a == VType.I64 || b == VType.I64)
            return VType.I64;
        if (a == VType.U64 || b == VType.U64)
            return VType.U64;
        if (a == VType.I32 || b == VType.I32)
            return VType.I32;
        if (a == VType.U32 || b == VType.U32)
            return VType.U32;
        
        return VType.I32;
    }

    int castToI32(IValue v) 
    {
        final switch (v.type) 
        {
            case VType.I32: return v.value.i32;
            case VType.I64: return cast(int) v.value.i64;
            case VType.U32: return cast(int) v.value.u32;
            case VType.U64: return cast(int) v.value.u64;
            case VType.I8:  return cast(int) v.value.i8;
            case VType.U8:  return cast(int) v.value.u8;
            case VType.Char: return cast(int) v.value.ch;
            case VType.F32: return cast(int) v.value.f32;
            case VType.F64: return cast(int) v.value.f64;
            case VType.Str:
                printf("VM ERROR: Cannot cast string to i32\n");
                exit(1);
        }
    }

    long castToI64(IValue v) 
    {
        final switch (v.type) 
        {
            case VType.I32: return cast(long) v.value.i32;
            case VType.I64: return v.value.i64;
            case VType.U32: return cast(long) v.value.u32;
            case VType.U64: return cast(long) v.value.u64;
            case VType.I8:  return cast(long) v.value.i8;
            case VType.U8:  return cast(long) v.value.u8;
            case VType.Char: return cast(long) v.value.ch;
            case VType.F32: return cast(long) v.value.f32;
            case VType.F64: return cast(long) v.value.f64;
            case VType.Str:
                printf("VM ERROR: Cannot cast string to i64\n");
                exit(1);
        }
    }

    uint castToU32(IValue v) 
    {
        final switch (v.type) 
        {
            case VType.I32: return cast(uint) v.value.i32;
            case VType.I64: return cast(uint) v.value.i64;
            case VType.U32: return v.value.u32;
            case VType.U64: return cast(uint) v.value.u64;
            case VType.I8:  return cast(uint) v.value.i8;
            case VType.U8:  return cast(uint) v.value.u8;
            case VType.Char: return cast(uint) v.value.ch;
            case VType.F32: return cast(uint) v.value.f32;
            case VType.F64: return cast(uint) v.value.f64;
            case VType.Str:
                printf("VM ERROR: Cannot cast string to u32\n");
                exit(1);
        }
    }

    ulong castToU64(IValue v) 
    {
        final switch (v.type) 
        {
            case VType.I32: return cast(ulong) v.value.i32;
            case VType.I64: return cast(ulong) v.value.i64;
            case VType.U32: return cast(ulong) v.value.u32;
            case VType.U64: return v.value.u64;
            case VType.I8:  return cast(ulong) v.value.i8;
            case VType.U8:  return cast(ulong) v.value.u8;
            case VType.Char: return cast(ulong) v.value.ch;
            case VType.F32: return cast(ulong) v.value.f32;
            case VType.F64: return cast(ulong) v.value.f64;
            case VType.Str:
                printf("VM ERROR: Cannot cast string to u64\n");
                exit(1);
        }
    }

    float castToF32(IValue v) 
    {
        final switch (v.type) 
        {
            case VType.I32: return cast(float) v.value.i32;
            case VType.I64: return cast(float) v.value.i64;
            case VType.U32: return cast(float) v.value.u32;
            case VType.U64: return cast(float) v.value.u64;
            case VType.I8:  return cast(float) v.value.i8;
            case VType.U8:  return cast(float) v.value.u8;
            case VType.Char: return cast(float) v.value.ch;
            case VType.F32: return v.value.f32;
            case VType.F64: return cast(float) v.value.f64;
            case VType.Str:
                printf("VM ERROR: Cannot cast string to f32\n");
                exit(1);
        }
    }

    double castToF64(IValue v) 
    {
        final switch (v.type) 
        {
            case VType.I32: return cast(double) v.value.i32;
            case VType.I64: return cast(double) v.value.i64;
            case VType.U32: return cast(double) v.value.u32;
            case VType.U64: return cast(double) v.value.u64;
            case VType.I8:  return cast(double) v.value.i8;
            case VType.U8:  return cast(double) v.value.u8;
            case VType.Char: return cast(double) v.value.ch;
            case VType.F32: return cast(double) v.value.f32;
            case VType.F64: return v.value.f64;
            case VType.Str:
                printf("VM ERROR: Cannot cast string to f64\n");
                exit(1);
        }
    }

    // Stack operations
    void opPush(ushort constIdx) 
    {
        if (constIdx >= constantsSize) {
            printf("VM ERROR: Constant index out of bounds\n");
            exit(1);
        }
        push(constants[constIdx]);
    }

    void opPopOp() 
    {
        pop();
    }

    void opDup() 
    {
        IValue val = *peek();
        push(val);
    }

    // Local variables
    void opLoadL(ubyte localIdx) 
    {
        uint idx = frameBase + localIdx;
        if (idx >= stackLimit) {
            printf("VM ERROR: LoadL index %d out of bounds (frameBase=%d, sp=%d)\n", localIdx, frameBase, sp);
            exit(1);
        }
        push(stack[idx]);
    }

    void opStoreL(ubyte localIdx) 
    {
        uint idx = frameBase + localIdx;
        if (idx >= stackLimit) {
            printf("VM ERROR: StoreL index %d out of bounds (frameBase=%d, sp=%d, idx=%d)\n", localIdx, frameBase, sp, 
                idx);
            exit(1);
        }
        stack[idx] = pop();
    }

    // Control flow
    void opJmp(uint targetPc) 
    {
        pc = targetPc;
    }

    void opJz(uint targetPc) 
    {
        IValue val = pop();
        if (toInt(&val) == 0)
            pc = targetPc;
    }

    void opJnz(uint targetPc) 
    {
        IValue val = pop();
        if (toInt(&val) != 0)
            pc = targetPc;
    }

    // Function calls
    void opCall(ushort targetPc, ubyte numLocals) 
    {
        if (fp >= frameLimit) 
        {
            printf("VM ERROR: Call stack overflow\n");
            exit(1);
        }
        
        // O argumento já está no topo da stack
        callStack[fp++] = CallFrame(pc, frameBase);
        
        // frameBase aponta para o primeiro slot de variável local (onde está o argumento)
        frameBase = sp - 1;
        pc = targetPc;
        
        // Reserve space for additional locals (além do argumento que já está lá)
        IValue zero;
        zero.type = VType.I32;
        zero.value.i32 = 0;
        for (uint i = 1; i < numLocals; i++)
            push(zero);
    }

    void opRet() 
    {
        if (fp == 0) 
        {
            printf("VM ERROR: Return with empty call stack\n");
            exit(1);
        }
        
        // Pop the return value (if any)
        IValue retVal;
        bool hasRetVal = sp > frameBase;
        if (hasRetVal)
            retVal = pop();
        
        CallFrame frame = callStack[--fp];
        
        // Limpa todos os locals da função
        sp = frameBase;
        
        frameBase = frame.frameBase;
        pc = frame.returnPc;
        
        // Push return value back
        if (hasRetVal)
            push(retVal);
    }

    void opPrint() {
        IValue* val = peek();
        final switch (val.type) 
        {
            case VType.I32:
                printf("%d", val.value.i32);
                break;
            case VType.I64:
                printf("%lld", val.value.i64);
                break;
            case VType.U32:
                printf("%u", val.value.u32);
                break;
            case VType.U64:
                printf("%llu", val.value.u64);
                break;
            case VType.I8:
                printf("%d", val.value.i8);
                break;
            case VType.U8:
                printf("%u", val.value.u8);
                break;
            case VType.F32:
                printf("%f", cast(double)val.value.f32);
                break;
            case VType.F64:
                printf("%f", val.value.f64);
                break;
            case VType.Str:
                printf("%s", val.value.str);
                break;
            case VType.Char:
                printf("%c", val.value.ch);
                break;
        }
    }
}

// Instruction encoding helpers
pragma(inline, true)
uint encode(ubyte op, uint arg = 0) 
{
    return cast(uint)op | (arg << 8);
}

pragma(inline, true)
uint encodeCall(ushort targetPc, ubyte numLocals) 
{
    return cast(uint)IKind.Call | (cast(uint)targetPc << 8) | (cast(uint)numLocals << 24);
}

// int main() 
// {
//     CodeBuilder builder = CodeBuilder(256);

//     auto v1 = builder.constI32(1);
//     auto v2 = builder.constI32(2);
//     auto v3 = builder.constI32(30);
//     auto v4 = builder.constStr("\n");


//     builder.jmpLabel("main")
//     .label("fibo")
//     .loadLocal(0)
//     .push(v2)
//     .lt()
//     .jzLabel("fibo_else")
//     .loadLocal(0)
//     .ret()
//     .label("fibo_else")
    
//     .loadLocal(0)
//     .push(v1)
//     .sub()
//     .callLabel("fibo", 1)

//     .loadLocal(0)
//     .push(v2)
//     .sub()
//     .callLabel("fibo", 1)

//     .add()
//     .ret()

//     .label("main")
//     .push(v3)
//     .callLabel("fibo", 1)
//     .print()
//     .push(v4)
//     .print()
//     .buildVM()
//     .run();
    
//     return 0;
// }
