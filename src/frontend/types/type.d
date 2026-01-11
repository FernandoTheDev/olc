module frontend.types.type;

import frontend;

enum BaseType : string
{
    String = "string",
    Char = "char",
    Int = "int",
    Long = "long",
    Ulong = "ulong",
    Uint = "uint",
    Ubyte = "ubyte",
    Byte = "byte",
    Short = "short",
    Ushort = "ushort",
    Float = "float",
    Double = "double",
    Bool = "bool",
    Void = "void",
    Any = "any",
}

const int[string] TYPE_HIERARCHY = [
    BaseType.Bool: 1,
    BaseType.Byte: 2,
    BaseType.Ubyte: 2,
    BaseType.Char: 2,
    BaseType.Short: 3,
    BaseType.Ushort: 3,
    BaseType.Int: 4,
    BaseType.Uint: 4,
    BaseType.Long: 5,
    BaseType.Ulong: 5,
    BaseType.Float: 6,
    BaseType.Double: 7,
];

abstract class Type
{
    bool constant = false;
    bool refConst;
    abstract bool isCompatibleWith(Type other, bool strict = true);
    abstract string toStr();
    abstract Type clone();

    bool isNumeric()
    {
        return false;
    }

    bool isArray()
    {
        return false;
    }

    bool isStruct()
    {
        return false;
    }

    bool isEnum()
    {
        return false;
    }

    bool isPrimitive()
    {
        return false;
    }

    bool isVoid()
    {
        return false;
    }

    bool isQualified()
    {
        return false;
    }

    bool isUnion()
    {
        return false;
    }

    bool isPointer()
    {
        return false;
    }

    Type getPromotedType(Type other)
    {
        if (auto prim1 = cast(PrimitiveType) this)
            if (auto prim2 = cast(PrimitiveType) other)
                return PrimitiveType.promote(prim1, prim2);
        return this;
    }
}

class PrimitiveType : Type
{
    BaseType baseType;

    private static immutable string[][string] STRICT_COMPAT = [
        BaseType.Bool: [BaseType.Byte, BaseType.Short, BaseType.Int, BaseType.Long],

        BaseType.Byte: [BaseType.Short, BaseType.Int, BaseType.Long],
        BaseType.Ubyte: [BaseType.Short, BaseType.Ushort, BaseType.Int, BaseType.Uint, BaseType.Long, BaseType.Ulong],

        BaseType.Char: [BaseType.Short, BaseType.Ushort, BaseType.Int, BaseType.Uint, BaseType.Long, BaseType.Ulong],

        BaseType.Short: [BaseType.Int, BaseType.Long],
        BaseType.Ushort: [BaseType.Int, BaseType.Uint, BaseType.Long, BaseType.Ulong],

        BaseType.Int: [BaseType.Long],
        BaseType.Uint: [BaseType.Long, BaseType.Ulong],

        BaseType.Long: [BaseType.Double],
        BaseType.Ulong: [BaseType.Double],

        BaseType.Float: [BaseType.Double],
        BaseType.Double: [],

        BaseType.String: [],
    ];

    private static immutable string[][string] LIBERAL_COMPAT = [
        BaseType.Bool: [
            BaseType.Byte, BaseType.Ubyte, BaseType.Char,
            BaseType.Short, BaseType.Ushort,
            BaseType.Int, BaseType.Uint,
            BaseType.Long, BaseType.Ulong,
            BaseType.Float, BaseType.Double
        ],

        BaseType.Byte: [
            BaseType.Ubyte, BaseType.Char,
            BaseType.Short, BaseType.Ushort,
            BaseType.Int, BaseType.Uint,
            BaseType.Long, BaseType.Ulong,
            BaseType.Float, BaseType.Double,
            BaseType.Bool
        ],

        BaseType.Ubyte: [
            BaseType.Byte, BaseType.Char,
            BaseType.Short, BaseType.Ushort,
            BaseType.Int, BaseType.Uint,
            BaseType.Long, BaseType.Ulong,
            BaseType.Float, BaseType.Double,
            BaseType.Bool
        ],

        BaseType.Char: [
            BaseType.Byte, BaseType.Ubyte,
            BaseType.Short, BaseType.Ushort,
            BaseType.Int, BaseType.Uint,
            BaseType.Long, BaseType.Ulong,
            BaseType.Bool
        ],

        BaseType.Short: [
            BaseType.Byte, BaseType.Ubyte,
            BaseType.Ushort,
            BaseType.Int, BaseType.Uint,
            BaseType.Long, BaseType.Ulong,
            BaseType.Float, BaseType.Double,
            BaseType.Bool
        ],

        BaseType.Ushort: [
            BaseType.Byte, BaseType.Ubyte,
            BaseType.Short,
            BaseType.Int, BaseType.Uint,
            BaseType.Long, BaseType.Ulong,
            BaseType.Float, BaseType.Double,
            BaseType.Bool
        ],

        BaseType.Int: [
            BaseType.Byte, BaseType.Ubyte,
            BaseType.Short, BaseType.Ushort,
            BaseType.Uint,
            BaseType.Long, BaseType.Ulong,
            BaseType.Float, BaseType.Double,
            BaseType.Bool, BaseType.Char
        ],

        BaseType.Uint: [
            BaseType.Byte, BaseType.Ubyte,
            BaseType.Short, BaseType.Ushort,
            BaseType.Int,
            BaseType.Long, BaseType.Ulong,
            BaseType.Float, BaseType.Double,
            BaseType.Bool
        ],

        BaseType.Long: [
            BaseType.Byte, BaseType.Ubyte,
            BaseType.Short, BaseType.Ushort,
            BaseType.Int, BaseType.Uint,
            BaseType.Ulong,
            BaseType.Float, BaseType.Double,
            BaseType.Bool, BaseType.Char
        ],

        BaseType.Ulong: [
            BaseType.Byte, BaseType.Ubyte,
            BaseType.Short, BaseType.Ushort,
            BaseType.Int, BaseType.Uint,
            BaseType.Long,
            BaseType.Float, BaseType.Double,
            BaseType.Bool
        ],

        BaseType.Float: [
            BaseType.Byte, BaseType.Ubyte,
            BaseType.Short, BaseType.Ushort,
            BaseType.Int, BaseType.Uint,
            BaseType.Long, BaseType.Ulong,
            BaseType.Double,
            BaseType.Bool
        ],

        BaseType.Double: [
            BaseType.Byte, BaseType.Ubyte,
            BaseType.Short, BaseType.Ushort,
            BaseType.Int, BaseType.Uint,
            BaseType.Long, BaseType.Ulong,
            BaseType.Float,
            BaseType.Bool
        ],

        BaseType.String: [
            BaseType.Byte, BaseType.Ubyte,
            BaseType.Short, BaseType.Ushort,
            BaseType.Int, BaseType.Uint,
            BaseType.Long, BaseType.Ulong,
            BaseType.Float, BaseType.Double,
            BaseType.Bool
        ],
    ];

    this(BaseType baseType)
    {
        this.baseType = baseType;
    }

    override bool isPrimitive()
    {
        return true;
    }

    bool isInteger()
    {
        return baseType == BaseType.Byte || baseType == BaseType.Ubyte ||
               baseType == BaseType.Short || baseType == BaseType.Ushort ||
               baseType == BaseType.Int || baseType == BaseType.Uint ||
               baseType == BaseType.Long || baseType == BaseType.Ulong ||
               baseType == BaseType.Char;
    }

    override bool isNumeric()
    {
        return baseType == BaseType.Byte || baseType == BaseType.Ubyte ||
               baseType == BaseType.Short || baseType == BaseType.Ushort ||
               baseType == BaseType.Int || baseType == BaseType.Uint ||
               baseType == BaseType.Long || baseType == BaseType.Ulong ||
               baseType == BaseType.Float || baseType == BaseType.Double ||
               baseType == BaseType.Char;
    }

    bool isUnsigned()
    {
        return baseType == BaseType.Ubyte || baseType == BaseType.Ushort ||
               baseType == BaseType.Uint || baseType == BaseType.Ulong;
    }

    bool isSigned()
    {
        return baseType == BaseType.Byte || baseType == BaseType.Short ||
               baseType == BaseType.Int || baseType == BaseType.Long;
    }

    bool isFloatingPoint()
    {
        return baseType == BaseType.Float || baseType == BaseType.Double;
    }

    override bool isCompatibleWith(Type other, bool strict = true)
    {
        if (baseType == BaseType.Any)
            return true;

        if (auto otherPrim = cast(PrimitiveType) other)
        {
            if (otherPrim.baseType == BaseType.Any)
                return true;

            if (baseType == otherPrim.baseType)
                return true;

            string thisStr = baseType;
            string otherStr = otherPrim.baseType;

            if (thisStr in STRICT_COMPAT && STRICT_COMPAT[thisStr].canFind(otherStr))
                return true;

            if (!strict)
                if (thisStr in LIBERAL_COMPAT && LIBERAL_COMPAT[thisStr].canFind(otherStr))
                    return true;
        }

        if (auto fn = cast(FunctionType) other)
            return isCompatibleWith(fn.returnType);

        if (PointerType ptr = cast(PointerType) other)
            if (isInteger() && (ptr.toStr() == "void*" || ptr.toStr() == "char*"))
                return true;

        return false;
    }

    override bool isVoid()
    {
        return baseType == BaseType.Void;
    }

    override string toStr()
    {
        return cast(string) baseType;
    }

    override Type clone()
    {
        return new PrimitiveType(baseType);
    }

    static PrimitiveType promote(PrimitiveType left, PrimitiveType right)
    {
        int leftLevel = TYPE_HIERARCHY.get(left.baseType, 0);
        int rightLevel = TYPE_HIERARCHY.get(right.baseType, 0);
        return (leftLevel >= rightLevel) ? left : right;
    }
}

class VoidType : Type
{
    private static VoidType _instance;

    // Singleton
    static VoidType instance()
    {
        if (_instance is null)
            _instance = new VoidType();
        return _instance;
    }

    this()
    {
    }

    override bool isVoid()
    {
        return true;
    }

    override bool isCompatibleWith(Type other, bool strict = true)
    {
        if (auto vd = cast(VoidType) other)
            return true;
        if (PrimitiveType prim = cast(PrimitiveType) other)
            return prim.baseType == BaseType.Void;
        return false;
    }

    override string toStr()
    {
        return "void";
    }

    override Type clone()
    {
        return instance();
    }
}

class ArrayType : Type
{
    Type elementType;
    int dimensions;
    long length = 0;

    this(Type elementType, int dimensions = 1, long length = 0, bool constant = false)
    {
        this.elementType = elementType;
        this.dimensions = dimensions;
        this.length = length;
        this.constant = constant;
    }

    override bool isArray()
    {
        return true;
    }

    override bool isCompatibleWith(Type other, bool strict = true)
    {
        if (auto otherArray = cast(ArrayType) other)
        {
            // Arrays devem ter o mesmo número de dimensões
            if (dimensions != otherArray.dimensions)
                return false;

            if (length != otherArray.length && constant)
                return false;

            // o meu array left deve ter o mesmo tamanho ou ser maior que o right (other)
            if (length < otherArray.length)
                return false;

            // O tipo dos elementos deve ser compatível
            return elementType.isCompatibleWith(otherArray.elementType, strict);
        }

        return false;
    }

    override string toStr()
    {
        string result = elementType.toStr();
        for (int i = 0; i < dimensions; i++)
            result ~= "[" ~ to!string(length) ~ "]";
        return result;
    }

    override Type clone()
    {
        return new ArrayType(elementType.clone(), dimensions, length);
    }

    // Retorna o tipo base (sem as dimensões de array)
    Type getBaseType()
    {
        return elementType;
    }
}

class StructType : Type
{
    string name;
    StructField[] fields;
    StructMethod[][string] methods;
    string mangledName;
    
    // NOVO: Lista de structs que este struct pode ser convertido
    string[] compatibleCasts;
    
    private int[string] fieldIndexMap;

    this(string name, StructField[] fields = [], StructMethod[][string] methods, 
         string mangledName = "", string[] compatibleCasts = [])
    {
        this.name = name;
        this.fields = fields;
        if (mangledName == "")
            this.mangledName = name;
        
        foreach (i, field; fields)
            fieldIndexMap[field.name] = cast(int) i;
        
        this.methods = methods;
        this.compatibleCasts = compatibleCasts;
    }

    override bool isCompatibleWith(Type other, bool strict = true)
    {
        if (other.toStr() == "void*")
            return true;
        
        if (auto otherStruct = cast(StructType) other)
        {
            if (name == otherStruct.name)
                return true;
    
            if (!strict && compatibleCasts.canFind(otherStruct.name))
                return true;
        }
        
        if (auto otherPtr = cast(PointerType) other)
        {
            if (auto otherStruct = cast(StructType) otherPtr.pointeeType)
            {
                if (!strict && compatibleCasts.canFind(otherStruct.name))
                    return true;
                
                if (!strict && hasStructuralCompatibility(otherStruct))
                    return true;
            }
        }
        
        return false;
    }
    
    bool hasStructuralCompatibility(StructType other)
    {
        // Se um struct tem os mesmos primeiros N campos de outro,
        // pode ser convertido (similar a herança em C)
        size_t minFields = min(fields.length, other.fields.length);
        
        if (minFields == 0)
            return false;
        
        foreach (i; 0 .. minFields)
        {
            if (fields[i].name != other.fields[i].name)
                return false;
            
            if (fields[i].resolvedType is null || other.fields[i].resolvedType is null)
                return false;
            
            if (fields[i].resolvedType.toStr() != other.fields[i].resolvedType.toStr())
                return false;
        }
        
        return true;
    }
    
    // NOVO: Adiciona um cast compatível
    void addCompatibleCast(string structName)
    {
        if (!compatibleCasts.canFind(structName))
            compatibleCasts ~= structName;
    }

    override string toStr()
    {
        return name;
    }

    override Type clone()
    {
        StructMethod[][string] methodsCopy;
        foreach (methodName, overloads; methods)
            methodsCopy[methodName] = overloads.dup;
        return new StructType(name, fields.dup, methodsCopy, mangledName, compatibleCasts.dup);
    }

    override bool isStruct()
    {
        return true;
    }

    StructField* getField(string fieldName)
    {
        if (auto idx = fieldName in fieldIndexMap)
            return &fields[*idx];
        return null;
    }

    bool addMethod(StructMethod newMethod)
    {
        string methodName = newMethod.funcDecl.name;        
        if (methodName in methods)
        {
            auto existingOverloads = methods[methodName];
            foreach (StructMethod existing; existingOverloads)
                if (isSameMethodSignature(newMethod, existing))
                    // Assinaturas idênticas - erro
                    return false;
                // Validação @nomangle para métodos
                else if (newMethod.funcDecl.noMangle && existing.funcDecl.noMangle)
                    return false;
            methods[methodName] ~= newMethod;
        }
        else
            methods[methodName] = [newMethod];
        return true;
    }

    bool isSameMethodSignature(StructMethod a, StructMethod b)
    {
        // Construtores são sempre únicos (não podem ter overload por assinatura)
        if (a.isConstructor && b.isConstructor)
        {
            // Verifica parâmetros
            if (a.funcDecl.args.length != b.funcDecl.args.length)
                return false;
            
            foreach (i, paramA; a.funcDecl.args)
            {
                auto paramB = b.funcDecl.args[i];
                if (paramA.resolvedType is null || paramB.resolvedType is null)
                    continue;
                if (paramA.resolvedType.toStr() != paramB.resolvedType.toStr())
                    return false;
            }
            return true;
        }

        // Para métodos normais, compara parâmetros
        if (a.funcDecl.args.length != b.funcDecl.args.length)
            return false;

        foreach (i, paramA; a.funcDecl.args)
        {
            auto paramB = b.funcDecl.args[i];
            if (paramA.resolvedType is null || paramB.resolvedType is null)
                continue;
            if (paramA.resolvedType.toStr() != paramB.resolvedType.toStr())
                return false;
        }

        return true;
    }

    StructMethod[] resolveMethods(string methodName)
    {
        if (methodName in methods)
            return methods[methodName];
        return [];
    }

    StructMethod* getMethod(string methodName)
    {
        auto overloads = resolveMethods(methodName);
        if (overloads.length > 0)
            return &overloads[0];
        return null;
    }

    StructMethod* getConstructor()
    {
        foreach (overloads; methods)
            foreach (ref method; overloads)
                if (method.isConstructor)
                    return &method;
        return null;
    }

    StructMethod* findMethod(string methodName, Type[] argTypes)
    {
        auto candidates = resolveMethods(methodName);
        if (candidates.length == 0)
            return null;

        StructMethod* bestMatch = null;
        int bestScore = 999_999;

        foreach (ref StructMethod cand; candidates)
        {
            // Verifica se o método tem parâmetros variádicos
            bool hasVariadic = false;
            size_t minargs = cand.funcDecl.args.length;

            foreach (i, param; cand.funcDecl.args)
            {
                if (param.resolvedType is null)
                {
                    hasVariadic = true;
                    minargs = i;
                    break;
                }
            }

            // IMPORTANTE: O primeiro parâmetro é o 'self', então comparamos a partir do índice 1
            size_t expectedParamsWithoutSelf = cand.funcDecl.args.length > 0 ? 
                cast(int)cand.funcDecl.args.length - 1 : 0;

            // Validação de número de argumentos (SEM contar o self)
            if (hasVariadic)
            {
                size_t minArgsWithoutSelf = minargs > 0 ? minargs - 1 : 0;
                if (argTypes.length < minArgsWithoutSelf)
                    continue;
            }
            else
            {
                if (argTypes.length != expectedParamsWithoutSelf)
                    continue;
            }

            int currentScore = 0;
            bool compatible = true;

            size_t argsToCheck = hasVariadic ? (minargs > 0 ? minargs - 1 : 0) : expectedParamsWithoutSelf;

            // Compara os argumentos (pulando o primeiro que é o self)
            foreach (i; 0 .. argsToCheck)
            {
                // +1 para pular o self nos parâmetros da função
                Type expected = cand.funcDecl.args[i + 1].resolvedType;
                Type received = argTypes[i];

                if (received is null || expected is null)
                {
                    compatible = false;
                    break;
                }

                if (expected.toStr() == received.toStr())
                    currentScore += 0; // Match exato
                else if (expected.isCompatibleWith(received))
                    currentScore += 1; // Match com conversão implícita
                else
                {
                    compatible = false;
                    break;
                }
            }

            if (compatible && currentScore < bestScore)
            {
                bestScore = currentScore;
                bestMatch = &cand;
            }
        }

        return bestMatch;
    }

    Type getFieldType(string fieldName)
    {
        if (auto field = getField(fieldName))
            return field.resolvedType;
        return null;
    }

    bool hasField(string fieldName)
    {
        return (fieldName in fieldIndexMap) !is null;
    }

    bool hasMethod(string methodName)
    {
        return (methodName in methods) !is null;
    }

    size_t fieldCount()
    {
        return fields.length;
    }

    void rebuildFieldIndexMap()
    {
        fieldIndexMap.clear();
        foreach (i, field; fields)
            fieldIndexMap[field.name] = cast(int) i;
    }

    StructMethod[] getAllMethods()
    {
        StructMethod[] result;
        foreach (overloads; methods)
            result ~= overloads;
        return result;
    }
}

class PointerType : Type
{
    Type pointeeType;
    bool refConst = false;

    this(Type pointeeType, bool refConst = false)
    {
        this.pointeeType = pointeeType;
        this.refConst = refConst;
    }

    override bool isCompatibleWith(Type other, bool strict = true)
    {
        if (auto otherPtr = cast(PointerType) other)
        {
            if (toStr() == "void*" || other.toStr() == "void*")
                return true;
            
            if (toStr() == other.toStr())
                return true;
            
            if (auto thisStruct = cast(StructType) pointeeType)
            {
                if (auto otherStruct = cast(StructType) otherPtr.pointeeType)
                {
                    if (!strict)
                    {
                        if (thisStruct.compatibleCasts.canFind(otherStruct.name))
                            return true;
                        
                        if (thisStruct.hasStructuralCompatibility(otherStruct))
                            return true;
                    }
                }
            }
            
            return pointeeType.isCompatibleWith(otherPtr.pointeeType, strict);
        }
        
        if (!strict)
        {
            if (PrimitiveType primi = cast(PrimitiveType) other)
            {
                if (primi.baseType == BaseType.Int || 
                    primi.baseType == BaseType.Long || 
                    primi.baseType == BaseType.Char)
                    return true;
            }
        }
        
        return false;
    }

    override bool isPointer()
    {
        return true;
    }

    override string toStr()
    {
        return format("%s*", pointeeType.toStr());
    }

    override Type clone()
    {
        return new PointerType(pointeeType.clone(), refConst);
    }
}

/// Tipo função: (inteiro, texto): logico
class FunctionType : Type
{
    Type[] paramTypes;
    Type returnType;
    string mangled = "";

    this(Type[] paramTypes, Type returnType)
    {
        this.paramTypes = paramTypes;
        this.returnType = returnType;
    }

    override bool isCompatibleWith(Type other, bool strict = true)
    {
        // Compatibilidade com outra função
        if (auto otherFunc = cast(FunctionType) other)
        {
            // Número de parâmetros deve ser igual
            if (paramTypes.length != otherFunc.paramTypes.length)
                return false;

            // Tipo de retorno deve ser compatível
            if (!returnType.isCompatibleWith(otherFunc.returnType, strict))
                return false;

            // Todos os parâmetros devem ser compatíveis
            foreach (i, thisParamType; paramTypes)
            {
                Type otherParamType = otherFunc.paramTypes[i];
                if (!thisParamType.isCompatibleWith(otherParamType, strict))
                    return false;
            }

            // writeln("O: ", otherFunc.mangled);
            // writeln("M: ", mangled);

            if (otherFunc.mangled == "" && mangled != "")
                otherFunc.mangled = mangled;

            if (mangled == "" && otherFunc.mangled != "")
                mangled = otherFunc.mangled;

            return true;
        }

        // Compatibilidade com union type
        if (auto unionType = cast(UnionType) other)
            return unionType.isCompatibleWith(returnType, strict);

        return false;
    }

    override string toStr()
    {
        string params = paramTypes.map!(t => t is null ? "..." : t.toStr()).join(", ");
        return "(" ~ params ~ ") -> " ~ returnType.toStr();
    }

    override Type clone()
    {
        Type[] clonedParams = paramTypes.map!(t => t.clone()).array;
        return new FunctionType(clonedParams, returnType.clone());
    }
}

class EnumType : Type
{
    string name;
    // Maps member name to its integer value (e.g., "RED" -> 0)
    long[string] members;
    Type baseType = null;

    this(string name, long[string] members, Type baseType = null)
    {
        this.name = name;
        this.members = members;
        this.baseType = baseType;
    }

    override bool isEnum()
    {
        return true;
    }

    override bool isCompatibleWith(Type other, bool strict = true)
    {
        // Enums are strictly compatible with themselves
        if (auto otherEnum = cast(EnumType) other)
            return this.name == otherEnum.name;
        
        if (baseType !is null)
            return baseType.isCompatibleWith(other, strict);
        
        return false;
    }

    override string toStr()
    {
        return "enum " ~ name;
    }

    override Type clone()
    {
        return new EnumType(name, members.dup);
    }
    
    // Helper to check if a value is valid for this enum
    bool hasMember(string memberName)
    {
        return (memberName in members) !is null;
    }
    
    long getMemberValue(string memberName)
    {
        if (auto val = memberName in members)
            return *val;
        return -1; // Or throw error
    }
}

class UnionType : Type
{
    string name;
    StructField[] fields;
    private int[string] fieldIndexMap;
    string mangledName;

    this(string name, StructField[] fields, string mangledName = "")
    {
        this.name = name;
        this.fields = fields;
        if (mangledName == "")
            this.mangledName = name;

        foreach (i, field; fields)
            fieldIndexMap[field.name] = cast(int) i;
    }

    override bool isUnion()
    {
        return true;
    }
    
    override bool isCompatibleWith(Type other, bool strict = true)
    {
        if (other.toStr() == "void*") return true;
        if (auto otherUnion = cast(UnionType) other)
            return name == otherUnion.name;
        return false;
    }

    override string toStr()
    {
        return "union " ~ mangledName;
    }

    override Type clone()
    {
        return new UnionType(name, fields.dup, mangledName);
    }

    // Busca um campo pelo nome
    StructField* getField(string fieldName)
    {
        if (auto idx = fieldName in fieldIndexMap)
            return &fields[*idx];
        return null;
    }

    // Retorna o tipo de um campo
    Type getFieldType(string fieldName)
    {
        if (auto field = getField(fieldName))
            return field.resolvedType;
        return null;
    }

    // Verifica se tem um campo específico
    bool hasField(string fieldName)
    {
        return (fieldName in fieldIndexMap) !is null;
    }
    
    // Helper to get largest member for backend sizing
    Type getLargestMember()
    {
        // Implementation logic for backend sizing would go here
        // or be handled in a separate utility. 
        // Since Type doesn't have size info yet, this might be premature 
        // without a size calculator.
        return null; 
    }

    void rebuildFieldIndexMap()
    {
        fieldIndexMap.clear();
        foreach (i, field; fields)
            fieldIndexMap[field.name] = cast(int) i;
    }
}
