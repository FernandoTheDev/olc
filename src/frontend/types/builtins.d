module frontend.types.builtins;

import frontend.types.type;
import std.stdio;

class BuiltinTypes
{
    // Tipos primitivos básicos
    static PrimitiveType _Int;      // i32
    static PrimitiveType _Uint;     // u32
    static PrimitiveType _Long;     // i64
    static PrimitiveType _Ulong;    // u64
    static PrimitiveType _Short;    // i16
    static PrimitiveType _Ushort;   // u16
    static PrimitiveType _Byte;     // i8
    static PrimitiveType _Ubyte;    // u8
    static PrimitiveType _Float;    // f32
    static PrimitiveType _Double;   // f64
    static PrimitiveType _Bool;     // i1
    static PrimitiveType _Char;     // char
    static PrimitiveType _Void;     // void
    static PrimitiveType _Any;      // any
    
    // Tipos especiais
    static PointerType   _String;   // string (char*)
    static PrimitiveType _Null;
    
    // Mapa de aliases para lookup rápido
    static Type[string] aliases;

    static void initialize()
    {
        // Inicializar tipos signed
        _Byte = new PrimitiveType(BaseType.Byte);
        _Short = new PrimitiveType(BaseType.Short);
        _Int = new PrimitiveType(BaseType.Int);
        _Long = new PrimitiveType(BaseType.Long);
        
        // Inicializar tipos unsigned
        _Ubyte = new PrimitiveType(BaseType.Ubyte);
        _Ushort = new PrimitiveType(BaseType.Ushort);
        _Uint = new PrimitiveType(BaseType.Uint);
        _Ulong = new PrimitiveType(BaseType.Ulong);
        
        // Inicializar tipos de ponto flutuante
        _Float = new PrimitiveType(BaseType.Float);
        _Double = new PrimitiveType(BaseType.Double);
        
        // Inicializar outros tipos
        _Bool = new PrimitiveType(BaseType.Bool);
        _Char = new PrimitiveType(BaseType.Char);
        _Void = new PrimitiveType(BaseType.Void);
        _Any = new PrimitiveType(BaseType.Any);
        
        // Tipos especiais
        _String = new PointerType(new PrimitiveType(BaseType.Char));
        _Null = new PrimitiveType(BaseType.Void);

        registerAliases();
    }

    private static void registerAliases()
    {
        // Aliases de tipos signed (nome completo)
        aliases["byte"] = _Byte;
        aliases["short"] = _Short;
        aliases["int"] = _Int;
        aliases["long"] = _Long;
        
        // Aliases de tipos unsigned (nome completo)
        aliases["ubyte"] = _Ubyte;
        aliases["ushort"] = _Ushort;
        aliases["uint"] = _Uint;
        aliases["ulong"] = _Ulong;
        
        // Aliases de tipos de ponto flutuante
        aliases["float"] = _Float;
        aliases["double"] = _Double;
        
        // Outros tipos básicos
        aliases["bool"] = _Bool;
        aliases["char"] = _Char;
        aliases["void"] = _Void;
        aliases["any"] = _Any;
        
        // Tipos especiais
        aliases["string"] = _String;
        aliases["null"] = _Null;
        
        // Aliases estilo LLVM/C (signed)
        aliases["i8"] = _Byte;
        aliases["i16"] = _Short;
        aliases["i32"] = _Int;
        aliases["i64"] = _Long;
        
        // Aliases estilo LLVM/C (unsigned)
        aliases["u8"] = _Ubyte;
        aliases["u16"] = _Ushort;
        aliases["u32"] = _Uint;
        aliases["u64"] = _Ulong;
        
        // Aliases estilo LLVM/C (float)
        aliases["f32"] = _Float;
        aliases["f64"] = _Double;
        
        // Alias especial para bool
        aliases["i1"] = _Bool;
    }

    static bool isPrimitiveTypeName(string name)
    {
        return (name in aliases) !is null;
    }

    static Type getPrimitive(string name)
    {
        if (auto type = name in aliases)
            return *type;
        return null;
    }

    static string[] listPrimitives()
    {
        return aliases.keys;
    }
    
    static bool isSignedInteger(Type type)
    {
        if (auto prim = cast(PrimitiveType) type)
        {
            return prim.baseType == BaseType.Byte ||
                   prim.baseType == BaseType.Short ||
                   prim.baseType == BaseType.Int ||
                   prim.baseType == BaseType.Long;
        }
        return false;
    }
    
    static bool isUnsignedInteger(Type type)
    {
        if (auto prim = cast(PrimitiveType) type)
        {
            return prim.baseType == BaseType.Ubyte ||
                   prim.baseType == BaseType.Ushort ||
                   prim.baseType == BaseType.Uint ||
                   prim.baseType == BaseType.Ulong;
        }
        return false;
    }
    
    static bool isIntegerType(Type type)
    {
        return isSignedInteger(type) || isUnsignedInteger(type);
    }
    
    static bool isFloatingPoint(Type type)
    {
        if (auto prim = cast(PrimitiveType) type)
        {
            return prim.baseType == BaseType.Float ||
                   prim.baseType == BaseType.Double;
        }
        return false;
    }
    
    static bool isNumericType(Type type)
    {
        return isIntegerType(type) || isFloatingPoint(type);
    }
    
    // Retorna o tamanho em bits de um tipo primitivo
    static int getBitSize(Type type)
    {
        if (auto prim = cast(PrimitiveType) type)
        {
            switch (prim.baseType)
            {
                case BaseType.Bool:
                    return 1;
                case BaseType.Byte:
                case BaseType.Ubyte:
                    return 8;
                case BaseType.Short:
                case BaseType.Ushort:
                    return 16;
                case BaseType.Int:
                case BaseType.Uint:
                case BaseType.Float:
                    return 32;
                case BaseType.Long:
                case BaseType.Ulong:
                case BaseType.Double:
                    return 64;
                case BaseType.Char:
                    return 8;
                default:
                    return 0;
            }
        }
        return 0;
    }
    
    // Retorna o tipo signed correspondente a um tipo unsigned
    static Type getSignedVersion(Type type)
    {
        if (auto prim = cast(PrimitiveType) type)
        {
            switch (prim.baseType)
            {
                case BaseType.Ubyte:  return _Byte;
                case BaseType.Ushort: return _Short;
                case BaseType.Uint:   return _Int;
                case BaseType.Ulong:  return _Long;
                default: return type;
            }
        }
        return type;
    }
    
    // Retorna o tipo unsigned correspondente a um tipo signed
    static Type getUnsignedVersion(Type type)
    {
        if (auto prim = cast(PrimitiveType) type)
        {
            switch (prim.baseType)
            {
                case BaseType.Byte:  return _Ubyte;
                case BaseType.Short: return _Ushort;
                case BaseType.Int:   return _Uint;
                case BaseType.Long:  return _Ulong;
                default: return type;
            }
        }
        return type;
    }
}
