module backend.builder;

import middle.mir.mir, middle.hir.hir;
import backend.llvm.codegen;
import backend.vm.codegen;
import target; // Import do TargetInfo
import cli : CompilerConfig; // Importa a struct de configuração
import std.process : executeShell, escapeShellCommand;
import std.stdio : writeln, writefln, stderr;
import std.file : exists, remove;
import std.format : format;
import std.array : split;

enum BackendType 
{
    LLVM
}

class BackendBuilder 
{
    static void buildVM(HirProgram hir)
    {
        VMBackend codegen = new VMBackend();
        codegen.generate(hir);
        codegen.buildVM().run();
    }

    static void buildLLVM(MirProgram mir, HirProgram hir, TargetInfo target, CompilerConfig config) 
    {   
        auto codegen = new LLVMBackend();
        codegen.initializeTargetData(target);
        codegen.generate(mir, hir);
        string triple = config.targetTriple == "" ? target.triple : config.targetTriple;
        
        // Define o nome do arquivo intermediário
        // Se a saída for "myapp", o IR será "myapp.ll"
        string irFilename = config.outputFile ~ ".ll";
        codegen.dumpToFile(irFilename);

        string[] cmdArgs = [
            "clang",
            irFilename,
            "-o", config.outputFile,
            format("-O%d", config.optLevel), // Nível de otimização
            "-lm",                           // Linka com Math Library (libc)
            "-Wno-override-module",          // Silencia warning comum de IR gerado manualmente
            "-target",
            triple
        ];

        if (config.compilerArg != "")
            cmdArgs ~= config.compilerArg.split();

        string cmd = escapeShellCommand(cmdArgs);
        auto result = executeShell(cmd);
        
        if (result.status != 0) 
        {
            stderr.writeln("\n\033[1;31m[CLANG ERROR]\033[0m Compilation failed:");
            stderr.writeln(result.output);
            stderr.writefln("Intermediate file preserved at: %s", irFilename);
            import core.stdc.stdlib : exit;
            exit(result.status);
        }
        
        if (!config.emitLLVM && exists(irFilename))
        {
            if (config.verbose) writefln("[Backend] Cleaning up %s...", irFilename);
            remove(irFilename);
        }
    }
}
