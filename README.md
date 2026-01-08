<p align="center"><img src="assets/olc.png" alt="OLC" width="400"></p>

---

**OLC (Orn LLVM Compiler)** is a high-performance alternative implementation for the Orn programming language [ORN](https://github.com/Blopaa/Orn), written in D.

Unlike the reference compiler (written in C, focused on simplicity and direct assembly generation), **OLC** adopts a modern modular architecture with support for multiple backends, using **LLVM** for optimized native code generation.

## Objective

The goal of OLC is to provide a robust compilation environment for the Orn language. The project philosophy is clear:

* **Official Orn Compiler:** Reference language, bootstrap, and simplicity (analogous to *GCC* or *DMD*).

* **OLC:** Performance, optimization, and development tools (analogous to *Clang* or *LDC*).

OLC strictly adheres to the Orn language specification, focusing on implementation quality and diagnostics, without creating incompatible dialects.

## Supported Backends

The compiler supports different compilation targets for different needs:

* **LLVM (Default):** Generates highly optimized native binaries.

* **VM:** Integrated virtual machine for rapid development, debugging, and interpreted execution. *(Under development)*
* **Assembly:** Direct machine code generation (x86_64). *(Under development)*
* **JIT:** Just-In-Time compilation for dynamic execution. *(Under development)*

## Basic Usage

```bash
# Compile to native binary (using LLVM)
olc main.orn
# Run using the internal VM (without generating binary)
olc main.orn --vm
```
