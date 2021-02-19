# Gone
Gone is a programming language built on plagiarising features from other languages.

## Features
All of these are todo.

- Structs from Go
- Enums from Rust
- Pattern matching from Rust
- For loops from Jai
- Gradual typing
- type inference from Rust
- Checked type casting from Java and Go
- Meta-classes from Zig/Jai
- Reflection from Java
- `any` type from TypeScript
- Garbage collection from GO
- Function overloading from Jai/C++/Java
- Macros from Jai
- Structural Polymorphism from Jai
- Defer from Jai
- Light conditional compilation from Rust
- Light operator overloading from Jai

## Goals
The goal of this language is to make it easier to ship medium quality software. Thus,
the philosophy of the compiler should be to optimize first and foremost for:

- **Fun** - This language should be fun to write in, and fun to develop as well!
- **Testability** - If it's not tested, it's not correct. The language should:
  - make fuzzing easy at the function granularity
  - make writing tests easier
  - make running tests easy
- **Productivity** - the language should:
  - make simple ideas easy to implement
  - compile quickly
  - get out of the programmer's way when prototyping
  - make debugging easier than programming

## Anti-Goals
- **Performance** - High performance code is hard, and this language purposely does
  nothing to help you with that. There are no implicit optimizations, no customizations
  for the runtime environment, no way to remove global garbage collection. Attempts
  generate high performance code *will* require lots of manual work, and *will not*
  in most cases succeed.
- **Binary compatibility** - Programs will be compiled from source each time.
- **Incremental compilation** - Programs will be compiled from source each time.

## Implementation Problems
- How do we make garbage collection behave at runtime like manual memory management?
  - Objects typically lies in a few patterns
    - Arrays
    - Trees
    - Graphs

    Ideally, we'd support all of them, but that's probably a bit too hard.

- How do we do lifetime analysis, especially for recursive functions, without annotations?
- How do we handle types at compile time?

## Architecture
Compiler passes:

1. Lexing
2. Parsing
3. Type checking
4. Memory management (insert GC calls, check lifetimes)
5. Code generation
