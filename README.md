# Gone
Gone will be a programming language built on plagiarising features from other
languages.

## Features
All of these are todo.

- Generators from Python/JavaScript/Rust unstable
- For loops from Java
- Closures from Go
- Logging/printing from Go
- Structs from Go
- Enums from Rust
- Pattern matching from Rust
- Simple type inference
- Checked type casting from Java and Go
- Meta-classes from Zig/Jai
- Reflection from Java
- "Any" type from Jai
- Macros from Jai
- Structural Polymorphism from Jai
- Defer from Jai
- Light conditional compilation from Jai
- Light operator overloading from Jai
- Polymorphic function deduplication?
- Keyword arguments and default arguments from Jai

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

