# Gone
Gone is a programming language built on plagiarising features from other languages.
It tries to stick mostly to the semantics of Go.

## Goals
- Productivity - Simple ideas should be simple to execute. The language should
  get out of the user's way as much as possible. It should also be fast to compile.
- Gradual typing - Programs can be fully untyped, fully typed, or any combination
  of the two.
- Testability - It should be really easy to test anything.

## Anti-Goals
- Performance - High performance code is hard, and this language purposely does
  nothing to help you with that. There are no optimizations, no customizations
  for the runtime environment, no way to remove global garbage collection.
- Binary compatibility - Programs will be compiled from source each time.
- Incremental compilation - Programs will be compiled from source each time.

## Standard Library Goals
- Performance - The standard library should be high performance, and should depend
  on as few language runtime features as possible.
- Productivity - The standard library should make it easy to write simple programs,
  and easier to write complex programs.
- Correctness - The standard library should be correct.

## Standard Library Anti-Goals
- Gradual typing - Everything in the standard library is typed.
