// This is the BCPL to WebAssembly Compiler

// Implemented by Martin Richards (c) Oct 2021
// WebAssembly modifications by Jason Sobotka Aug 2024

// The compiler is now compiled as three sections.

//OCODE is directly translated to WebAssembly which itself is a simple stack machine

GET "com/bcplsyn.b"

.

GET "com/bcpltrn.b"

.

GET "com/bcplcgwasm.b"
