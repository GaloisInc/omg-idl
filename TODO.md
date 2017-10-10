# IDL Building Block Support

This is not a complete implementation of the IDL spec. As we are
motivated by supporting the DDS family of specs, We only support the
following IDL building blocks:

- Core Data Types (7.4.1)
- Interfaces — Basic (7.4.3)
- Anonymous Types (7.4.14)

## Interfaces - Full

    This building block adds the possibility to embed inside an
    interface declaration, declarations of types, constants and exceptions.

Let's try to think of a clean mapping of this into Rust, if it ever
becomes necessary.

# Technical Debt

This project is aimed at a particular purpose: generating Rust
bindings for DDS. As such, there are some corners we've cut in order
to get there sooner. This is a list of the main ones we know about:

- No filtering is done on emitted names to avoid clashes with Rust
  keywords. E.g., if you have in IDL `const long type = 2;`, your
  generated code won't compile.

- The weird scoping rules for introduced type names are not
  necessarily followed (see section 7.5.3 of the spec). This should
  only result in incorrect IDL being accepted, rather than correct IDL
  being accepted incorrectly.

- Exceptions aren't supported on interfaces. Mapping them to Rust
  could get messy in the case where an operation could throw more than
  one kind of exception: do we make a fresh `enum` to go on the `Err`
  side of each operation?

- `out` parameters should probably be returned as owned types in a
  tuple with the declared return type of an operation.

- Generated code readability: we should track more information about
  typedefs. Right now a typedef just resolves to the underlying type,
  so the underlying type shows up at reference sites in the generated
  code.

- Error reporting should be improved, particularly around constant
  evaluation.
