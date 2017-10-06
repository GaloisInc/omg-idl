# IDL Building Block Support

This is not a complete implementation of the IDL spec. We only support
the following IDL building blocks:

- Core Data Types (7.4.1)
- Interfaces — Basic (7.4.3)
- Anonymous Types (7.4.14)

# Technical Debt

This project is aimed at a particular purpose: generating Rust
bindings for DDS. As such, there are some corners we've cut in order
to get there sooner. This is a list of the main ones we know about:

- The weird scoping rules for introduced type names are not
  necessarily followed (see section 7.5.3 of the spec). This should
  only result in incorrect IDL being accepted, rather than correct IDL
  being accepted incorrectly.
