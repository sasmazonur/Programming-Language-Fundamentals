# Prolog
## Description
MiniMiniLogo is a simplified version of the MiniLogo language. Which is itself a simplified version of the Logo language for programming simple 2D graphics.

MiniMiniLogo is like MiniLogo, except that it doesn’t have macros, variables, or addition. This leaves only a very simple syntax remaining.

### Part 1. Family relations
This part, defines several Prolog predicates that describe family relationships. It builds blocks that four basic predicates: `female/1`, `male/1`, `married/2`, and `parent/2`. The provided template uses these basic predicates to encode the following family tree, which used for testing the definitions.

### Part 2. Language Implementation
It definess two predicates to implement a simple stack-based language.

##### cmd/3
Predicate `cmd/3`, which describes the effect of a command on the stack. That is, the predicate `cmd(C,S1,S2)` means that executing command C with stack S1 produces stack S2.

##### prog/3
Predicate `prog/`3, which describes the effect of a program on the stack. That is, the predicate `prog(P,S1,S2)` means that executing program P with stack S1 produces stack S2.
