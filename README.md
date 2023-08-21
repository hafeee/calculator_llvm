# Small LLVM Calculator

This is a small calculator for the interview process at Exaloop. It supports the following operands:

- \- (Subtraction)
- \+ (Addition)
- \* (Multiplication)
- \/ (Division)
- \< (Less Than)
- \> (Greater Than)
- \= (Equality Check)

Commands should end with a semicolon (;) to indicate the end of an expression. For example:

1 + 15 * 3 - 6;


## Build and Run

To build the calculator, use the following command:

```bash
clang++ -g calculator.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o calculator

```

To run the calculator, execute:
```bash
./calculator
```

