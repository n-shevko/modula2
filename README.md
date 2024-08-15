Implementation of compiler for tiny subset of Modula 2 to learn basics of compiler engineering.
LLVM IR was used as a target language.

### What was done

1. Implemented [peg packrat parser generator](https://en.wikipedia.org/wiki/Packrat_parser)
   It consumes file like [this](https://github.com/n-shevko/modula2/blob/main/modula_2/grammar.txt)
   and generates:
   - ast type definitions like [these](https://github.com/n-shevko/modula2/blob/main/modula_2/RawAst.fs)
   - parser file like [this](https://github.com/n-shevko/modula2/blob/main/modula_2/Parser.fs)
   The result parser file has function parse which can parse source code and generate ast object. 
   Parser generator supports basic quantifiers like: *+?
   Nonterminal in {} means that it is entry point (top level) nonterminal.

2. Ast [->](https://github.com/n-shevko/modula2/blob/main/modula_2/Cfg.fs) CFG pass 
   
3. CFG [->](https://github.com/n-shevko/modula2/blob/main/modula_2/Ssa.fs) CFG in SSA form pass.
   Inspired by "Simple and Efficient Construction of Static Single Assignment Form (Braun)"

### Known issues

- Compiler doesn't have any garbage collection
- Implemented only arrays, structs, integers. 
- Elimination of left recursion from grammar works only if left recursion appears in the first branch
- Syntax error reports are incomprehensible :) [This](https://github.com/n-shevko/modula2/blob/main/parser/ReportSyntaxErrors.fs) thing should be definitely rewritten 

### Other
Any modification of an array\structure copy entire agregate and then modify only needed subelement. 
This is done on purpose for potential CPS conversion in the future.
