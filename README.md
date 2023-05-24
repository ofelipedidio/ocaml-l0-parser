# OCaml L0 Parser

## Setup 

Include contents of `parser.ml` in the source code of your project.

## Examples

### Parser

L0 source code can be parsed using `Parser.parse`:

```ocaml 
let source_code = "if iszero pred 0 then 0 else succ 0"
let ast: expr = Parser.parse source_code
```

> Please note this example may raise `Parser.ParserError` if the source code is not syntactically correct.

### REPL

> *Warning*: The REPL does not function on TryOCaml 

To use the REPL, you can include the following code in your source code:

```ocaml
let () = Parser.repl (fun _ -> ())
```

The following example shows how to execute a function after the AST is parsed:

```ocaml 
let on_ast_parsed (ast: expr): unit =
    print_endline (expr_to_string ast)

let () = Parser.repl on_ast_parsed
```
