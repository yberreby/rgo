https://play.golang.org/p/Xne3T3CL7d

```go
package main

import (
	"go/ast"
	"go/parser"
	"go/token"
)

func main() {
	// src is the input for which we want to print the AST.
	src := `
package main

func foo(x (func(a ...interface{}) (n int, err error))) {}

func main() {
	println("Hello, World!")
	foo(fmt.Println)
}
`

	// Create the AST by parsing src.
	fset := token.NewFileSet() // positions are relative to fset
	f, err := parser.ParseFile(fset, "", src, 0)
	if err != nil {
		panic(err)
	}

	// Print the AST.
	ast.Print(fset, f)
}
```

Excerpt from the output:

```
1: *ast.ExprStmt {
   160  .  .  .  .  .  .  X: *ast.CallExpr {
   161  .  .  .  .  .  .  .  Fun: *ast.Ident {
   162  .  .  .  .  .  .  .  .  NamePos: 8:2
   163  .  .  .  .  .  .  .  .  Name: "foo"
   164  .  .  .  .  .  .  .  .  Obj: *(obj @ 11)
   165  .  .  .  .  .  .  .  }
   166  .  .  .  .  .  .  .  Lparen: 8:5
   167  .  .  .  .  .  .  .  Args: []ast.Expr (len = 1) {
   168  .  .  .  .  .  .  .  .  0: *ast.SelectorExpr {
   169  .  .  .  .  .  .  .  .  .  X: *ast.Ident {
   170  .  .  .  .  .  .  .  .  .  .  NamePos: 8:6
   171  .  .  .  .  .  .  .  .  .  .  Name: "fmt"
   172  .  .  .  .  .  .  .  .  .  }
   173  .  .  .  .  .  .  .  .  .  Sel: *ast.Ident {
   174  .  .  .  .  .  .  .  .  .  .  NamePos: 8:10
   175  .  .  .  .  .  .  .  .  .  .  Name: "Println"
   176  .  .  .  .  .  .  .  .  .  }
   177  .  .  .  .  .  .  .  .  }
   178  .  .  .  .  .  .  .  }
   179  .  .  .  .  .  .  .  Ellipsis: -
   180  .  .  .  .  .  .  .  Rparen: 8:17
   181  .  .  .  .  .  .  }
   182  .  .  .  .  .  }
```

MethodExpr < SelectorExpr, basically. I thought the Go AST would differentiate
between method expressions (`Type.MethodName`) and selector expressions
(`someExpr.someField`, `foo().bar`...), but it doesn't, because treating them as
the same thing makes compilation easier...


