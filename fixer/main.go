package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/hexops/gotextdiff"
	"github.com/hexops/gotextdiff/myers"
	"github.com/hexops/gotextdiff/span"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"io"
	"os"
	"strconv"
	"strings"
)

func parsePosition(s string) token.Position {
	split := strings.Split(s, ":")
	if len(split) != 3 {
		panic("Invalid position")
	} else {
		line, err := strconv.Atoi(split[1])
		if err != nil {
			panic("Invalid position (Line not a number)")
		}
		column, err := strconv.Atoi(split[2])
		if err != nil {
			panic("Invalid position (Column not a number)")
		}
		return token.Position{
			Filename: split[0],
			Offset:   0,
			Line:     line,
			Column:   column,
		}
	}
}

func main() {
	reader := bufio.NewReader(os.Stdin)
	changes := make(map[string][]token.Position)
	for {
		lineBytes, _, err := reader.ReadLine()
		line := string(lineBytes)
		if err != nil {
			break
		}
		if strings.HasPrefix(line, "PARALLELIZE ") {
			position := parsePosition(line[len("PARALLELIZE "):])
			changes[position.Filename] = append(changes[position.Filename], position)
		}
	}

	processFile(changes)
}

var fset = token.NewFileSet()

const parserMode = parser.ParseComments

type FVisitor struct {
	fset      *token.FileSet
	positions []token.Position
}

func wrapInGo(node ast.Stmt) *ast.GoStmt {
	return &ast.GoStmt{
		Go: 0,
		Call: &ast.CallExpr{
			Fun: &ast.FuncLit{
				Type: &ast.FuncType{
					Func:       0,
					TypeParams: nil,
					Params: &ast.FieldList{
						Opening: 0,
						List:    nil,
						Closing: 0,
					},
					Results: nil,
				},
				Body: &ast.BlockStmt{
					Lbrace: 0,
					List:   []ast.Stmt{node},
					Rbrace: 0,
				},
			},
			Lparen:   0,
			Args:     nil,
			Ellipsis: 0,
			Rparen:   0,
		},
	}

}

func containsPosition(positions []token.Position, position token.Position) bool {
	for _, pos := range positions {
		if pos == position {
			return true
		}
	}
	return false
}

func (f FVisitor) Visit(node ast.Node) ast.Visitor {
	if node == nil {
		return f
	}
	var n, ok = node.(*ast.BlockStmt)
	if ok {
		for i, stmt := range n.List {
			switch op := stmt.(type) {
			case *ast.SendStmt:
				position := fset.Position(op.Arrow)
				position.Offset = 0
				if containsPosition(f.positions, position) {
					n.List[i] = wrapInGo(op)
				}
			case *ast.ExprStmt:
				unaryExpr, ok := op.X.(*ast.UnaryExpr)
				if ok && unaryExpr.Op == token.ARROW {
					position := fset.Position(unaryExpr.OpPos)
					position.Offset = 0
					if containsPosition(f.positions, position) {
						n.List[i] = wrapInGo(op)
					}
				}

			default:
				ast.Walk(f, stmt)
			}
		}
		return nil
	}
	return f
}

func processFile(changes map[string][]token.Position) error {
	var f *os.File
	var err error

	for filename, positions := range changes {
		f, err = os.Open(filename)
		if err != nil {
			return err
		}
		defer f.Close()

		src, err := io.ReadAll(f)
		if err != nil {
			return err
		}

		srcFile, err := parser.ParseFile(fset, filename, src, parserMode)
		if err != nil {
			return err
		}

		file, err := parser.ParseFile(fset, filename, src, parserMode)
		if err != nil {
			return err
		}

		ast.Walk(FVisitor{fset: fset, positions: positions}, file)

		newSrc, err := gofmtFile(file)
		if err != nil {
			return err
		}

		if true {
			orig := gofmt(srcFile)
			newSrc := string(newSrc)
			edits := myers.ComputeEdits(span.URIFromPath(filename), orig, newSrc)
			fmt.Print(gotextdiff.ToUnified(filename, "fixed/"+filename, orig, edits))
			return nil
		}

		_, err = os.Stdout.Write(newSrc)
		// return os.WriteFile(f.Name(), newSrc, 0)
		if err != nil {
			return err
		}
	}

	return nil
}

func gofmtFile(f *ast.File) ([]byte, error) {
	var buf bytes.Buffer
	if err := format.Node(&buf, fset, f); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func gofmt(n any) string {
	var gofmtBuf bytes.Buffer
	if err := format.Node(&gofmtBuf, fset, n); err != nil {
		return "<" + err.Error() + ">"
	}
	return gofmtBuf.String()
}
