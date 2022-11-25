package change

import (
	"bufio"
	"go/ast"
	"go/token"
	"strconv"
	"strings"
)

type Change interface {
	Add(lineReader *bufio.Reader, line string) ([]string, error)
	Patch(filename string, file *ast.File) *ast.File
}

func ParsePosition(s string) token.Position {
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

func ReadPositionLine(reader *bufio.Reader) (token.Position, error) {
	data, _, err := reader.ReadLine()
	if err != nil {
		return token.Position{}, err
	}
	dataString := string(data)
	return ParsePosition(dataString), nil
}
func WrapInGo(node ast.Stmt, pos token.Pos) *ast.GoStmt {
	return &ast.GoStmt{
		Go: pos,
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
