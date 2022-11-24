package parallelize

import (
	"bufio"
	"fixer/change"
	"go/ast"
	"go/token"
	"golang.org/x/tools/go/ast/astutil"
)

type Parallelize struct {
	fset      *token.FileSet
	positions map[string][]token.Position
}

func NewParallelize(fset *token.FileSet) *Parallelize {
	return &Parallelize{
		fset:      fset,
		positions: make(map[string][]token.Position),
	}
}

func (p *Parallelize) Add(reader *bufio.Reader, line string) ([]string, error) {
	if line != "PARALLELIZE" {
		return nil, nil
	}

	position, err := change.ReadPositionLine(reader)
	if err != nil {
		return nil, err
	}
	filename := position.Filename

	if positions, ok := p.positions[filename]; ok {
		p.positions[filename] = append(positions, position)
	} else {
		positions = []token.Position{position}
		p.positions[filename] = positions
	}
	return []string{filename}, nil
}

func (p *Parallelize) Patch(filename string, file *ast.File) *ast.File {
	tmp := astutil.Apply(file, func(cursor *astutil.Cursor) bool {
		return step1(p.fset, p.positions[filename], cursor)
	}, nil)
	return tmp.(*ast.File)
}

func wrapInGo(node ast.Stmt, pos token.Pos) *ast.GoStmt {
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

func containsPosition(positions []token.Position, position token.Position) bool {
	for _, pos := range positions {
		if pos == position {
			return true
		}
	}
	return false
}

type visit struct {
	fset      *token.FileSet
	positions []token.Position
}

func step1(fset *token.FileSet, positions []token.Position, cursor *astutil.Cursor) bool {
	node := cursor.Node()
	if node == nil {
		return true
	}

	switch op := node.(type) {
	case *ast.SendStmt:
		position := fset.Position(op.Arrow)
		position.Offset = 0
		if containsPosition(positions, position) {
			cursor.Replace(wrapInGo(op, op.Arrow))
		}
	case *ast.AssignStmt:
		for _, rhs := range op.Rhs {
			unaryExpr, ok := rhs.(*ast.UnaryExpr)
			if ok && unaryExpr.Op == token.ARROW {
				position := fset.Position(unaryExpr.OpPos)
				position.Offset = 0
				if containsPosition(positions, position) {
					cursor.Replace(wrapInGo(op, unaryExpr.OpPos))
					break
				}
			}
		}
	case *ast.ExprStmt:
		unaryExpr, ok := op.X.(*ast.UnaryExpr)
		if ok && unaryExpr.Op == token.ARROW {
			position := fset.Position(unaryExpr.OpPos)
			position.Offset = 0
			if containsPosition(positions, position) {
				cursor.Replace(wrapInGo(op, unaryExpr.OpPos))
			}
		}
	}
	return true
}
