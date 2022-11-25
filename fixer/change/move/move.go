package move

import (
	"bufio"
	"fixer/change"
	"go/ast"
	"go/token"
	"golang.org/x/tools/go/ast/astutil"
)

type FromToStruct struct {
	// TODO: Deal with from/to statements in different files
	from     token.Position
	fromStmt *ast.SendStmt
	to       token.Position
}

type Move struct {
	fset          *token.FileSet
	fromPositions map[string][]*FromToStruct
	toPositions   map[string][]*FromToStruct
}

func NewMove(fset *token.FileSet) *Move {
	return &Move{
		fset:          fset,
		fromPositions: make(map[string][]*FromToStruct),
		toPositions:   make(map[string][]*FromToStruct),
	}
}

func (m *Move) Add(reader *bufio.Reader, line string) ([]string, error) {
	if line != "MOVE" {
		return nil, nil
	}

	fromPosition, err := change.ReadPositionLine(reader)
	if err != nil {
		return nil, err
	}
	fromFilename := fromPosition.Filename

	toPosition, err := change.ReadPositionLine(reader)
	if err != nil {
		return nil, err
	}
	toFilename := toPosition.Filename

	fromTo := FromToStruct{
		from: fromPosition,
		to:   toPosition,
	}

	var fromToList []*FromToStruct
	var ok bool

	if fromToList, ok = m.fromPositions[fromFilename]; ok {
		fromToList = append(fromToList, &fromTo)
	} else {
		fromToList = []*FromToStruct{&fromTo}
	}
	m.fromPositions[fromFilename] = fromToList

	if fromToList, ok = m.toPositions[toFilename]; ok {
		fromToList = append(fromToList, &fromTo)
	} else {
		fromToList = []*FromToStruct{&fromTo}
	}
	m.toPositions[toFilename] = fromToList

	return []string{fromFilename, toFilename}, nil
}

func (p *Move) Patch(filename string, file *ast.File) *ast.File {
	tmp := astutil.Apply(file, func(cursor *astutil.Cursor) bool {
		return step1(p.fset, p.fromPositions[filename], cursor)
	}, nil)
	tmp = astutil.Apply(file, func(cursor *astutil.Cursor) bool {
		return step2(p.fset, p.toPositions[filename], cursor)
	}, nil)
	return tmp.(*ast.File)
}

func containsFrom(fromToList []*FromToStruct, fromPosition token.Position) *FromToStruct {
	for _, fromTo := range fromToList {
		if fromTo.from == fromPosition {
			return fromTo
		}
	}
	return nil
}

func containsTo(fromToList []*FromToStruct, toPosition token.Position) *FromToStruct {
	for _, fromTo := range fromToList {
		if fromTo.to == toPosition {
			return fromTo
		}
	}
	return nil
}

func step1(fset *token.FileSet, fromPositions []*FromToStruct, cursor *astutil.Cursor) bool {
	node := cursor.Node()
	if node == nil {
		return true
	}
	var op, ok = node.(*ast.SendStmt)
	if ok {
		position := fset.Position(op.Arrow)
		position.Offset = 0
		if fromTo := containsFrom(fromPositions, position); fromTo != nil {
			fromTo.fromStmt = op
			fromTo.fromStmt.Arrow = 0
			cursor.Delete()
		}
	}
	return true
}

func step2(fset *token.FileSet, toPositions []*FromToStruct, cursor *astutil.Cursor) bool {
	node := cursor.Node()
	if node == nil {
		return true
	}
	switch op := node.(type) {
	case *ast.SendStmt:
	case *ast.AssignStmt:
		for _, rhs := range op.Rhs {
			unaryExpr, ok := rhs.(*ast.UnaryExpr)
			if ok && unaryExpr.Op == token.ARROW {
				position := fset.Position(unaryExpr.OpPos)
				position.Offset = 0
				if fromTo := containsTo(toPositions, position); fromTo != nil {
					cursor.InsertBefore(change.WrapInGo(fromTo.fromStmt, unaryExpr.OpPos))
				}
			}
		}
	case *ast.ExprStmt:
		unaryExpr, ok := op.X.(*ast.UnaryExpr)
		if ok && unaryExpr.Op == token.ARROW {
			position := fset.Position(unaryExpr.OpPos)
			position.Offset = 0
			if fromTo := containsTo(toPositions, position); fromTo != nil {
				cursor.InsertBefore(change.WrapInGo(fromTo.fromStmt, unaryExpr.OpPos))
			}
		}
	}
	return true
}
