package main

import (
	"bufio"
	"bytes"
	"fixer/change"
	"fixer/change/move"
	"fixer/change/parallelize"
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
)

func main() {
	fset := token.NewFileSet()
	reader := bufio.NewReader(os.Stdin)
	allFilenames := make(map[string]struct{})
	changes := []change.Change{
		parallelize.NewParallelize(fset),
		move.NewMove(fset),
	}
	for {
		lineBytes, _, err := reader.ReadLine()
		if err == io.EOF {
			break
		}
		if err != nil {
			panic(err)
		}
		line := string(lineBytes)
		for _, c := range changes {
			filenames, err := c.Add(reader, line)
			if err != nil {
				panic(err)
			}
			if filenames != nil {
				for _, filename := range filenames {
					allFilenames[filename] = struct{}{}
				}
			}
		}
	}

	err := processFile(allFilenames, fset, changes)
	if err != nil {
		panic(err)
	}
}

const parserMode = parser.ParseComments

func processFile(filenames map[string]struct{}, fset *token.FileSet, changes []change.Change) error {
	var f *os.File
	var err error

	for filename := range filenames {
		err := func() error {
			f, err = os.Open(filename)
			if err != nil {
				return err
			}
			defer func(f *os.File) {
				err := f.Close()
				if err != nil {
					panic(err)
				}
			}(f)

			src, err := io.ReadAll(f)
			if err != nil {
				return err
			}

			srcFile, err := parser.ParseFile(fset, filename, src, parserMode)
			if err != nil {
				return err
			}
			updatedFile, err := parser.ParseFile(fset, filename, src, parserMode)
			if err != nil {
				return err
			}

			for _, c := range changes {
				updatedFile = c.Patch(filename, updatedFile)
			}

			updatedSrc, err := gofmtFile(updatedFile, fset)
			if err != nil {
				return err
			}

			if true {
				orig := gofmt(srcFile, fset)
				newSrc := string(updatedSrc)
				edits := myers.ComputeEdits(span.URIFromPath(filename), orig, newSrc)
				fmt.Print(gotextdiff.ToUnified(filename, "fixed/"+filename, orig, edits))
				fmt.Print("\n\n")
				fmt.Print(newSrc)
				return nil
			}

			// _, err = os.Stdout.Write(updatedSrc)
			// return os.WriteFile(f.Name(), updatedSrc, 0)
			if err != nil {
				return err
			}

			return nil
		}()
		if err != nil {
			return err
		}

	}

	return nil
}

func gofmtFile(f *ast.File, fset *token.FileSet) ([]byte, error) {
	var buf bytes.Buffer
	if err := format.Node(&buf, fset, f); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func gofmt(n any, fset *token.FileSet) string {
	var gofmtBuf bytes.Buffer
	if err := format.Node(&gofmtBuf, fset, n); err != nil {
		return "<" + err.Error() + ">"
	}
	return gofmtBuf.String()
}
