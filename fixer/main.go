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

var fset = token.NewFileSet()

func main() {
	reader := bufio.NewReader(os.Stdin)
	all_filenames := make(map[string]struct{})
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
					all_filenames[filename] = struct{}{}
				}
			}
		}
	}

	err := processFile(all_filenames, changes)
	if err != nil {
		panic(err)
	}
}

const parserMode = parser.ParseComments

func processFile(filenames map[string]struct{}, changes []change.Change) error {
	var f *os.File
	var err error

	for filename := range filenames {
		err := func() error {
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
			ast.Print(fset, srcFile)

			file, err := parser.ParseFile(fset, filename, src, parserMode)
			if err != nil {
				return err
			}

			for _, c := range changes {
				file = c.Patch(filename, file)
			}

			newSrc, err := gofmtFile(file)
			if err != nil {
				return err
			}

			if true {
				orig := gofmt(srcFile)
				newSrc := string(newSrc)
				edits := myers.ComputeEdits(span.URIFromPath(filename), orig, newSrc)
				fmt.Print(gotextdiff.ToUnified(filename, "fixed/"+filename, orig, edits))
				fmt.Print("\n\n")
				fmt.Print(newSrc)
				return nil
			}

			// _, err = os.Stdout.Write(newSrc)
			// return os.WriteFile(f.Name(), newSrc, 0)
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
