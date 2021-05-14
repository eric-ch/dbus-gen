// SPDX-License-Identifier: BSD-3-Clause
package main

import (
	"errors"
	"os"
	"path/filepath"
	"text/template"

	flag "github.com/spf13/pflag"
)

func OpenOutput(name, destDir string) (*os.File, error) {
	var (
		err    error
		output *os.File
	)

	outPath := filepath.Join(destDir, name+".go")
	output, err = os.OpenFile(outPath, os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		return nil, err
	}

	return output, nil
}

func main() {
	var (
		tmplFile = flag.StringP("template", "t", "", "code template")
		xmlFile  = flag.StringP("input", "i", "", "XML DBus IDL file")
		destDir  = flag.StringP("dest", "d", ".", "destination directory for code")
		pkgName  = flag.StringP("package", "p", "", "go package name")
		intfBase = flag.StringP("intf-base", "b", "", "base name of the DBus interface")

		idl    *Idl
		err    error
		output *os.File
	)

	flag.Parse()

	if *tmplFile == "" {
		panic(errors.New("no template file provided"))
	}

	idl, err = NewIdl(*xmlFile)
	if err != nil {
		panic(err)
	}

	output, err = OpenOutput(idl.Name, *destDir)
	if err != nil {
		panic(err)
	}
	defer output.Close()

	t := NewTemplateState(idl.Node, *pkgName, idl.Name, *intfBase)

	funcMap := template.FuncMap{
		"deduct":     Deduct,
		"increment":  Increment,
		"initial":    Initial,
		"camelcase":  CamelCase,
		"underscore": Underscore,
		"hyphen":     Hyphen,
	}

	tmpl := template.Must(template.New(*tmplFile).Funcs(funcMap).ParseFiles(*tmplFile))

	err = tmpl.Execute(output, t)
	if err != nil {
		panic(err)
	}
}
