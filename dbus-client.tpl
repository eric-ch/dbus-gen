{{- define "no_return_value" -}}
func ({{.ObjectInitial}} *{{.ObjectName}}) {{camelcase .GetMethod.Name false}}(
	{{- $len := len .GetMethod.Parameters | deduct}}

	{{- range $idx, $elem := .GetMethod.Parameters}}
		{{- if eq $idx $len}} 
			{{- $elem.Name}} {{$elem.GoType}})
		{{- else}}
			{{- $elem.Name}} {{$elem.GoType}}{{", "}}
		{{- end}}
	{{- else}}
		{{- ")"}}
	{{- end}} error {

	call := {{.ObjectInitial}}.Call("{{.GetInterface.Name}}" + ".{{.GetMethod.Name}}", 0

	{{- range $idx, $elem := .GetMethod.Parameters}}
		{{- if eq $idx $len}}
			{{- ", "}}{{- $elem.Name}})
		{{- else}}
			{{- ", "}}{{$elem.Name}}
		{{- end}}
	{{- else}}
		{{- ")"}}
	{{- end}}

	return call.Err
}

{{end}}

{{- define "return_value" -}}
func ({{.ObjectInitial}} *{{.ObjectName}}) {{camelcase .GetMethod.Name false}}(
        {{- $param_len := len .GetMethod.Parameters | deduct}}
        {{- $arg_len := len .GetMethod.Returns | deduct}}

        {{- range $idx, $elem := .GetMethod.Parameters}}
                {{- if eq $idx $param_len}}
                        {{- $elem.Name}} {{$elem.GoType}})
                {{- else}}
                        {{- $elem.Name}} {{$elem.GoType}}{{", "}}
                {{- end}}
        {{- else}}
                {{- ")"}}
        {{- end}}

        {{- " ("}}
        {{- range $idx, $elem := .GetMethod.Returns}}
                {{- if eq $idx $arg_len}}
                        {{- camelcase $elem.Name true}} {{$elem.GoType}}
                {{- else}}
                        {{- camelcase $elem.Name true}} {{$elem.GoType}}{{", "}}
                {{- end}}
        {{- end}}
        {{- ", err error) {"}}

        err = {{.ObjectInitial}}.Call("{{.GetInterface.Name}}" + ".{{.GetMethod.Name}}", 0

        {{- range $idx, $elem := .GetMethod.Parameters}}
                {{- if eq $idx $param_len}}
                        {{- ", "}}{{- $elem.Name}})
                {{- else}}
                        {{- ", "}}{{$elem.Name}}
                {{- end}}
        {{- else}}
                {{- ")"}}
        {{- end}}

        {{- ".Store("}}
        {{- range $idx, $elem := .GetMethod.Returns}}
                {{- if eq $idx $arg_len}}
                        {{- "&"}}{{- camelcase $elem.Name true}})
                {{- else}}
                        {{- camelcase $elem.Name true}}{{", "}}
                {{- end}}
        {{- end}}

        return
}

{{end}}

{{- define "properties" -}}
var {{camelcase .ShortInterfaceName true}}Properties = map[string]bool {
{{range $idx, $elem := .GetInterface.Properties}}
	{{- "\t\""}}{{$elem.Name}}{{"\": "}}
	{{- if eq $elem.Access "read"}}
		{{- "false"}}
	{{- else}}
		{{- "true"}}
	{{- end}}
	{{- ","}}
{{end -}}
}

func ({{.ObjectInitial}} *{{.ObjectName}}) Get{{camelcase .ShortInterfaceName true}}
	{{- "Property(name string) (dbus.Variant, error) {"}}

        if _, ok := {{camelcase .ShortInterfaceName true}}Properties[name]; ok {
                return {{.ObjectInitial}}.GetProperty("{{.GetInterface.Name}}" + "." + name)
        }

        return dbus.Variant{}, fmt.Errorf("Invalid property (%s)", name)
}

func ({{.ObjectInitial}} *{{.ObjectName}}) Set{{camelcase .ShortInterfaceName true}}
	{{- "Property(name string, value interface{}) error {"}}

        if writeable, ok := {{camelcase .ShortInterfaceName true}}Properties[name]; ok && writeable {
                return {{.ObjectInitial}}.SetProperty("{{.GetInterface.Name}}" + "." + name, value)
        }

        return fmt.Errorf("Invalid property (%s)", name)
}
{{end}}

{{- define "signals" -}}

{{- range $idx, $elem := .GetInterface.Signals}}
func Subscribe{{camelcase $elem.Name false}}(conn dbus.Conn, dest string) error {
        return conn.AddMatchSignal(dbus.WithMatchDestination(dest),
                dbus.WithMatchInterface("{{$.GetInterface.Name}}"),
                dbus.WithMatchMember("{{$elem.Name}}"))
}

func UnSubscribe{{camelcase $elem.Name false}}(conn dbus.Conn, dest string) error {
        return conn.RemoveMatchSignal(dbus.WithMatchDestination(dest),
                dbus.WithMatchInterface("{{$.GetInterface.Name}}"),
                dbus.WithMatchMember("{{$elem.Name}}"))
}

{{end}}
{{- end}}

{{- $top := . }}
{{- "package"}} {{.PackageName}}

import (
	"fmt"

        "github.com/godbus/dbus/v5"
)

type {{.ObjectName}} struct {
        dbus.BusObject
}

func New{{.ObjectName}}(conn *dbus.Conn, dest, path string) *{{.ObjectName}} {
        return &{{.ObjectName}}{conn.Object(dest, dbus.ObjectPath(path))}
}

{{range $i_idx, $i_elem := .Node.Interfaces}}
	{{- $state := $.SetInterfaceIdx $i_idx -}}
/* Interface {{$i_elem.Name}} */{{"\n"}}
	{{- range $m_idx, $m_elem := $i_elem.Methods}}
		{{- $state := $.SetMethodIdx $m_idx}}

		{{- $return_count := len $m_elem.Returns}}
		{{- if gt $return_count 0}}
			{{- template "return_value" $top }}
		{{- else}}
			{{- template "no_return_value" $top }}
		{{- end}}
	{{- end}}

	{{- $prop_count := len $i_elem.Properties}}
	{{- if gt $prop_count 0 }}
		{{- template "properties" $top}}
	{{- end}}

	{{- $sig_count := len $i_elem.Signals}}
	{{- if gt $sig_count 0 }}
		{{- template "signals" $top}}
	{{- end}}
{{- end}}
