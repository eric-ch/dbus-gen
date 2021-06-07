{{- define "enum_bindings"}}
  {{- $e := .}}
  {{- range .Values}}
e{{$e.Name}}_{{.Suffix}} :: {{$e.HaskellType}}
    {{- if eq $e.Type "s"}}
e{{$e.Name}}_{{.Suffix}} = "{{.Value}}"
    {{- else}}
e{{$e.Name}}_{{.Suffix}} = {{.Value}}
    {{- end}}
    {{- "\n"}}
  {{- end}}
{{- end -}}

module {{.PackageName}} (
{{- $first := true}}
{{- range $e_idx, $e_elem := .Node.Enums}}
  {{- range $e_elem.Values}}
  {{if $first}} {{$first = false}}{{else}},{{end}} e{{$e_elem.Name}}_{{.Suffix}}
  {{- end}}
{{- end}}
{{- range .Node.Interfaces}}
  {{- range $e_idx, $e_elem := .Enums}}
    {{- range $e_elem.Values}}
  {{if $first}} {{$first = false}}{{else}},{{end}} e{{$e_elem.Name}}_{{.Suffix}}
    {{- end}}
  {{- end}}
{{- end}}
) where

import Data.String
import Data.Word
import Data.Int
{{range .Node.Enums -}}
{{template "enum_bindings" .}}
{{- end}}
{{- range .Node.Interfaces}}
  {{- range .Enums}}
{{template "enum_bindings" .}}
  {{- end}}
{{- end}}
