{{- define "enum_bindings"}}
  {{- $e := .}}
  {{- range .Values}}

e{{$e.Name}}_{{.Suffix}} :: {{$e.HaskellType}}
    {{- if eq $e.Type "s"}}
e{{$e.Name}}_{{.Suffix}} = "{{.Value}}"
    {{- else}}
e{{$e.Name}}_{{.Suffix}} = {{.Value}}
    {{- end}}
  {{- end}}
{{- end -}}

module {{.PackageName}} (
{{- $first := true}}
{{- range $e_idx, $e_elem := .Node.Enums}}
  {{- range .Values}}
  {{if $first}}{{$first = false}}{{else}}, {{end}}e{{$e_elem.Name}}_{{.Suffix}}
  {{- end}}
{{- end}}
{{- range $i_idx, $i_elem := .Node.Interfaces}}
  {{- range $e_idx, $e_elem := .Enums}}
    {{- range .Values}}
  {{if $first}}{{$first = false}}{{else}}, {{end}}e{{$e_elem.Name}}_{{.Suffix}}
    {{- end}}
  {{- end}}
  {{- range $m_idx, $m_elem := .Methods}}
  {{if $first}}{{$first = false}}{{else}}, {{end}}{{camelcase $i_elem.Name true}}{{camelcase .Name false}}
  {{- end}}
  {{- range $p_idx, $p_elem := .Properties}}
  {{if $first}}{{$first = false}}{{else}}, {{end}}{{camelcase $i_elem.Name true}}Get{{camelcase .Name false}}
    {{- if eq .Access "readwrite"}}
  , {{camelcase $i_elem.Name true}}Set{{camelcase .Name false}}
    {{- end}}
  {{- end}}
{{- end}}
) where

import Data.Maybe
import Data.String
import Data.Int
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as B
import Rpc.Core

mkcall :: String -> String -> String -> String -> [Variant] -> RpcCall
mkcall service objpath interface memb args =
  RpcCall (fromString service) (fromString objpath) (fromString interface) (fromString memb) args

{{- range .Node.Enums}}{{- template "enum_bindings" .}}{{end}}
{{- range .Node.Interfaces}}{{range .Enums}}{{template "enum_bindings" .}}{{end}}{{end}}

{{- range $i_idx, $i_elem := .Node.Interfaces}}

-- Interface {{$i_elem.Name}}
  {{- range $i_elem.Methods}}

{{camelcase $i_elem.Name true}}{{camelcase .Name false}} :: ((MonadRpc e m)) => String -> String ->
    {{- range .Parameters}} {{.HaskellType}} ->{{end}} m (
    {{- range $r_idx, $r_elem := .Returns}}{{if $r_idx}}, {{end}}{{$r_elem.HaskellType}}{{end}})
{{camelcase $i_elem.Name true}}{{camelcase .Name false}} service_ objPath_
    {{- range .Parameters}} {{.Name}}{{end}} = do
  variants <- rpcCall (mkcall service_ objPath_ "{{$i_elem.Name}}" "{{.Name}}" [
    {{- range $p_idx, $p_elem := .Parameters}}{{if $p_idx}},{{end}} (toVariant $ {{$p_elem.Name}}){{end}} ])
  case variants of
    [{{range $r_idx, $r_elem := .Returns}}{{if $r_idx}},{{end}} out_{{$r_idx}}{{end}} ] -> return (
    {{- range $r_idx, $r_elem := .Returns}}{{if $r_idx}}, {{end}}(((\mv_ -> let Just v_ = mv_ in v_) . fromVariant) $ out_{{$r_idx}}){{end}})
    _ -> error "RPC call returned unexpected number of arguments! {{$i_elem.Name}}.{{.Name}}"
  {{- end}}
  {{- range $i_elem.Properties}}

{{camelcase $i_elem.Name true}}Get{{camelcase .Name false}} :: ((MonadRpc e m)) => String -> String -> m ({{.HaskellType}})
{{camelcase $i_elem.Name true}}Get{{camelcase .Name false}} service_ objPath_ = (
  do
    variants <- rpcCall (mkcall service_ objPath_ "org.freedesktop.DBus.Properties" "Get" [ toVariant "{{$i_elem.Name}}", toVariant "{{.Name}}" ])
    case variants of
      [ out_0 ] -> return ((((\mv_ -> let Just v_ = mv_ in v_) . fromVariant) $ (((\mv_ -> let Just v_ = mv_ in v_) . fromVariant) $ out_0)))
      _ -> error "RPC call returned unexpected number of arguments! org.freedesktop.DBus.Properties.Get")
    {{- if eq .Access "readwrite"}}

{{camelcase $i_elem.Name true}}Set{{camelcase .Name false}} :: ((MonadRpc e m)) => String -> String -> {{.HaskellType}} -> m ()
{{camelcase $i_elem.Name true}}Set{{camelcase .Name false}} service_ objPath_ value = do
  variants <- rpcCall (mkcall service_ objPath_ "org.freedesktop.DBus.Properties" "Set" [ toVariant "{{$i_elem.Name}}", toVariant "{{.Name}}", (toVariant $ (toVariant $ value)) ])
  case variants of
    [] -> return ()
    _ -> error "RPC call returned unexpected number of arguments! org.freedesktop.DBus.Properties.Set"
    {{- end}}
  {{- end}}
{{- end}}
