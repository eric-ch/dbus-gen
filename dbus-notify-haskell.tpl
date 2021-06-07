module {{.PackageName}} (
{{- $first := true}}
{{- range $i_idx, $i_elem := .Node.Interfaces}}
  {{- range .Signals}}
  {{if $first}}{{$first = false}}{{else}}, {{end}}notify{{camelcase $i_elem.Name false}}{{camelcase .Name false}}
  {{- end}}
{{- end}}
) where

import Data.String
import Data.Word
import Data.Int
import Rpc.Core

{{- range $i_idx, $i_elem := .Node.Interfaces}}
  {{- range $i_elem.Signals}}

notify{{camelcase $i_elem.Name false}}{{camelcase .Name false}} :: ((MonadRpc e m)) => ObjectPath ->{{range .Args}} {{.HaskellType}} ->{{end}} m ()
notify{{camelcase $i_elem.Name false}}{{camelcase .Name false}} objPath_{{range .Args}} {{.Name}}{{end}} =
  rpcEmitSignal RpcSignal {
    signalPath = objPath_,
    signalInterfaceT = fromString "{{$i_elem.Name}}",
    signalMemberT = fromString "{{.Name}}",
    signalArgs = [
      {{- range $p_idx, $p_elem := .Args}}
      {{if $p_idx}},{{else}} {{end}} (toVariant $ {{$p_elem.Name}})
      {{- end}}
    ]
  }
  {{- end}}  
{{- end}}
