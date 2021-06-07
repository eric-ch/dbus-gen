{{- $top := . -}}
{-# LANGUAGE OverloadedStrings #-}
module {{.PackageName}} (
  {{.ObjectName}}Server (..)
  -- Use this to get a list of interfaces for export purposes, from a server object definition. 
  , interfaces
) where

import Data.String
import Data.Word
import Data.Int
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as B
import Rpc.Core
import Tools.FreezeIOM

-- {{.ObjectName}}Server data type is an agregate of methods which have to be implemented by the user of this wrapper.
data {{.ObjectName}}Server m = {{.ObjectName}}Server {
{{- $first := true}}
{{- range $i_idx, $i_elem := .Node.Interfaces}}
  {{- range $p_idx, $p_elem := $i_elem.Properties}}
  {{if $first}}{{$first = false}}{{else}}, {{end}}{{camelcase $i_elem.Name true}}Get{{camelcase .Name false}} :: m ({{.HaskellType}})
    {{- if eq .Access "readwrite"}}
  , {{camelcase $i_elem.Name true}}Set{{camelcase .Name false}} :: {{.HaskellType}} -> m ()
    {{- end}}
  {{- end}}
  {{- range $m_idx, $m_elem := $i_elem.Methods}}
  {{if $first}}{{$first = false}}{{else}}, {{end}}{{camelcase $i_elem.Name true}}{{camelcase .Name false}} ::{{range .Parameters}} {{.HaskellType}} ->{{end}} m ({{range $r_idx, $r_elem := .Returns}}{{if $r_idx}}, {{end}}{{.HaskellType}}{{end}})
  {{- end}}
{{- end}}
}

-- Stubs handle marshalling datatypes from/to Variants interfacing with DBus calls.
{{- range $i_idx, $i_elem := .Node.Interfaces}}

-- Interface {{.Name}}
  {{- range .Methods}}

stub{{camelcase $i_elem.Name false}}{{camelcase .Name false}} :: ((FreezeIOM ctx i m, MonadRpc e m)) => {{$top.ObjectName}}Server m -> [Variant] -> m [Variant]
stub{{camelcase $i_elem.Name false}}{{camelcase .Name false}} server_ args_ = do
  case args_ of
    [{{range $p_idx, $p_elem := .Parameters}}{{if $p_idx}},{{end}} {{.Name}}{{end}} ] -> do
      ({{range $r_idx, $r_elem := .Returns}}{{if $r_idx}}, {{end}}out_{{$r_idx}}{{end}}) <- {{camelcase $i_elem.Name true}}{{camelcase .Name false}} server_
        {{- range .Parameters}}
        (((\mv_ -> let Just v_ = mv_ in v_) . fromVariant) $ {{.Name}})
        {{- end}}
      return [{{range $r_idx, $r_elem := .Returns}}{{if $r_idx}},{{end}} (toVariant $ out_{{$r_idx}}){{end}} ]
    _ -> error "invalid arguments"
  {{- end}}
{{- end}}

-- DBus Properties wrappers
stubOrgFreedesktopDBusPropertiesGet :: ((FreezeIOM ctx i m, MonadRpc e m)) => {{.ObjectName}}Server m -> [Variant] -> m [Variant]
stubOrgFreedesktopDBusPropertiesGet server_ args_ = do
  case args_ of
    [ interface, property ] -> do
      (out_0) <-
        orgFreedesktopDBusPropertiesGet server_
          (((\mv_ -> let Just v_ = mv_ in v_) . fromVariant) $ interface)
          (((\mv_ -> let Just v_ = mv_ in v_) . fromVariant) $ property)
      return [ (toVariant $ out_0) ]
    _ -> error "invalid arguments"

stubOrgFreedesktopDBusPropertiesSet :: ((FreezeIOM ctx i m, MonadRpc e m)) => {{.ObjectName}}Server m -> [Variant] -> m [Variant]
stubOrgFreedesktopDBusPropertiesSet server_ args_ = do
  case args_ of
    [ interface, property, value ] -> do
      () <- orgFreedesktopDBusPropertiesSet server_
        (((\mv_ -> let Just v_ = mv_ in v_) . fromVariant) $ interface)
        (((\mv_ -> let Just v_ = mv_ in v_) . fromVariant) $ property)
        (((\mv_ -> let Just v_ = mv_ in v_) . fromVariant) $ value)
      return [  ]
    _ -> error "invalid arguments"

stubOrgFreedesktopDBusPropertiesGetAll :: ((FreezeIOM ctx i m, MonadRpc e m)) => {{.ObjectName}}Server m -> [Variant] -> m [Variant]
stubOrgFreedesktopDBusPropertiesGetAll server_ args_ = do
  case args_ of
    [ interface ] -> do
      (out_0) <- orgFreedesktopDBusPropertiesGetAll server_
        (((\mv_ -> let Just v_ = mv_ in v_) . fromVariant) $ interface)
      return [ (toVariant $ out_0) ]
    _ -> error "invalid arguments"

orgFreedesktopDBusPropertiesGet :: ((MonadRpc e m)) => {{.ObjectName}}Server m -> String -> String -> m Variant
orgFreedesktopDBusPropertiesGet obj intf property =
  case (intf, property) of
  {{- range $i_idx, $i_elem := .Node.Interfaces}}
    {{- range $i_elem.Properties}}
    ("{{$i_elem.Name}}", "{{.Name}}") -> {{camelcase $i_elem.Name true}}Get{{camelcase .Name false}} obj >>= return . toVariant
    {{- end}}
  {{- end}}
    _ -> error $ "property " ++ intf ++ "." ++ property ++ " is not readable, or does not exist"

orgFreedesktopDBusPropertiesSet :: ((MonadRpc e m)) => {{.ObjectName}}Server m -> String -> String -> Variant -> m ()
orgFreedesktopDBusPropertiesSet obj intf property value =
  case (intf, property) of
  {{- range $i_idx, $i_elem := .Node.Interfaces}}
    {{- range $i_elem.Properties}}
      {{- if eq .Access "readwrite"}}
    ("{{$i_elem.Name}}", "{{.Name}}") -> let Just v = fromVariant value in {{camelcase $i_elem.Name true}}Set{{camelcase .Name false}} obj v
      {{- end}}
    {{- end}}
  {{- end}}
    _ -> error $ "property " ++ intf ++ "." ++ property ++ " is not writable, or does not exist"

orgFreedesktopDBusPropertiesGetAll :: ((FreezeIOM ctx i m, MonadRpc e m)) => {{.ObjectName}}Server m -> String -> m (Map String Variant)
orgFreedesktopDBusPropertiesGetAll obj intf =
  case intf of
  {{- range $i_idx, $i_elem := .Node.Interfaces}}
    "{{$i_elem.Name}}" -> rpcRunParallel [
    {{- range $p_idx, $p_elem := $i_elem.Properties}}
        {{if $p_idx}},{{else}} {{end}} {{camelcase $i_elem.Name true}}Get{{camelcase .Name false}} obj >>= \v -> return ("{{.Name}}", toVariant v)
    {{- end}}
      ] >>= return . Map.fromList
  {{- end}}
    _ -> error $ "unknown interface " ++ intf

-- Returns a list of interfaces for exporting, given the server data type.
interfaces :: ((FreezeIOM ctx i m, MonadRpc e m)) => {{.ObjectName}}Server m -> [RpcInterface m]
interfaces server_ =
  [
{{- range $i_idx, $i_elem := .Node.Interfaces}}
    RpcInterface "{{.Name}}" [
  {{- range $m_idx, $m_elem := .Methods}}
      {{if $m_idx}},{{else}} {{end}} rpcMethod "{{.Name}}" "
        {{- range $p_idx, $p_elem := .Parameters}}{{if $p_idx}},{{end}}{{.Name}}:{{.Type}}{{end}}" "
        {{- range $r_idx, $r_elem := .Returns}}{{if $r_idx}},{{end}}{{.Name}}:{{.Type}}{{end}}"
        {{- " "}}(stub{{camelcase $i_elem.Name false}}{{camelcase .Name false}} server_)
  {{- end}}
    ] [
  {{- range $p_idx, $p_elem := .Properties}}
      {{if $p_idx}},{{else}} {{end}} rpcProperty "{{.Name}}" "{{.Type}}" {{.HaskellAccess}}
  {{- end}}
    ],
{{- end}}
    RpcInterface "org.freedesktop.DBus.Properties" [
        rpcMethod "Get" "interface:s,property:s" "value:v" (stubOrgFreedesktopDBusPropertiesGet server_)
      , rpcMethod "Set" "interface:s,property:s,value:v" "" (stubOrgFreedesktopDBusPropertiesSet server_)
      , rpcMethod "GetAll" "interface:s" "properties:a{sv}" (stubOrgFreedesktopDBusPropertiesGetAll server_)
    ] [
    ]
  ]
