// SPDX-License-Identifier: BSD-3-Clause
package main

import (
        "strings"
)

type TemplateState struct {
        Node          *Node
        PackageName   string
        InterfaceBase string
        ObjectName    string
        ObjectInitial string
        InterfaceIdx  int
        MethodIdx     int
        PropertyIdx   int
}

func NewTemplateState(n *Node, pkg, objName, ib string) *TemplateState {
        return &TemplateState{
                Node:          n,
                PackageName:   pkg,
                InterfaceBase: ib,
                ObjectName:    CamelCase(objName, false),
                ObjectInitial: Initial(objName),
                InterfaceIdx:  0,
                MethodIdx:     0,
                PropertyIdx:   0,
        }
}

func (t *TemplateState) GetInterface() Interface {
        return t.Node.Interfaces[t.InterfaceIdx]
}

func (t *TemplateState) ShortInterfaceName() string {
        i := t.Node.Interfaces[t.InterfaceIdx]
        return strings.TrimPrefix(i.Name, t.InterfaceBase)
}

func (t *TemplateState) SetInterfaceIdx(i int) *TemplateState {
        t.InterfaceIdx = i

        return t
}

func (t *TemplateState) GetMethod() Method {
        return t.GetInterface().Methods[t.MethodIdx]
}

func (t *TemplateState) SetMethodIdx(i int) *TemplateState {
        t.MethodIdx = i

        return t
}

func (t *TemplateState) GetProperty() Property {
        return t.GetInterface().Properties[t.PropertyIdx]
}

func (t *TemplateState) SetPropertyIdx(i int) *TemplateState {
        t.PropertyIdx = i

        return t
}
