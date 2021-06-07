// SPDX-License-Identifier: BSD-3-Clause
package main

var haskellSingleSigMap = map[string]string{
	"y": "Word8",
	"b": "Bool",
	"n": "Int16",
	"q": "Word16",
	"i": "Int32",
	"u": "Word32",
	"x": "Int64",
	"t": "Word64",
	"d": "Double",
	"s": "String",
	"o": "ObjectPath",
	"v": "Variant",
	"h": "System.Posix.IO", // dubious
}

var haskellDoubleSigMap = map[string]string{
	"ay": "B.ByteString", // Why not [Word8]
	"ai": "[Int32]",
	"au": "[Word32]",
	"ax": "[Int64]",
	"at": "[Word64]",
	"as": "[String]",
	"ao": "[ObjectPath]",
}

// DBus structs are not supported.
var haskellComplexSigMap = map[string]string{
	"a{ss}":     "(Map.Map String String)",
	"a{su}":     "(Map.Map String Word32)",
	"a{sv}":     "(Map.Map String Variant)",
	"aa{ss}":    "[(Map.Map String String)]",
	"aa{sv}":    "[(Map.Map String Variant)]",
	"a{sa{sv}}": "(Map.Map String (Map.Map String Variant))",
	// Handle structs as String, these are not supported by fromVariant provided by dbus-core
	"a(isssssa{ss}a{ss}s)": "[String]",
	//"a(isssssa{ss}a{ss}s)": "(Int32, String, String, String, String, String, (Map String String), (Map String String), String)",
}

func HaskellType(dt string) string {
	if t, ok := haskellSingleSigMap[dt]; ok {
		return t
	}
	if t, ok := haskellDoubleSigMap[dt]; ok {
		return t
	}
	if t, ok := haskellComplexSigMap[dt]; ok {
		return t
	}
	return "()"
}

func (a Arg) HaskellType() string {
	return HaskellType(a.Type)
}

func (e Enum) HaskellType() string {
	return HaskellType(e.Type)
}

func (p Property) HaskellType() string {
	return HaskellType(p.Type)
}

func (p Property) HaskellAccess() string {
	if p.Access == "readwrite" {
		return "ReadWrite"
	}
	return "Read"
}
