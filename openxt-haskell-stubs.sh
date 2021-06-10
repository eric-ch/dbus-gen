#! /bin/bash

set -e

usage() {
    cat - <<EOF
    Usage: $0 XML-DIR OUTPUT-DIR"
        Generate the Haskell stubs for OpenXT in OUTPUT-DIR using the IDL XML files in XML-DIR
EOF
}

# Usage: dbusgen_server IDL-PATH BASE-NAME
dbusgen_server() {
    local idl_path="$1"
    local base_name="$2"

    ./dbus-gen -i "$idl_path" -t dbus-server-haskell.tpl -p "Rpc.Autogen.${base_name}Server" -o "$output/${base_name}Server.hs"
    if grep -q '<signal' "$idl_path"; then
        ./dbus-gen -i "$idl_path" -t dbus-notify-haskell.tpl -p "Rpc.Autogen.${base_name}Notify" -o "$output/${base_name}Notify.hs"
    fi
    if grep -q '<tp:enum' "$idl_path"; then
        ./dbus-gen -i "$idl_path" -t dbus-const-haskell.tpl -p "Rpc.Autogen.${base_name}Const" -o "$output/${base_name}Const.hs"
    fi
}

# Usage: dbusgen_client IDL-PATH BASE-NAME
dbusgen_client() {
    local idl_path="$1"
    local base_name="$2"

    ./dbus-gen -i "$idl_path" -t dbus-client-haskell.tpl -p "Rpc.Autogen.${base_name}Client" -o "$output/${base_name}Client.hs"
}

# Sanity checks
if ! command -v ./dbus-gen; then
    echo "Cannot find ./dbus-gen" >&2
    exit 1
fi
if [ $# -ne 2 ]; then
    usage
    exit 1
fi

# Inputs
idldir="$1"
output="$2"

# Haskell
## Db
dbusgen_client "$idldir/db.xml" "Db"

## Dbus
dbusgen_client "$idldir/dbus.xml" "Dbus"

## Network
dbusgen_server "$idldir/network.xml" "Network"
dbusgen_server "$idldir/network_nm.xml" "NetworkNm"
dbusgen_server "$idldir/network_daemon.xml" "NetworkDaemon"
dbusgen_server "$idldir/network_domain.xml" "NetworkDomain"
dbusgen_server "$idldir/network_slave.xml" "NetworkSlave"

dbusgen_client "$idldir/network.xml" "Network"
dbusgen_client "$idldir/network_nm.xml" "NetworkNm"
dbusgen_client "$idldir/network_daemon.xml" "NetworkDaemon"
dbusgen_client "$idldir/network_slave.xml" "NetworkSlave"

## NetworkManager
dbusgen_client "$idldir/org.freedesktop.NetworkManager.xml" "NmManager"
dbusgen_client "$idldir/org.freedesktop.NetworkManager.xml" "NmManager"
dbusgen_client "$idldir/org.freedesktop.NetworkManager.Device.xml" "NmDevice"
dbusgen_client "$idldir/org.freedesktop.NetworkManager.Device.Wired.xml" "NmDeviceEthernet"
dbusgen_client "$idldir/org.freedesktop.NetworkManager.Device.Wireless.xml" "NmDeviceWifi"
dbusgen_client "$idldir/org.freedesktop.NetworkManager.Device.Modem.xml" "NmDeviceModem"
dbusgen_client "$idldir/org.freedesktop.NetworkManager.AccessPoint.xml" "NmAccessPoint"
dbusgen_client "$idldir/org.freedesktop.NetworkManager.Connection.Active.xml" "NmActiveConnection"

## XenMgr
dbusgen_server "$idldir/xenmgr.xml" "Xenmgr"
dbusgen_server "$idldir/xenmgr_vm.xml" "XenmgrVm"
dbusgen_server "$idldir/xenmgr_host.xml" "XenmgrHost"
dbusgen_server "$idldir/vm_nic.xml" "VmNic"
dbusgen_server "$idldir/vm_disk.xml" "VmDisk"

dbusgen_client "$idldir/xenmgr.xml" "Xenmgr"
dbusgen_client "$idldir/xenmgr_vm.xml" "XenmgrVm"
dbusgen_client "$idldir/xenmgr_host.xml" "XenmgrHost"
dbusgen_client "$idldir/vm_nic.xml" "VmNic"
dbusgen_client "$idldir/vm_disk.xml" "VmDisk"
dbusgen_client "$idldir/guest.xml" "Guest"
dbusgen_client "$idldir/ctxusb_daemon.xml" "CtxusbDaemon"

## Input
dbusgen_client "$idldir/input_daemon.xml" "InputDaemon"

## UpdateMgr
dbusgen_server "$idldir/updatemgr.xml" "Updatemgr"

## Rpc-proxy
dbusgen_server "$idldir/rpc_proxy.xml" "RpcProxy"
