#!/bin/sh -x

git clone https://github.com/ernw/ss7MAPer

cd ss7MAPer
rebar get-deps

cd deps/osmo_map   
patch -p1 < ../../patches/osmo_map.patch   
cd ../osmo_sccp   
patch -p1 < ../../patches/osmo_sccp.patch   
cd ../osmo_ss7   
patch -p1 < ../../patches/osmo_ss7.patch 

cd ../../
sed -i 's/DRV_CFLAGS/CFLAGS/g' deps/epcap/rebar.config
sed -i 's/DRV_LDFLAGS/LDFLAGS/g' deps/epcap/rebar.config

cd deps
git clone http://cgit.osmocom.org/erlang/signerl/

cd signerl/TCAP/asn_src/ITU
make

cd ../../../../..
cp deps/signerl/TCAP/asn_src/ITU/*rl deps/osmo_sccp/src/

mkdir deps/epcap/deps
ln -sd ../../pkt deps/epcap/deps/pkt
mkdir deps/osmo_sccp/deps   
ln -sd ../../osmo_ss7 deps/osmo_sccp/deps/osmo_ss7   
ln -sd ../../epcap deps/osmo_sccp/deps/epcap   
ln -sd ../../pkt deps/osmo_sccp/deps/pkt   
ln -sd ../../signerl/MAP deps/osmo_sccp/deps/MAP    
ln -sd ../../signerl/SCCP deps/osmo_sccp/deps/SCCP   
ln -sd ../../signerl/TCAP deps/osmo_sccp/deps/TCAP   
mkdir deps/osmo_map/deps   
ln -sd ../../osmo_ss7 deps/osmo_map/deps/osmo_ss7   
ln -sd ../../epcap deps/osmo_map/deps/epcap   
ln -sd ../../pkt deps/osmo_map/deps/pkt

cp deps/signerl/SCCP/itu/include/sccp.hrl deps/osmo_sccp/src/   
cp deps/signerl/TCAP/include/tcap.hrl deps/osmo_map/src/

rebar co

cd rel/
rebar create-node nodeid=ss7MAPer
cd ..
rebar generate
