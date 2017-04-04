# ss7MAPer

SS7 MAP (pen-)testing toolkit

## Binary releases

As a lot of people run into problems building the tool, there are binary releases which can be found here: https://github.com/ernw/ss7MAPer/tree/master/releases

As people also run into problems using the binary release, there is a [docker image](https://hub.docker.com/r/ernw/ss7maper/) with ss7MAPer running on ubuntu 16.04

If you use the binary version, skip right over the next chapter.

## Get it running

You will need:

 * Erlang. Get it from your repo or from http://www.erlang.org.

 * Rebar. Get it from your repo or from https://github.com/rebar/rebar

 * The code (;   
 
        git clone https://github.com/ernw/ss7MAPer   
        
 * The dependencies   
 
        cd ss7MAPer   
        rebar get-deps   
        
 * Patch the dependencies   
 
        cd deps/osmo_map   
        patch -p1 < ../../patches/osmo_map.patch   
        cd ../osmo_sccp   
        patch -p1 < ../../patches/osmo_sccp.patch   
        cd ../osmo_ss7   
        patch -p1 < ../../patches/osmo_ss7.patch   
        
 * Get the deps to build (; This is not as easy as it might sound, I needed to:   
    Patch the epacp/rebar.config and replace   
        
        {port_envs, [   
          {"DRV_CFLAGS", "-g -Wall $ERL_CFLAGS"},   
          {"DRV_LDFLAGS", "-lpcap $ERL_LDFLAGS"}   
        ]}.   
           
    with   
        
        {port_envs, [   
          {"CFLAGS", "-g -Wall $ERL_CFLAGS"},   
          {"LDFLAGS", "-lpcap $ERL_LDFLAGS"}   
        ]}.   
           
    Another dependency is not covered by rebar, so you need to fetch it manually:   
        
        cd deps   
        git clone http://cgit.osmocom.org/erlang/signerl/   
           
    Build the ASN.1 source files:   
        
        cd deps/signerl/TCAP/asn_src/ITU   
        make   
           
    Copy the ASN.1 files to osmo_sccp:   
        
        cp deps/signerl/TCAP/asn_src/ITU/*rl deps/osmo_sccp/src/   
           
    Also the osmo libs have dependencies on each other and some other deps are shared, so I created some symlinks:   
        
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
           
    And copy some files in place:   
        
        cp deps/signerl/SCCP/itu/include/sccp.hrl deps/osmo_sccp/src/   
        cp deps/signerl/TCAP/include/tcap.hrl deps/osmo_map/src/   
           
 * Build the code   
        
        rebar co   
           
If all the steps above are too much to do by hand (as for me testing ;) you can use the build script here: https://github.com/ernw/ss7MAPer/blob/master/prepare_n_build.sh

## The config file

The config file is split in 4 section, sctp, m3ua, sccp and target.

In the *sctp* section source and destination ip as well as source and destination port of the SCTP connection are configured.

In the *m3ua* section all the M3UA parameters, like local and remote point code are configured.

In the *sccp* section currently only the local (or source) global title needs to be configured.

Last but not least in the *target* section information about the tested environment need to be configured, like the global title of the HLR, or the MSISDN of the tested phone.

Be sure to modify it to your needs.

## Running the tool

### Running a source build

To run the tool one needs to start a rebar shell:

    cd ss7MAPer   
    rebar shell   

Start the application and its dependencies with:

    application:start(sasl).
    application:start(ss7MAPer).

If everything is set up correctly the m3ua connection comes up.

### Running the binary release

Run the application by starting it from the root directory:

    cd ss7MAPer
    ./bin/ss7MAPer console

If everything is set up correctly the m3ua connection comes up.

### Using the tool

Once the application is started, there are some commands that can be executed from the erlang command line:

 - ss7MAPer:test\_hlr()
 - ss7MAPer:test\_msc()
 - ss7MAPer:test\_smsc()

and each of them does exactly as its called, running MAP tests against the targets defined in the config file.
