# ss7MAPer

SS7 MAP (pen-)testing toolkit

## Get it running

You will need:

 * Erlang. Get it from your repo or from http://www.erlang.org

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
        
        mkdir deps/osmo_sccp/deps   
        ln -sd ../../osmo_ss7 deps/osmo_sccp/deps/osmo_ss7   
        ln -sd ../../epcap deps/osmo_sccp/deps/epcap   
        ln -sd ../../pkt deps/osmo_sccp/deps/pkt   
        ln -sd ../../signerl/MAP deps/osmo_sccp/deps/MAP    
        ln -sd ../../signerl/SCCP deps/osmo_sccp/deps/SCCP   
        ln -sd ../../signerl/TCAP deps/osmo_sccp/deps/TCAP   
        mkdir deps/osmo_map/deps   
        ln -sd ../../osmo_ss7 deps/osmo_sccp/deps/osmo_ss7   
        ln -sd ../../epcap deps/osmo_sccp/deps/epcap   
        ln -sd ../../pkt deps/osmo_sccp/deps/pkt   
           
    And copy some files in place:   
        
        cp deps/signerl/SCCP/itu/include/sccp.hrl deps/osmo_sccp/src/   
        cp deps/signerl/TCAP/include/tcap.hrl deps/osmo_map/src/   
           
 * Build the code   
        
        rebar co   
           

## The config file

The config file is split in 4 section, sctp, m3ua, sccp and target.

In the *sctp* section source and destination ip as well as source and destination port of the SCTP connection are configured.

In the *m3ua* section all the M3UA parameters, like local and remote point code are configured.

In the *sccp* section currently only the local (or source) global title needs to be configured.

Last but not least in the *target* section information about the tested environment need to be configured, like the global title of the HLR, or the MSISDN of the tested phone.

Be sure to modify it to your needs.

## Running the tool

To run the tool one needs to start a rebar shell:

    cd ss7MAPer   
    rebar shell   

Then the SIGTRAN connection needs to be established:

    Pid = ss7test_app:start(1, "./configfile").   

If everything is set up correctly the m3ua connection comes up.

To run the HLR tests, simply enter:

    Pid ! {test_hlr}.   



        
