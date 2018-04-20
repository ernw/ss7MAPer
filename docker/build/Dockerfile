FROM ubuntu:16.04
RUN apt-get -y update && apt-get install -y erlang rebar make gcc libpcap-dev wget
CMD wget https://raw.githubusercontent.com/ernw/ss7MAPer/master/prepare_n_build.sh && rm -rf /usr/lib/erlang/man && sh prepare_n_build.sh && tar -xvjf /ss7MAPer-trunk_ubuntu16.04_x64.tar.bz2 /ss7MAPer/rel/ss7MAPer
