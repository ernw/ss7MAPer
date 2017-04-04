FROM ubuntu:16.04
RUN apt-get -y update && apt-get install -y wget bzip2 libsctp1 && wget -O /tmp/ss7MAPer.tar.bz2 https://github.com/ernw/ss7MAPer/raw/master/releases/ss7MAPer-v0.2_linux_x64.tar.bz2 && tar -xf /tmp/ss7MAPer.tar.bz2 -C /
CMD cd /ss7MAPer && ./bin/ss7MAPer console
