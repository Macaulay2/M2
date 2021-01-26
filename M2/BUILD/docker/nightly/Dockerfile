# Time usage: <5min
# Net usage:  ~100MB
# Disk usage: <600MB docker image

FROM ubuntu:20.04

RUN apt-get update && \
    apt-get install -y software-properties-common apt-transport-https curl git sudo unzip gnupg && \
    add-apt-repository -y ppa:macaulay2/macaulay2 && apt-get update && apt-get clean

# this seems to be necessary because libstdc++6 is too old on Ubuntu 18.04
RUN add-apt-repository -y ppa:ubuntu-toolchain-r/test && apt-get update && \
    apt-get install -y -q --no-install-recommends libomp5-9 && apt-get clean

# Install Macaulay2
COPY Macaulay2-*.zip /
RUN unzip Macaulay2-*.zip && apt-get install -y /Macaulay2-*.deb

# Install optional packages
RUN apt-get install -y -q --no-install-recommends mlocate bash-completion && apt-get clean && updatedb

#RUN apt-get install -y -q --no-install-recommends emacs && apt-get clean

# Add non-root user for building and running Macaulay2
RUN useradd -G sudo -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER 1000:0

# Setting environment variables
ENV LD_LIBRARY_PATH /usr/local/lib/Macaulay2/lib
ENV PATH /usr/local/libexec/Macaulay2/bin:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT M2
