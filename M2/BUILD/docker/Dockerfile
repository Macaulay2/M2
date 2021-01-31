# Time usage: <5min
# Net usage:  ~200MB
# Disk usage: <800MB docker image

FROM ubuntu:focal

# Setting up Macaulay2 repository
RUN apt-get update && \
    apt-get install -y --no-install-recommends software-properties-common apt-transport-https && \
    add-apt-repository ppa:profzoom/macaulay2 && add-apt-repository ppa:profzoom/macaulay2 && \
    apt-get update && apt-get clean

# Install Macaulay2
RUN apt-get install -y --no-install-recommends macaulay2 && apt-get clean

# Install optional packages
RUN apt-get install -y emacs elpa-macaulay2 bash-completion curl git mlocate && \
    apt-get clean && updatedb

# Add non-root user for running Macaulay2
RUN useradd -G sudo -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER 1000:0

# Setting environment variables
ENV LD_LIBRARY_PATH /usr/lib/Macaulay2/lib
ENV PATH /usr/libexec/Macaulay2/bin:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT emacs
