# Time usage: <5min
# Net usage:  <200MB
# Disk usage: <850MB

FROM debian:trixie

LABEL org.opencontainers.image.source=https://github.com/Macaulay2/M2

# Setup the system
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates wget git && apt-get clean

# Install Macaulay2
RUN wget -O /etc/apt/sources.list.d/macaulay2.sources https://macaulay2.com/Repositories/Debian/trixie/macaulay2.sources && apt-get update && apt-get install -y --no-install-recommends macaulay2 4ti2 cohomcalg coinor-csdp gfan nauty normaliz topcom msolve && apt-get clean

# Add non-root user for using Macaulay2
RUN useradd -G sudo -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

WORKDIR /home/macaulay
ENTRYPOINT ["M2"]
