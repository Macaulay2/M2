# Time usage: <5min
# Net usage:  <200MB
# Disk usage: <700MB

FROM debian:bullseye

# Setup the system
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates debian-keyring git && apt-get clean

# Install Macaulay2
RUN echo "deb [signed-by=/usr/share/keyrings/debian-maintainers.gpg] https://people.debian.org/~dtorrance/debian bullseye/" >> /etc/apt/sources.list && apt-get update && apt-get install -y --no-install-recommends macaulay2 && apt-get clean

# Add non-root user for using Macaulay2
RUN useradd -G sudo -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

WORKDIR /home/macaulay
ENTRYPOINT ["M2"]
