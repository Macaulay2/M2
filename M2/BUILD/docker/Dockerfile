FROM ubuntu:bionic

# Setting up Macaulay2 repository
RUN apt-get update && \
    apt-get install -y --no-install-recommends apt-transport-https ca-certificates curl gnupg && \
    curl https://faculty.math.illinois.edu/Macaulay2/PublicKeys/Macaulay2-key -s --output - | apt-key add - && \
    echo 'deb http://www.math.uiuc.edu/Macaulay2/Repositories/Ubuntu bionic main' > \
	/etc/apt/sources.list.d/macaulay2.list && apt-get update && apt-get clean

# Install Macaulay2
RUN apt-get install -y macaulay2 && apt-get clean

# Install optional packages
RUN apt-get install -y emacs bash-completion git mlocate && apt-get clean && updatedb

# Add non-root user for running Macaulay2
RUN useradd -G sudo -g root -u 1000 -m macaulay && echo "macaulay ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
USER 1000:0

# Setting environment variables
ENV LD_LIBRARY_PATH /usr/lib/Macaulay2/lib
ENV PATH /usr/libexec/Macaulay2/bin:${PATH}

WORKDIR /home/macaulay
ENTRYPOINT emacs
