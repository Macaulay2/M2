# Time usage: 
# Net usage:  
# Disk usage: 

FROM linuxbrew/brew

# Install optional packages
RUN apt-get update && apt-get install -y -q --no-install-recommends vim mlocate && apt-get clean

USER 1000:0

# Add the tap containing the formulae for Macaulay2 and its dependencies
RUN brew update && brew config && brew tap mahrud/tap

RUN brew install bash-completion && echo '\n\
    [[ -r "`brew --prefix`/etc/profile.d/bash_completion.sh" ]] && \n\
	. "`brew --prefix`/etc/profile.d/bash_completion.sh"' >> /home/linuxbrew/.bashrc

# Install dependencies of Macaulay2
#RUN brew install --only-dependencies mahrud/tap/macaulay2

# Install Macaulay2
#RUN brew install --verbose --build-bottle mahrud/tap/macaulay2

RUN sudo updatedb

WORKDIR /home/linuxbrew
