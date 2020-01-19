FROM ubuntu:bionic

RUN apt-get update && \
    apt-get install -y curl gnupg apt-transport-https emacs && \
    curl https://faculty.math.illinois.edu/Macaulay2/PublicKeys/Macaulay2-key -s --output - | apt-key add - && \
    echo 'deb http://www.math.uiuc.edu/Macaulay2/Repositories/Ubuntu bionic main' > \
    	 /etc/apt/sources.list.d/macaulay2.list && apt-get clean

RUN apt-get update && \
    apt-get install -y macaulay2 && \
    apt-get clean

RUN yes | M2 --silent --print-width 77 --stop --int --no-readline -q --no-randomize -e "setupEmacs(); exit(0);"
COPY emacs-docker.el /root/.emacs

WORKDIR /home
ENTRYPOINT emacs

# Create the image:
# docker build --tag m2 .

# Start a graphical instance:
# docker run --volume `pwd`/storage:/home --volume `pwd`/../../../:/home/M2 --net=host --env="DISPLAY" m2

# Start a non-graphical instance:
# docker run --volume `pwd`/storage:/home --volume `pwd`/../../../:/home/M2 --interactive --tty m2
