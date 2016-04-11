FROM fedora:latest
MAINTAINER rschoene <rene.schoene@tu-dresden.de>

# Install basic programs
RUN dnf update -y && dnf install -y git \
 python \
 fabric \
 curl \
 tar
##&& rm -rf /var/lib/apt/lists/*

# Install racket
RUN curl -L -o /racket-install.sh https://mirror.racket-lang.org/installers/6.4/racket-6.4-x86_64-linux-debian-squeeze.sh
RUN chmod u+rx /racket-install.sh
RUN /racket-install.sh --unix-style

# Create known_hosts
RUN mkdir /root/.ssh/
RUN touch /root/.ssh/known_hosts
# Add bitbuckets key
RUN ssh-keyscan github.com >> /root/.ssh/known_hosts

# Clone RACR repo
RUN git clone https://github.com/christoff-buerger/racr.git
# Clone project repo
RUN git clone https://github.com/rene-schoene/racr-mquat.git

# Install RACR
ADD easy-install-racr.sh /racr/easy-install-racr.sh
WORKDIR /racr/
RUN chmod u+rx ./easy-install-racr.sh
RUN ./easy-install-racr.sh

# Prepare racr-mquat
WORKDIR /racr-mquat/
RUN sed -i -e 's|RACR_HOME = .*|RACR_HOME = "/racr/racr/"|' -e 's|PROGRAM_HOME = .*|PROGRAM_HOME = "/racr-mquat/"|' constants.py
RUN fab install.racket
