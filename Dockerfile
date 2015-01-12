FROM ubuntu:12.04

ENV HOME /root
ENV DEBIAN_FRONTEND noninteractive
WORKDIR /root
RUN apt-get update

# ghc 7.8.3
RUN apt-get install -y openssh-server # so that bootstrap.sh and wget works without --no-check-certificate (?)
RUN apt-get install -y wget libgmp3-dev build-essential
RUN ln -s /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/libgmp.so.3
RUN ln -s /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/libgmp.so
RUN wget http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-x86_64-unknown-linux-deb7.tar.xz
RUN tar xf ghc-7.8.3-x86_64-unknown-linux-deb7.tar.xz
RUN rm ghc-7.8.3-x86_64-unknown-linux-deb7.tar.xz

WORKDIR /root/ghc-7.8.3
RUN ./configure
RUN make install
WORKDIR /root
RUN rm -rf ghc-7.8.3

# cabal 1.20.0.2
RUN wget http://www.haskell.org/cabal/release/cabal-1.20.0.2/Cabal-1.20.0.2.tar.gz
RUN tar xf Cabal-1.20.0.2.tar.gz
RUN rm Cabal-1.20.0.2.tar.gz
WORKDIR Cabal-1.20.0.2
RUN ghc --make Setup
RUN ./Setup configure
RUN ./Setup build
RUN ./Setup install
WORKDIR /root
RUN rm -rf ./Cabal-1.20.0.2

WORKDIR /root
RUN wget http://www.haskell.org/cabal/release/cabal-install-1.20.0.2/cabal-install-1.20.0.2.tar.gz
RUN tar xf cabal-install-1.20.0.2.tar.gz
RUN rm cabal-install-1.20.0.2.tar.gz
WORKDIR cabal-install-1.20.0.2
RUN apt-get install -y zlib1g-dev
RUN ./bootstrap.sh
ENV PATH $HOME/.cabal/bin:$PATH
RUN cabal update
RUN echo "export PATH=~/.cabal/bin:$PATH" >> /root/.profile
WORKDIR /root
RUN rm -rf ./cabal-install-1.20.0.2
RUN cp ~/.cabal/config ~/.cabal/config.old
RUN sed -E 's/(-- )?(library-profiling: )False/\2True/' < ~/.cabal/config.old > ~/.cabal/config
RUN locale-gen en_US.UTF-8
RUN export LC_ALL='en_US.UTF-8'
ENV LC_ALL en_US.UTF-8
RUN cabal install -j happy

# Git clone
RUN apt-get install -y git-core
WORKDIR ${HOME}/
RUN git clone https://github.com/lenguyenthedat/raptor.git

# Build
WORKDIR ${HOME}/raptor
RUN cabal sandbox init
RUN cabal install
RUN bash run.sh