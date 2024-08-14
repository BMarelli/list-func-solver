FROM ubuntu:focal

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=America/Buenos_Aires

# install dependencies
RUN \
  apt-get update -y && \
  apt-get install -y --no-install-recommends \
  curl \
  libnuma-dev \
  zlib1g-dev \
  libgmp-dev \
  libgmp10 \
  git \
  wget \
  lsb-release \
  software-properties-common \
  gnupg2 \
  apt-transport-https \
  gcc \
  autoconf \
  automake \
  build-essential \
  libncurses5 \
  libncurses5-dev \
  z3 \
  llvm

# Install ghcup
RUN \
  curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
  chmod +x /usr/bin/ghcup

# install GHC and cabal
ARG GHC=9.4.8
ARG CABAL=3.10.3.0

RUN \
  ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
  ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL}

# Print installed versions
RUN ghc --version && cabal --version

# Update cabal package list
RUN cabal update

# Create working directory
RUN mkdir -p /list-func-solver
WORKDIR /list-func-solver

# Copy project files
COPY . .


# Build project
RUN cabal build

CMD ["bash"]
