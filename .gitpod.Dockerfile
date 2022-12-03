FROM ocaml/opam:ubuntu

RUN sudo apt-get update && apt-get install -yq \
    git \
    git-lfs \
    sudo \
    && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/*
    
RUN whoami

# https://www.gitpod.io/docs/configure/workspaces/workspace-image#custom-base-image
RUN sudo useradd -l -u 33333 -G sudo -md /home/gitpod -s /bin/bash -p gitpod gitpod
USER gitpod
WORKDIR /home/gitpod
RUN opam init && opam install dune core core_bench utop ocaml-lsp-server ocamlformat ppx_jane ppx_let
RUN opam init
