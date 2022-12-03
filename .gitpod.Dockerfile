FROM ocaml/opam:ubuntu
# https://www.gitpod.io/docs/configure/workspaces/workspace-image#custom-base-image
RUN useradd -l -u 33333 -G sudo -md /home/gitpod -s /bin/bash -p gitpod gitpod
USER gitpod
WORKDIR /home/gitpod
RUN opam init && opam install dune core core_bench utop ocaml-lsp-server ocamlformat ppx_jane ppx_let
RUN opam init
