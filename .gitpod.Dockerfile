FROM ocaml/opam:ubuntu

RUN whoami

RUN opam init && opam install dune core core_bench utop ocaml-lsp-server ocamlformat ppx_jane ppx_let

# https://www.gitpod.io/docs/configure/workspaces/workspace-image#custom-base-image
RUN sudo useradd -l -u 33333 -G sudo -md /home/gitpod -s /bin/bash -p gitpod gitpod
RUN sudo cp -r /home/opam/.opam /home/gitpod/.opam
USER gitpod
WORKDIR /home/gitpod
RUN whoami
RUN pwd
RUN opam init
