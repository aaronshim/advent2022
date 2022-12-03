FROM ocaml/opam:ubuntu
RUN useradd -ms /bin/bash gitpod
USER gitpod
WORKDIR /home/gitpod
RUN opam init && opam install dune core core_bench utop ocaml-lsp-server ocamlformat ppx_jane ppx_let
RUN opam init
