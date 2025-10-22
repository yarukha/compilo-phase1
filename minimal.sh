#!/usr/bin/env bash
NIX_FOLDER="/dev/shm/$(whoami)"
NIX_PREFIX="nixpkgs#ocaml-ng.ocamlPackages_5_2"
NIX_PKGS="${NIX_PREFIX}.ocaml ${NIX_PREFIX}.dot-merlin-reader ${NIX_PREFIX}.ocaml-lsp ${NIX_PREFIX}.ocamlformat"

export HOME="/dev/shm/$(whoami)/home"
mkdir -p "$HOME"
chmod 700 "$HOME"
export NP_EXTRA_MOUNTS="$HOME:$HOME"

if [[ ! -f ${NIX_FOLDER}/nix-portable ]]; then
    wget -q "https://github.com/DavHau/nix-portable/releases/latest/download/nix-portable-$(uname -m)" -O "${NIX_FOLDER}/nix-portable"
    chmod +x "${NIX_FOLDER}/nix-portable"
fi

if [[ $# -gt 0 ]]; then
    exec env HOME="$HOME" NP_GIT="$(which git)" NP_LOCATION="${NIX_FOLDER}" \
        "${NIX_FOLDER}/nix-portable" nix shell ${NIX_PKGS} --command bash -c "$*"
else
    exec env HOME="$HOME" NP_GIT="$(which git)" NP_LOCATION="${NIX_FOLDER}" \
        "${NIX_FOLDER}/nix-portable" nix shell ${NIX_PKGS}
fi

