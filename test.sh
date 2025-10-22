#!/usr/bin/env bash
set -euo pipefail

NIX_CHROOT_URL="https://github.com/DavHau/nix-portable/releases/latest/download/nix-portable-$(uname -m)"
NIX_FOLDER="$PWD/.nix-portable"
NIX_PREFIX="nixpkgs#ocaml-ng.ocamlPackages_5_2"
NIX_PKGS="${NIX_PREFIX}.ocaml ${NIX_PREFIX}.dot-merlin-reader ${NIX_PREFIX}.ocaml-lsp ${NIX_PREFIX}.ocamlformat"

# --- Préparer un HOME valide ---
export HOME="$PWD/.nix-home"
mkdir -p "$HOME/.config/nix"
chmod 700 "$HOME"

# --- Télécharger nix-portable si manquant ---
if [[ ! -f ${NIX_FOLDER}/nix-portable ]]; then
    echo "Downloading DavHau/nix-portable"
    mkdir -m 0755 -p "${NIX_FOLDER}"
    wget -q "${NIX_CHROOT_URL}" -O "${NIX_FOLDER}/nix-portable"
    chmod +x "${NIX_FOLDER}/nix-portable"
fi

# --- Forcer le runtime ---
export NP_LOCATION="${NIX_FOLDER}"
export NP_RUNTIME="proot"
export NP_EXTRA_MOUNTS="$HOME:$HOME"
export NP_GIT="$(command -v git || true)"

# --- Vérif debug ---
echo "== DEBUG =="
echo "HOME=$HOME"
echo "NP_LOCATION=$NP_LOCATION"
echo "NP_RUNTIME=$NP_RUNTIME"
echo "NP_EXTRA_MOUNTS=$NP_EXTRA_MOUNTS"
echo "=============="

# --- Exécution ---
if [[ $# -gt 0 ]]; then
    exec env HOME="$HOME" \
        "${NIX_FOLDER}/nix-portable" nix shell ${NIX_PKGS} --command bash -c "$*"
else
    exec env HOME="$HOME" \
        "${NIX_FOLDER}/nix-portable" nix shell ${NIX_PKGS}
fi

