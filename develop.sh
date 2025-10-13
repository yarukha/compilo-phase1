#!/usr/bin/env bash

NIX_CHROOT_URL="https://github.com/DavHau/nix-portable/releases/latest/download/nix-portable-$(uname -m)"
NIX_FOLDER="/dev/shm/$(whoami)"

NIX_PREFIX="nixpkgs#ocaml-ng.ocamlPackages_5_2"
NIX_PKGS="${NIX_PREFIX}.ocaml ${NIX_PREFIX}.dot-merlin-reader ${NIX_PREFIX}.ocaml-lsp ${NIX_PREFIX}.ocamlformat"

if command -v nix >/dev/null 2>&1 
then
    if ! $(nix config show | grep experimental-features | grep flakes >/dev/null 2>&1)
    then
       echo "Flakes are not enabled, please add the following to '~/.config/nix/nix.conf' or '/etc/nix/nix.conf':"
       echo "experimental-features = nix-command flakes"
       exit 1
    fi
    exec nix shell ${NIX_PKGS}
fi

if [[ ! $(hostname) =~ ^[0-9][0-9]$ ]]
then
    echo "It is recommended that you install Nix on your system. This requires root access."
    read -r -p "Would you like to install Nix? [y/N] " answer
    echo ""

    if [[ "${answer}" =~ ^([yY][eE][sS]|[yY])$ ]]
    then
        exec sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --daemon
    fi
    
    echo "This script will install Nix in the ~/.nix-portable folder. This does not require root access."
    read -r -p "Would you like to continue with a portable installation (not recommended) ? [y/N] " answer
    echo ""

    if [[ ! "${answer}" =~ ^([yY][eE][sS]|[yY])$ ]]
    then
        echo "There are no other available methods. Exiting."
        exit 1
    fi

    NIX_FOLDER="${HOME}/.nix-portable"
fi

NIX_PKGS="${NIX_PREFIX}.ocaml"

if [[ ! -f ${NIX_FOLDER}/nix-portable ]]
then
    echo "Downloading DavHau/nix-portable"

    mkdir -m 0755 -p "${NIX_FOLDER}"
    wget -q "${NIX_CHROOT_URL}" -O "${NIX_FOLDER}/nix-portable"
    chmod +x "${NIX_FOLDER}/nix-portable"
fi

echo "Starting Nix shell"
exec bash -c "NP_GIT=$(which git) NP_LOCATION=${NIX_FOLDER} ${NIX_FOLDER}/nix-portable nix shell ${NIX_PKGS}"
