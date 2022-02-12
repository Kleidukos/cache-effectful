#!/usr/bin/env bash
set -euo pipefail

if type -p nix &>/dev/null ; then
  echo "Aborting: Nix is already installed at $(type -p nix)"
  exit
fi

# Configure Nix
add_config() {
  echo "$1" | sudo tee -a /tmp/nix.conf >/dev/null
}
# Set jobs to number of cores
add_config "max-jobs = auto"
# Allow binary caches for user
add_config "trusted-users = root $USER"

# Nix installer flags
installer_options=(
  --daemon
  --daemon-user-count 4
  --no-channel-add
  --darwin-use-unencrypted-nix-store-volume
  --nix-extra-conf-file /tmp/nix.conf
)

echo "installer options: ${installer_options[@]}"
# On self-hosted runners we don't need to install more than once
if [[ ! -d /nix/store ]]
then
  sh <(curl --retry 5 --retry-connrefused -L "https://nixos.org/nix/install") "${installer_options[@]}"
fi

if [[ $OSTYPE =~ darwin ]]; then
  # Disable spotlight indexing of /nix to speed up performance
  sudo mdutil -i off /nix

  # macOS needs certificates hints
  cert_file=/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt
  export NIX_SSL_CERT_FILE=$cert_file
  sudo launchctl setenv NIX_SSL_CERT_FILE "$cert_file"
fi

# Set paths

# if [[ $INPUT_NIX_PATH != "" ]]; then
#   echo "NIX_PATH=${INPUT_NIX_PATH}" >> "$GITHUB_ENV"
# fi
