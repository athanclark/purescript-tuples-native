{ pkgs ? import ./pinned.nix { } }:

import
  (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "master";
      sha256 = "051fzxd03y0c63sll2bhn0h66dywy9lw6ylyh5vq8fymvix20q94";
    }
  ) {
  inherit pkgs;
}
