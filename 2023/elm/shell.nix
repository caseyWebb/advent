{
  pkgs ? import <nixpkgs> {}
}:
pkgs.mkShell {
  buildInputs = [
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-test
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-review
    pkgs.jq
  ];
}
