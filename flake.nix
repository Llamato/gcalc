{
    description = "An attempt at a simple nix dev shell and build script";
    inputs.nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    outputs = { self, nixpkgs-unstable }:
    let
      pkgs = import nixpkgs-unstable { system = "x86_64-linux"; };
    in
    {
    pkgs.haskellPackages.developPackage {
      root = ./.;
      name = "gcalc";
    };
  }
}