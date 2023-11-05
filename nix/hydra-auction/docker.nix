# Docker images built from our packages. NOTE: These images don't include any
# metadata, as this is only added by the Github workflow.

{ myPackages # as defined in packages.nix
, system ? builtins.currentSystem
, nixpkgs ? <nixpkgs>
}:
let
  pkgs = import nixpkgs { inherit system; };
in
{
  cliImage = pkgs.dockerTools.buildLayeredImage
    {
      name = "hydra-auction-cli";
      tag = "latest";
      created = "now";
      contents = [ myPackages.hydra-auction ];
      config = {
        Cmd = [ "hydra-auction" ];
      };
    };
  delegateImage = pkgs.dockerTools.buildLayeredImage
    {
      name = "hydra-auction-delegate";
      tag = "latest";
      created = "now";
      contents = [ myPackages.hydra-auction-delegate ];
      config = {
        Cmd = [ "hydra-auction-delegate" ];
      };
    };
  platformImage = pkgs.dockerTools.buildLayeredImage
    {
      name = "hydra-auction-platform";
      tag = "latest";
      created = "now";
      contents = [ myPackages.hydra-auction-platform ];
      config = {
        Cmd = [ "hydra-auction-platform" ];
      };
    };
}
