# Build Setup

This directory contains all build utilities used by this project.

They are intended to be used with flake.parts
but are designed to work independent of it.
`default.nix` defines a flake.parts module,
while `lib.nix` is a nixpkgs-style package that can be
used with `callPackage` to provide the core functionality.

`default.nix` files may depend on other modules
but `lib.nix` files do not to preserve full reusablility --
you can just copy-paste them into your project and use as you wish.

flake.parts modules are exposed as flake attributes
under `flakeModules.<moduleName>`
