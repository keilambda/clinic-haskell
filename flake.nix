{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { localSystem = { inherit system; }; };
      zlib = pkgs.zlib;
      postgresql = pkgs.postgresql;
    in
    {
      devShells.default = pkgs.mkShell ({
        buildInputs = [
          zlib
          postgresql
        ];

        shellHook = ''
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${postgresql.lib}/lib
        '';
      });
    }
  );
}
