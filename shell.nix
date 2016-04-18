with import <nixpkgs> {}; 
let sourceFilesOnly = path: type:
    (lib.hasPrefix "target" (toString path)) ;
in stdenv.mkDerivation {
    name = "speckled";
    src = builtins.filterSource sourceFilesOnly ./.;
    buildInputs = [ leiningen openjdk ];
    M2REPOSITORY = ''m2repo'';
    buildPhase = ''
      lein uberjar
    '';
}

