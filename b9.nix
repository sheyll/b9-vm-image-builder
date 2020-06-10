{haskellPackages,
 nix-gitignore
 }:

let
  cleanSrcGitIgnore = nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  cleanSrc = cleanSrcGitIgnore ./.;

in 
  haskellPackages.callCabal2nix "b9" cleanSrc {}
