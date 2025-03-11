{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipData";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''Functions for extracting data from formulas and
    environments, and for describing data, for addressing missing
    data, and for setting up data in a neat format for other
    analyses.'';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    flipAPI
    flipTransformations
    flipImputation
    flipFormat
    CVXR
    flipTime
    flipU
    haven
    icarus
    lubridate
    stringdist
    verbs
    survey
    stringr
  ];
}
