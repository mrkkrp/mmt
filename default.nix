let
  pkgs = import <nixpkgs> {};
  source = pkgs.lib.sourceByRegex ./. [
    "^mmt\.el$"
    "^test.*$"
  ];
in
pkgs.stdenv.mkDerivation {
  name = "mmt";
  src = source;
  buildInputs = [
    (pkgs.emacs28WithPackages (epkgs: [epkgs.ert-runner]))
  ];
  buildPhase = ''
    emacs -L . --batch -f batch-byte-compile *.el 2> stderr.txt
    cat stderr.txt
    ! grep -q ': Warning:' stderr.txt
  '';
  installPhase = ''
    LISPDIR=$out/share/emacs/site-lisp
    install -d $LISPDIR
    install *.el *.elc $LISPDIR
  '';
  checkPhase = ''
    emacs -L . --batch --eval "(progn (require 'ert-runner) (ert-run-tests t (lambda (x) nil)))"
  '';
  doCheck = true;
}
