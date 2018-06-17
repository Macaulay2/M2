######
#
# Author: Brandon Elam Barker
# Authoremail: brandon.barker@gmail.com
# Start time: 08 June, 2018
#
######
with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "Macaulay2-dev";
  buildInputs = [
    atlas
    autoconf
    automake
    bison
    boehmgc
    boost
    cddlib
    curl
    emacs
    fflas-ffpack
    flex
    gfortran
    gdbm
    givaro
    glpk
    gmp
    gnumake
    gnupatch
    libatomic_ops
    liblapack
    libmpc
    libtool
    libxml2
    lzma    
    mpfr
    ncurses
    # ncurses5
    openssh
    pari
    pinentry_ncurses
    pkgconfig
    readline
    time
    unzip
    xorg.xauth
    xorg.xinit
    xorg.xkill
    xterm
    yasm
    zlib
  ];
  shellHook = ''
    export FC=gfortran
  '';

}
