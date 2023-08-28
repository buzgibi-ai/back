let
   pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-23.05.tar.gz") {};
in
pkgs.mkShell {
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive"; 
  buildInputs = [ pkgs.postgresql pkgs.lzma pkgs.zlib pkgs.imagemagick pkgs.glibcLocales];
  }