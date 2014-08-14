{ stdenv, fetchurl, gnutls, pkgconfig, ncurses
, texinfo, libxml2, imagemagick
}:

stdenv.mkDerivation rec {
  name = "emacs-24.3.92";

  src = fetchurl {
    url    = "http://alpha.gnu.org/gnu/emacs/pretest/${name}.tar.xz";
    sha256 = "1dxy6hxpj40ahpq3qrndpfra8d0q2wn05qb50dah08g2rfbm1bp5";
  };

  preConfigure = ''
    unset MACOSX_DEPLOYMENT_TARGET

    # The search for 'tputs' will fail because it's in ncursesw within the
    # ncurses package, yet Emacs' configure script only looks in ncurses.
    # Further, we need to make sure that the -L option occurs before mention
    # of the library, so that it finds it within the Nix store.
    sed -i 's/tinfo ncurses/tinfo ncursesw/' configure
    ncurseslib=$(echo ${ncurses}/lib | sed 's#/#\\/#g')
    sed -i "s/OLIBS=\$LIBS/OLIBS=\"-L$ncurseslib \$LIBS\"/" configure
    sed -i 's/LIBS="\$LIBS_TERMCAP \$LIBS"/LIBS="\$LIBS \$LIBS_TERMCAP"/' configure

    configureFlagsArray=(
      --with-xml2=yes
      --with-gnutls=yes
      --with-imagemagick=no
      --with-ns
      --disable-ns-self-contained
    )
    makeFlagsArray=(
      CC=/usr/bin/gcc
      CFLAGS=-O3
      LDFLAGS=-O3
    );
  '';

  buildInputs =
    [ pkgconfig gnutls ncurses libxml2 texinfo imagemagick ];

  postInstall = ''
    mkdir -p $out/Applications
    cp -r nextstep/Emacs.app $out/Applications/Emacs.app
    cat >$out/share/emacs/site-lisp/site-start.el <<EOF
    ;; nixos specific load-path
    (when (getenv "NIX_PROFILES") (setq load-path
                          (append (reverse (mapcar (lambda (x) (concat x "/share/emacs/site-lisp/"))
                                                   (split-string (getenv "NIX_PROFILES"))))
                           load-path)))
        
    ;; make tramp work for NixOS machines
    (eval-after-load 'tramp '(add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))
    EOF
  '';

  doCheck = true;

  meta = with stdenv.lib; {
    description = "GNU Emacs 24, the extensible, customizable text editor";
    homepage    = http://www.gnu.org/software/emacs/;
    license     = licenses.gpl3Plus;
    maintainers = with maintainers; [ chaoflow lovek323 simons the-kenny ];
    platforms   = platforms.all;

    longDescription = ''
      GNU Emacs is an extensible, customizable text editor—and more.  At its
      core is an interpreter for Emacs Lisp, a dialect of the Lisp
      programming language with extensions to support text editing.

      The features of GNU Emacs include: content-sensitive editing modes,
      including syntax coloring, for a wide variety of file types including
      plain text, source code, and HTML; complete built-in documentation,
      including a tutorial for new users; full Unicode support for nearly all
      human languages and their scripts; highly customizable, using Emacs
      Lisp code or a graphical interface; a large number of extensions that
      add other functionality, including a project planner, mail and news
      reader, debugger interface, calendar, and more.  Many of these
      extensions are distributed with GNU Emacs; others are available
      separately.
    '';
  };
}
