Emacs: 24.5.1 built from source with `--with-x-toolkit=lucid`

Works best with [ctags.io](https://github.com/universal-ctags/ctags).

To configure UI font, append `Xresources` to
`~/.Xresources`([ref](http://www.nongnu.org/emacsdoc-fr/manuel/lucid-resources.html)).
Run `xrdb ~/.Xresources` to load it.

To get extra X11 core fonts on Ubuntu, install the `xfonts-*` packages. Use
`xfontsel` to get the string for a font.
