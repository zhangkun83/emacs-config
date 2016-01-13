Emacs: 24.5.1 built from source with `--with-x-toolkit=lucid`

Works best with [ctags.io](https://github.com/universal-ctags/ctags).

To configure UI font, edit `~/.Xresources`[ref](http://www.nongnu.org/emacsdoc-fr/manuel/lucid-resources.html):
```
Emacs.pane.menubar.font: 8x16
Emacs.menu*.font: 8x16
Emacs.dialog*.font: 8x16
```

Run `xrdb ~/.Xresources` to load it.
