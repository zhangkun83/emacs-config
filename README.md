This is my Emacs configuration.

I am using it with Emacs 24.5.1.

Installation
============

Compiling Emacs
---------------

I prefer compiling Emacs from source, configured with
 `--with-x-toolkit=lucid` and `--without-xaw3d`. The lucid toolkit has
 a slick look-and-feel, but its 3D scroll bar is so ugly that I would
 disable it.

Tags
----

Either **etags** or the **ctags** that comes with major Linux
distributions won't correctly parse Java generics. **exuberant etags**
does parse Java generics correctly, but is buggy. I find
[ctags.io](https://github.com/universal-ctags/ctags) works best.

Font
----

To configure the font of the GUI (menu etc), append `xresources` to
`~/.xresources`([ref](http://www.nongnu.org/emacsdoc-fr/manuel/lucid-resources.html)).
Run `xrdb ~/.xresources` to load it.

On linux i use **terminus** as the main font, which is an x11 core
font. On ubuntu, it is in the `xfonts-terminus` package.

Supporting scripts
------------------

Create symbolic links under any directory in your search path, for the
scripts under the `bin` directory. They provide better support for
emacs server/client, and are also needed by markdown preview.
