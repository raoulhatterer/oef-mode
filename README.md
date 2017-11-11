

oef-mode
========

provide oef-mode (Online Exercise Format for wims) to emacs  

Features (Non-exhaustive)
-------------------------

* Syntax highlighting 
* Automatic indentation
* Connection to a wims session
* OEF Menu with:
 * Examples
 * Useful Characters Shortcuts
 * OEF Commands and Keywords
 
Requirements
------------

- Emacs 24.4+
- Emacs should have displaying images support
- Emacs should have the [XPM](https://en.wikipedia.org/wiki/X_PixMap) image format support

Installation
------------

oef-mode available on [MELPA](http://melpa.org/). Add the following to
your emacs config file somewhere (.emacs, init.el, whatever):

```
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
```

Then use `M-x package-install RET pacmacs RET` to install the game.
 
Development
-----------

Before developing this package please remove it from Emacs if it was
installed before. You'll need [Cask][cask] to install the dependencies.

I usually use the following workflow when I develop this package:

1. `$ git clone git://github.com/codingteam/pacmacs.el.git && cd pacmacs.el`;
2. `$ cask install`;
3. `$ cask exec emacs`;
4. `M-x find-file RET /path/to/pacmacs.el/pacmacs.el RET`;
5. `M-x eval-expression RET (add-to-list 'load-path default-directory) RET`;
6. `M-x eval-buffer RET`;
7. `M-x pacmacs-start RET`;
8. `M-x pacmacs-quit RET`;
9. Change something in the source code;
10. Go to the step `6`.

Licence
-------

Totally GPL




[cask]: http://cask.readthedocs.org/en/latest/
