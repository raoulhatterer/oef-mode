

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

eof-mode available on [MELPA](http://melpa.org/). Add the following to
your emacs config file somewhere (.emacs, init.el, whatever):

.. code:: python
 (require 'package)
 (add-to-list 'package-archives
              '("melpa" . "https://melpa.org/packages/") t)


Then use `M-x package-install RET pacmacs RET` to install the game.
 
 

Licence
-------

Totally GPL



