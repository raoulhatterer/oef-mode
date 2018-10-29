

oef-mode
========

provide oef-mode (Online Exercise Format for wims) to emacs  

Features (Non-exhaustive)
-------------------------

* Syntax highlighting 
* Automatic indentation
* Completion suggestions
* Connection to a wims session
* OEF Menu with:
    - Examples
    - Useful Characters Shortcuts
    - OEF Commands and Keywords
 
Requirements
------------

- Emacs
- Yafolding
- rainbow-mode
- emmet-mode
- rainbow-delimiters
- expand-region
- cl-li
- company-mode

Installation
------------

I would recommend El-get http://wikemacs.org/wiki/El-get to install oef-mode

 - GnuTLS must be available
So the idea is that you copy/paste this code into your *scratch* buffer:
(gnutls-available-p)
hit C-j, and if you get 't' GnuTLS is available but if you get 'nil'  you have to install it:
On Aquamacs you need Aquamacs >=3.5

 - copy/paste this code into your *scratch* buffer: 

(url-retrieve
 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
 (lambda (s)
   (goto-char (point-max))
   (eval-print-last-sexp)))

 - hit C-j, and you have a working el-get.




Development
-----------

Before developing this package please remove it from Emacs if it was
installed before. You'll need [Cask][cask] to install the dependencies.
After cask installation I added in my `~/.profile` file:
```
# cask
 export PATH="/home/hatterer/.cask/bin:$PATH"
```

I usually use the following workflow when I develop this package:

1. `$ git clone https://github.com/raoulhatterer/oef-mode.git && cd oef-mode`;
2. `$ cask install`;
3. `$ cask exec emacs`;
4. `M-x find-file RET /path/to/oef-mode.el/oef-mode.el RET`;
5. `M-x eval-expression RET (add-to-list 'load-path default-directory) RET`;
6. `M-x eval-buffer RET`;
7. `M-x find-file RET /path/to/oef-mode.el/exemple.oef RET`;
8. Change something in the source code;
9. Go to the step `6`.

Licence
-------

Totally GPL




[cask]: http://cask.readthedocs.org/en/latest/
