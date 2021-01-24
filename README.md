

# oef-mode
Provide oef-mode (Online Exercise Format for wims) to emacs  

## Features (Non-exhaustive)
* Syntax highlighting 
* Automatic indentation
* Completion suggestions
* Connection to a wims session
* OEF Menu with:
    - Examples
    - Useful Characters Shortcuts
    - OEF Commands and Keywords
 
## Requirements
- Emacs or Aquamacs
- emmet-mode
- company-mode
- rainbow-delimiters
- rainbow-mode
- wrap-region
- expand-region
- cl-li
- yafolding

## Installation
I would recommend El-get http://wikemacs.org/wiki/El-get to install oef-mode


### On emacs


#### If you use el-get :
- Put this line in your init file
(load "oef-mode.el") ;; load the packaged named oef-mode

#### If you don't use el-get :
-  in the dir "~/.emacs.d/lisp/", (create that directory if it doesn't exist) create  a directory named oef-mode
- copy the content https://github.com/raoulhatterer/oef-mode in this directory

(add-to-list 'load-path "~/.emacs.d/lisp/oef-mode") ;; Tell emacs where is your personal elisp lib dir
(load "oef-mode.el") ;; load the packaged named oef-mode



### On Aquamacs

Usually user-specific customizations should go into ~/Library/Preferences/Aquamacs Emacs/Preferences.el
though El-get must be called before (package-initialize) yet customization.el contain (package-initialize)
we have to put user-specific customizations into customizations.el.

So :

- Edit  ~/Library/Preferences/Aquamacs Emacs/customizations.el  : 
Copy- paste this code before (package-initialize):

        ;; for El-get
        (with-eval-after-load 'tls
          (push "/private/etc/ssl/cert.pem" gnutls-trustfiles))
        
        (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
        (unless (require 'el-get nil 'noerror)
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
            (goto-char (point-max))
            (eval-print-last-sexp)))
        
        (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
        (el-get 'sync)
        
        (package-initialize)

(The 2 first lines are a way to have GnuTLC working because Aquamacs doesn't support GNUTLS. )

- Save and restart Aquamacs. el-get will be downloaded and installed .

- Install oef-mode from el-get : `M-x el-get-install` `oef-mode`


### Required packages

- Edit  ~/Library/Preferences/Aquamacs Emacs/Preferences.el  : 

Copy- paste this code :


        ;;; EMMET-MODE
        ;; oef-mode has to change the binding for 'emmet-wrap-with-markup
        ;; so add in your innit file:
        (require 'emmet-mode)
        (eval-after-load "emmet-mode"
          '(define-key emmet-mode-keymap (kbd "C-c C-c") 'oef-edit-in-browser))
        (eval-after-load "emmet-mode"
          '(define-key emmet-mode-keymap (kbd "C-c w") 'emmet-wrap-with-markup))
        (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
        ;; `emmet-mode' will automatically start with oef-mode
        (add-hook 'html-mode-hook 'emmet-mode)
        (add-hook 'css-mode-hook  'emmet-mode)
        (add-hook 'web-mode-hook  'emmet-mode)

        ;; * company
        (require 'company)
        ;; To use company-mode in all buffers, add the following line to your init file:
        (add-hook 'after-init-hook 'global-company-mode)

        ;; * `rainbow-delimiters'
        ;; add to your init file:
        (require 'rainbow-delimiters)
        (add-hook 'oef-mode-hook 'rainbow-delimiters-mode) ; Auto-start parens matching

        ;; * `rainbow-mode'
        ;; add to your init file:
        (require 'rainbow-mode)
        (add-to-list 'rainbow-html-colors-major-mode-list 'oef-mode) ; 
        (add-hook 'oef-mode-hook 'rainbow-mode) ; Auto-start CSS colorization

        ;; * `wrap-region'
        ;; add to your init file:
        (require 'wrap-region)
        (add-hook 'oef-mode-hook 'wrap-region-mode)

        ;; `Expand Region'
        (require 'expand-region)
        (global-set-key (kbd "C-=") 'er/expand-region)


## Development

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

## Licence


Totally GPL




[cask]: http://cask.readthedocs.org/en/latest/
