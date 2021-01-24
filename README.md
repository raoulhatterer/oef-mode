

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
- Emacs:
    - OSX: https://emacsformacosx.com  or Aquamacs
    - GNU/Linux: Most GNU/Linux distributions provide GNU Emacs in their repositories, which is the recommended way to install Emacs unless you always want to use the latest release.
- emmet-mode: a way to Write HTML and CSS quicker. Place point in a zencoding snippet and press C-j to expand it. https://github.com/rooney/zencoding
- company-mode: Company is a text completion framework for Emacs. The name stands for "complete anything". It uses pluggable back-ends and front-ends to retrieve and display completion candidates. https://company-mode.github.io
- rainbow-delimiters: highlights delimiters such as parentheses, brackets or braces according to their depth. Each successive level is highlighted in a different color. This makes it easy to spot matching delimiters. https://github.com/Fanael/rainbow-delimiters
- rainbow-mode: This minor mode sets background color to strings that match color names, e.g. #0000ff is displayed in white with a blue background.
- wrap-region: Wrap Region is a minor mode for Emacs that wraps a region with punctuations. Select a region and press any of the following keys: ", ', (, {, [ or press "C-c w" to wrap with markup, e.g. <div>Selection</div> 
- expand-region: 
Expand region increases the selected region by semantic units. Just keep pressing "C-=" until it selects what you want. f you expand too far, you can contract the region by pressing - (minus key). https://github.com/magnars/expand-region.el
- cl-lib: Some CommonLisp functions and macros can be used in EmacsLisp programs. 
- yafolding: Folding code blocks based on indentation. https://github.com/zenozeng/yafolding.el

## Installation
I would recommend El-get http://wikemacs.org/wiki/El-get to install oef-mode


### On emacs


#### If you use el-get :

- Install oef-mode from el-get : `M-x el-get-install` `oef-mode`
- Put this line in your init file (`~/.emacs.d/init.el`)
`(load "oef-mode.el") ;; load the package named oef-mode`

#### If you don't use el-get :
-  in the dir "~/.emacs.d/lisp/", (create that directory if it doesn't exist) create  a directory named oef-mode
- Download the zip file https://github.com/raoulhatterer/oef-mode/archive/master.zip unzip the content in this directory
- Add those lines in your init file:
    - `(add-to-list 'load-path "~/.emacs.d/lisp/oef-mode") ;; Tell emacs where is your personal elisp lib dir`

    - `(load "oef-mode.el") ;; load the package named oef-mode`



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



## Licence


Totally GPL




[cask]: http://cask.readthedocs.org/en/latest/
