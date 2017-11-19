==============================================================================
The WWW Interactive Multipurpose Server (WIMS) project is designed for
supporting intensive mathematical exercises via the Internet
or in a computer-equipped classroom with server-side interactivity,
accessible at the address http://wims.unice.fr.
oef-mode is a mode for editing exercises (online exercise format) files
witch should have ".oef" extension to be recognized.
On linux you have to run `xdg-mime install oef-mime.xml' in a Terminal
and then restart your session to define .oef as a  new type of files.
==============================================================================

manually installation:

==============================================================================
This section is a tutorial on how to install oef-mode Emacs package manually.
First method for trying: "Load the File Manually"
To use the package, all you have to do is to make Emacs load the file 'oef-mode.el'.
alt+x load-file then give the file path.
Now, Emacs is aware of the package.  To activate, call “oef-mode” (with alt+x).
Other method: "Load File at Startup"
* Emacs (Linux):
If you want Emacs to load the file 'oef-mode.el' when it starts, put the file 'oef-mode.el'
in the dir "~/.emacs.d/lisp/", (create that directory if it doesn't exist).
By convention, the dir ~/.emacs.d/lisp/ is for packages you manually installed.
Then put the following (without ;;) in your Emacs init file "~/.emacs"
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
 (add-to-list 'load-path "~/.emacs.d/lisp/") ;; Tell Emacs where is your personal elisp lib dir
 (load "oef") ;; load the packaged named oef (best not to include the ending “.el” or “.elc”)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
before the line (package-initialize).
* aquamacs (OSX):
If you want aquamacs to load the file 'oef-mode.el' when it starts, put the file 'oef-mode.el'
in the dir "~/Library/Preferences/Aquamacs Emacs/Packages/lisp/"
(create that directory if it doesn't exist).
Then put the following (without ;;) in your aquamacs init file
"~/Library/Preferences/Aquamacs Emacs/Preferences.el"
not in  ~/.emacs  witch is deprecated -- meaning 'should not be used for new installations,
but will continue to be supported' -- in Aquamacs on OS X)
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
 (add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/Packages/lisp/oef-mode/")
 ;Tell Emacs where is your personal elisp lib dir
 (load "oef-mode")
 ; load the packaged named oef-mode (best not to include the ending “.el” or “.elc”)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
before the line (package-initialize).
How to Debug Aquamacs if you need to:
Past the following command in a terminal:
/Applications/Aquamacs.app/Contents/MacOS/Aquamacs -nw --debug-init
==============================================================================

Recommended packages:

==============================================================================
* emmet-mode
oef-mode is derived from sgml-mode so if you have installed emmet-mode
and added to your innit file:
   (require 'emmet-mode)
   (add-hook 'sgml-mode-hook 'emmet-mode) ; Auto-start on any markup modes
`emmet-mode' will automatically start with oef-mode
* company
`Company' is a modular completion framework.  I recommend it.
Enable `company-mode' in all buffers with M-x global-company-mode.
* rainbow-delimiters
add to your init file:
   (require 'rainbow-delimiters)
   (add-hook 'oef-mode-hook 'rainbow-delimiters-mode) ; Auto-start parens matching
* rainbow-mode
add to your init file:
   (require 'rainbow-mode)
   (add-to-list 'rainbow-html-colors-major-mode-list 'oef-mode) ;
   (add-hook 'oef-mode-hook 'rainbow-mode) ; Auto-start HTML and CSS colorization
* yafolding-mode
Folding code blocks based on indentation
Automatically installed and launch
* LaTeX-math-mode
* wrap-region-mode
add to your init file:
   (require 'wrap-region)
   (add-hook 'oef-mode-hook 'wrap-region-mode)
* expand-region
add to your init file:
   (require 'expand-region)
   (global-set-key (kbd "C-=") 'er/expand-region)

==============================================================================
