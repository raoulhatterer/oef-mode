;;; oef-mode.el --- Major mode for editing oef (wims) templates
;;; -*- coding: utf-8 -*-

;; Copyright 2017-2023 Raoul HATTERER

;; Author: Raoul Hatterer <hatterer.raoul@gmail.com>
;; Version: "20231014.0228"
;; previous Version: "20231013.1834"
;; Maintainer: Raoul Hatterer <hatterer.raoul@gmail.com>

;; Created: July 2017
;; Keywords: languages
;; URL: http://github.com/raoulhatterer/oef-mode
;; Package-Requires: ((emmet-mode)(company-mode)(rainbow-delimiters)(rainbow-mode)(yafolding)(wrap-region)(expand-region)(cl-lib))
;; News:
;; Package-Type: multi

;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;==============================================================================
;; The WWW Interactive Multipurpose Server (WIMS) project is designed for
;; supporting intensive mathematical exercises via the Internet
;; or in a computer-equipped classroom with server-side interactivity,
;; accessible at the address http://wims.unice.fr.
;; oef-mode is a mode for editing exercises (online exercise format) files
;; witch should have ".oef" extension to be recognized.
;; On linux you have to run `xdg-mime install oef-mime.xml' in a Terminal
;; and then restart your session to define .oef as a  new type of files.
;;==============================================================================

;;; manually installation:

;;==============================================================================
;; This section is a tutorial on how to install oef-mode Emacs package manually.
;; First method for trying: "Load the File Manually"
;; To use the package, all you have to do is to make Emacs load the file 'oef-mode.el'.
;; alt+x load-file then give the file path.
;; Now, Emacs is aware of the package.  To activate, call “oef-mode” (with alt+x).
;; Other method: "Load File at Startup"
;; * Emacs (Linux):
;; If you want Emacs to load the file 'oef-mode.el' when it starts, put the file 'oef-mode.el'
;; in the dir "~/.emacs.d/lisp/", (create that directory if it doesn't exist).
;; By convention, the dir ~/.emacs.d/lisp/ is for packages you manually installed.
;; Then put the following (without ;;) in your Emacs init file "~/.emacs"
;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;;  (add-to-list 'load-path "~/.emacs.d/lisp/") ;; Tell Emacs where is your personal elisp lib dir
;;  (load "oef-mode") ;; load the packaged named oef (best not to include the ending “.el” or “.elc”)
;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; before the line (package-initialize).
;; * aquamacs (OSX):
;; If you want aquamacs to load the file 'oef-mode.el' when it starts, put the file 'oef-mode.el'
;; in the dir "~/Library/Preferences/Aquamacs Emacs/Packages/lisp/"
;; (create that directory if it doesn't exist).
;; Then put the following (without ;;) in your aquamacs init file
;; "~/Library/Preferences/Aquamacs Emacs/Preferences.el"
;; not in  ~/.emacs  witch is deprecated -- meaning 'should not be used for new installations,
;; but will continue to be supported' -- in Aquamacs on OS X)
;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;;  (add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/Packages/lisp/oef-mode/")
;;  ;Tell Emacs where is your personal elisp lib dir
;;  (load "oef-mode")
;;  ; load the packaged named oef-mode (best not to include the ending “.el” or “.elc”)
;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; before the line (package-initialize).
;; How to Debug Aquamacs if you need to:
;; Past the following command in a terminal:
;; /Applications/Aquamacs.app/Contents/MacOS/Aquamacs -nw --debug-init
;;==============================================================================

;;; Recommended packages:

;;==============================================================================
;; * emmet-mode
;; Minor mode for writing HTML and CSS markup.
;; `oef-edit-in-browser' is bound to `C-c C-c' and emmet-mode is bound to `C-c C-c w'
;;  Which means it's going to block `oef-edit-in-browser'.  To fix this add this in your init file:
;; (require 'emmet-mode)
;; (eval-after-load "emmet-mode"
;;   '(define-key emmet-mode-keymap (kbd "C-c C-c") 'oef-edit-in-browser))
;; (eval-after-load "emmet-mode"
;;   '(define-key emmet-mode-keymap (kbd "C-c w") 'emmet-wrap-with-markup))
;; (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;; ;; `emmet-mode' will automatically start with oef-mode
;; (add-hook 'html-mode-hook 'emmet-mode)
;; (add-hook 'css-mode-hook  'emmet-mode)
;; (add-hook 'web-mode-hook  'emmet-mode)
;; * company-mode
;; `Company-mode' is a modular completion framework.
;; To use company-mode in all buffers, add the following line to your init file:
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)
;; * `rainbow-delimiters'
;; add to your init file:
;; (require 'rainbow-delimiters)
;;    (add-hook 'oef-mode-hook 'rainbow-delimiters-mode) ; Auto-start parens matching
;; * `rainbow-mode'
;; add to your init file:
;; (require 'rainbow-mode)
;;    (add-to-list 'rainbow-html-colors-major-mode-list 'oef-mode) ; 
;;    (add-hook 'oef-mode-hook 'rainbow-mode) ; Auto-start HTML and CSS colorization
;; * `yafolding'
;; Folding code blocks based on indentation
;; Automatically installed and launch
;; * `wrap-region'
;; add to your init file:
;; (require 'wrap-region)
;; (add-hook 'oef-mode-hook 'wrap-region-mode)
;; * `expand-region'
;; add to your init file:
;; (require 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)

;;==============================================================================

;;; Code:
(unless (featurep 'aquamacs); subdir inclusion so oef-mode dir will be included to emacs load path
  (let ((default-directory  "~/.emacs.d/"))
    (normal-top-level-add-subdirs-to-load-path)))

(require 'cl-lib)

;;---- KILL-ALL-LOCAL-VARIABLES-------------------------------------------------

(kill-all-local-variables)
;; This function eliminates all the buffer-local variable bindings of the current buffer
;; except for variables marked as permanent and local hook functions that have a non-nil
;; permanent-local-hook property (see Setting Hooks).
;; As a result, the buffer will see the default values of most variables.
;; This function also resets certain other information pertaining to the buffer: it sets
;; the local keymap to nil, the syntax table to the value of (standard-syntax-table),
;; the case table to (standard-case-table), and the abbrev table to the value of fundamental-mode-abbrev-table.
;; The very first thing this function does is run the normal hook change-major-mode-hook.

;;---- AUTO-START --------------------------------------------------------------

(add-hook 'sgml-mode-hook 'oef-mode-hook)

(defun oef-mode-hook ()
  "(De)Activation of some (un)usefull minor modes."
  (auto-fill-mode -1)
  (yafolding-mode 1)
  (autoload 'LaTeX-math-mode "latex" "LaTeX-math-mode" t)
  (LaTeX-math-mode)
  (flycheck-mode -1) ; TODO implement Syntax checking for oef-mode
  )

;;---- CONSTS ------------------------------------------------------------------


(defconst oef-mode-version "20231014.0228"
  "Oef-mode version. For maintainer: Has to be generated by  `(format-time-string \"%Y%m%d.%H%M\")' in *scratch* and past here before commit")
;; (format-time-string "%Y%m%d.%H%M") evaluation in  *scratch* buffer

;;---- GROUPS ------------------------------------------------------------------

(defgroup oef nil
  "Mode for editing OEF (wims) files"
  :group 'languages
  :prefix "oef-mode"
  :link '(url-link :tag "Site" "http://wims.unice.fr")
  :link '(url-link :tag "Repository" "https://github.com/raoulhatterer/oef-mode"))

(defgroup oef-faces nil
  "Faces for syntax highlighting."
  :group 'oef    ; CUSTOMIZE > PROGRAMMING > LANGUAGE > OEF > OEF-FACES 
  :group 'faces) ; CUSTOMIZE > FACES > OEF-FACES

;;---- FACES -------------------------------------------------------------------

(defface oef-font-function-name-face
  '((t (:foreground "orange red")))
  "Face for functions"
  :group 'oef-faces)

(defface oef-font-equal-face
  '((t :inherit oef-font-function-name-face))
  "Face for equal sign"
  :group 'oef-faces)

(defface oef-font-command-face
  '((t :inherit oef-font-function-name-face))
  "Face for commands"
  :group 'oef-faces)

(defface oef-font-answer-command-face
  '((t
     (:box
      (:line-width 2 :color "#FFF2F2" :style nil)
      :background "#FFF2F2"
      :inherit
      (oef-font-command-face))))
  "Face for answer command"
  :group 'oef-faces)

(defface oef-font-hint-command-face
  '((t
     (:box
      (:line-width 1 :color "orange red" :style nil)
      :inherit
      (oef-font-command-face))))
  "Face for hint command"
  :group 'oef-faces)

(defface oef-font-statement-command-face
  '((t
     (:height 1.2 :weight extra-bold :inherit
	      (oef-font-answer-command-face))))
  "Face for statement command"
  :group 'oef-faces)

(defface oef-font-positivenumber-face
  '((t (:foreground "#555555")))
  "Face for positive number"
  :group 'oef-faces)

(defface oef-font-documentation-face
  '((t :inherit font-lock-doc-face))
  "Face for documentation"
  :group 'oef-faces)

(defface oef-font-htag-face
  '((t (:foreground "snow4")))
  "Face for h1 h2 h3 tags"
  :group 'oef-faces)

(defface oef-font-litag-face
  '((t (:foreground "magenta")))
  "Face for li tags"
  :group 'oef-faces)

(defface oef-font-h1text-lightbg-face
  '((t
     (:width normal :height 1.1 :weight bold :underline
             (:color "red" :style line)
             :foreground "black")))
  "Face for sections between h1 tags when the background is light"
  :group 'oef-faces)

(defface oef-font-h1text-darkbg-face
  '((t
     (:width normal :height 1.1 :weight bold :underline
             (:color foreground-color :style line)
             :foreground "white")))
  "Face for sections between h1 tags when the background is dark"
  :group 'oef-faces)

(defface oef-font-h1text-face
  '((t
     (:inherit
      (oef-font-h1text-lightbg-face))))
  "Face for sections between h1 tags"
  :group 'oef-faces)


(defface oef-font-h2text-lightbg-face
  '((t
     (:width normal :height 1.1 :weight bold :foreground "black")))
  "Face for sub-sections between h2 tags when the background is light"
  :group 'oef-faces)

(defface oef-font-h2text-darkbg-face
  '((t
     (:width normal :height 1.05 :weight bold :foreground "white")))
  "Face for sub-sections between h2 tags when the background is dark"
  :group 'oef-faces)

(defface oef-font-h2text-face
  '((t
     (:inherit
      (oef-font-h2text-lightbg-face))))
  "Face for sub-sections between h2 tags"
  :group 'oef-faces)

(defface oef-font-h3text-face
  '((t
     (:width normal :underline
             (:color foreground-color :style line)
             )))
  "Face for sub-sub-sections between h3 tags"
  :group 'oef-faces)


(defface oef-font-answer-type-face
  '((t (:foreground "#CC9900")))
  "Face for answer type and options"
  :group 'oef-faces)

(defface oef-font-control-face
  '((t (:foreground "#FF8C00")))
  "dark orange"
  :group 'oef-faces)

(defface oef-font-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments"
  :group 'oef-faces)

(defface oef-font-warning-face
  '((t :inherit font-lock-warning-face))
  "Face for warning"
  :group 'oef-faces)

(defface oef-font-type-face
  '((t :inherit font-lock-type-face))
  "Face for type"
  :group 'oef-faces)

(defface oef-font-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keywords"
  :group 'oef-faces)

(defface oef-font-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for variables"
  :group 'oef-faces)

(defface oef-font-formula-braces-face
  '((t  :background "LemonChiffon2"))
  "Face for mathematical formulas"
  :group 'oef-faces)

(defface oef-font-mark-face
  '((t  :background "Yellow"))
  "Face for html mark tag"
  :group 'oef-faces)

(defface oef-font-draw-face
  '((t  :foreground "sea green"))
  "Face for draw commands"
  :group 'oef-faces)


;;---- VARS --------------------------------------------------------------------

(defcustom oef-line-spacing 0.1
  "Additional space to put between lines when displaying an `oef-mode' buffer."
  :group 'oef)

(defcustom oef-these-phrases-are-made-of-words-not-keywords
  '("la solution" "de solution" "en solution" "une solution" "d'une solution" "des conditions" "tout point" "du point" "plusieurs points" "ses points" "un point")
  "You can add your own phases here."
  :group 'oef)

(defcustom oef-author "First Name, Last Name"
  "Author name"
  :group 'oef)

(defcustom oef-email "your@email"
  "Author email"
  :group 'oef)

(defcustom oef-language "fr"
  "Language"
  :group 'oef)

(defvar oef-grabed-word-for-goto nil
  "This variable is used to navigate in the buffer.")

(defvar oef-answers-index nil
  "This variable is used to navigate in the buffer.")

(defvar oef-menu-answers-options ; "STAR BLANK TYPE" or "BLANK BLANK BLANK OPTION" in the menu DONE
  '("type=" "option=" "weight=" "* type=default"
    "* type=raw"
    "   option=noaccent" "   option=nocase" "   option=nodigit" "   option=nomathop" "   option=noparenthesis" "   option=nopunct" "   option=noquote" "   option=nospace" "   option=reaccent" "   option=singlespace" "   option=symtext"
    "* type=numeric"
    "   option=comma" "   option=absolute" "   option=nonstop noanalyzeprint" "   option=noanalyzeprint nonstop" "   option=nonstop" "   option=noanalyzeprint"
    "* type=function"
    "   option=integer"
    "* type=equation"
    "   option=eqsign=yes"
    "* type=algexp"
    "* type=litexp"
    "* type=formal"
    "* type=case"
    "   option=noreaccent"
    "* type=nocase"
    "* type=atext"
    "* type=checkbox"
    "* type=radio"
    "* type=flashcard"
    "* type=multipleclick"
    "   option=nolegend"
    "   option=split"
    "   option=shuffle"
    "   option=sort"
    "   option=eqweight"
    "   option=sort split"
    "   option=sort eqweight"
    "* type=menu"
    "   option=shuffle multiple=1"
    "   option=shuffle multiple=2"
    "   option=shuffle multiple=3"
    "   option=shuffle multiple=4"
    "   option=shuffle multiple=5"
    "   option=shuffle multiple=6"
    "   option=shuffle multiple=7"
    "   option=shuffle multiple=8"
    "   option=sort multiple=1"
    "   option=sort multiple=2"
    "   option=sort multiple=3"
    "   option=sort multiple=4"
    "   option=sort multiple=5"
    "   option=sort multiple=6"
    "   option=sort multiple=7"
    "   option=sort multiple=8"
    "* type=mark"
    "   option=color"
    "* type=click"
    "* type=multipleclick"
    "* type=flashcard"
    "   option=show"
    "* type=chembrut"
    "* type=chemdraw"
    "* type=chemclick"
    "* type=chemeq"
    "* type=chset"
    "* type=clickfill"
    "* type=dragfill"
    "* type=clicktile"
    "* type=clock"
    "* type=compose"
    "* type=complex"
    "* type=coord"
    "* type=correspond"
    "* type=crossword"
    "* type=draw"
    "* type=geogebra"
    "* type=javacurve"
    "* type=jmolclick"
    "* type=jsxgraph"
    "* type=jsxgraphcurve"
    "* type=keyboard"
    "* type=matrix"
    "* type=numexp"
    "* type=puzzle"
    "* type=range"
    "* type=reorder"
    "* type=set"
    "* type=fset"
    "* type=aset"
    "* type=units"
    "* type=sigunits"
    "* type=symtext"
    "* type=time"
    "* type=wlist"
    )
  "Used for a dedicated submenu thanks to `oef-get-answers-options'.")

(defvar oef-answers-options nil
  "`oef-answers-options' used for highlighting and for completion (`oef-completions').  It is automatically build from the variable `oef-menu-answers-options' a list of answers types and options.")

(defvar oef-definitions-commands ; in the menu DONE
  '("title{«Exercise Title»}"
    "language{«en or fr»}"
    "author{«forename1»,«name1»;«forename2»,«name2»}"
    "email{«email1»,«email2»}"
    "format{html}"
    "css{<style></style>}"
    "keywords{«keyword1»,«keyword2»}"
    "credits{«acknowledgement of those who contributed to the document or exercise, whether through ideas or in a more direct sense»}"
    "description{«forTheStudent»}"
    "observation{«forTheTeacher»}"
    "precision{1000}"
    "range{«n1..n2»}"
    "computeanswer{«yes» or «no»}"
    "steps{«&opt:choice1,»«reply1»
«&opt:choice2,»«reply2»,«reply3»
«&opt:choice3»}"
    "nextstep{<>}"
    "statement{«message»}"
    "answer{«message»}{«goodAnswer»}{«&opt:type=»}{«&opt:option=»}{«&opt:weight=»}"
    "choice{«message»}{«goodAnswers»}{«badAnswers»}{«&opt:option=»}{«&opt:weight=»}"
    "condition{«message»}{«conditions»}{«&opt:option=»}{«&opt:weight=»}"
    "solution{«solution»}"
    "hint{«hint»}"
    "help{«popupHelp»}"
    "feedback{«condition»}{«message»}"
    "conditions{«conditionsNumbers»}"
    "latex{}"
    "embed{«reply1»,«&opt:option»}"
    )
  "In this variable we have the definitions of `oef-commands'.  Used to get `oef-commands' (thanks to `oef-get-list-commands-names') for highlighting.  Also used to get the 'Commands menu' (thanks to `oef-get-menu-commands')."
  )

(defvar oef-commands nil
  "`oef-commands' is automatically  build for highlighting and completion from `oef-definitions-commands' a list of commands definitions.")

(defvar oef-definitions-special-commands ; in the menu DONE
  '(
    "special{imagefill «parameters»}"
    "special{expandlines «parameters»}"
    "special{help «parameters»}"
    "special{tabs2lines «parameters»}"
    "special{rename «parameters»}"
    "special{tooltip «parameters»}"
    "special{codeinput «parameters»}"
    "special{imageinput «parameters»}"
    "special{mathmlinput «parameters»}"
    "special{drawinput «parameters»}"
    )
  "In this variable we have the definitions of `oef-special-commands'.  Used to get `oef-special-commands' (thanks to `oef-get-list-commands-names') for highlighting.  Also used to get the 'Special Commands menu' (thanks to `oef-get-menu-special-commands')."
  )

(defvar oef-special-commands nil
  "`oef-special-commands' is automatically  build from `oef-definitions-special-commands' a list of special commands definitions.")

(defvar oef-doc-commands ; used for highlighting DONE ; in the menu INPROGRESS
  '("calcform"
    "comment"
    "def"
    "define"
    "docform"
    "form"
    "draw"
    "embed"
    "exercise"
    "tool"
    "help"
    "adm"
    "fold"
    "for"
    "form"
    "if"
    "ifval"
    "link"
    "ref"
    "href"
    "reload"
    ;;"slib"
    "tooltip"
    "while")
  )

(defvar oef-storage-types
  '("real" "complex" "text" "integer" "rational" "function" "matrix" )
  "List of Oef Variable Types.  Used for highlighting and completion.  See also `oef-menu-exo-init-types' and `oef-menu-doc-init-types'."
  )

(defvar oef-menu-exo-init-types ; in the menu DONE
  '(
    "complex{=}"
    "function{=}"
    "integer{=}"
    "matrix{=}"
    "rational{=}"
    "real{=}"
    "text{=}"
    )
  "In this variable we have the definitions of variables initialization commands to be used in an exercise.  Used to get the 'Initialization menu' (thanks to `oef-get-exo-init-types').  See also `oef-storage-types' and `oef-menu-doc-init-types'."
  )

(defvar oef-menu-doc-init-types ; in the menu DONE
  '(
    "def{complex =}"
    "def{function =}"
    "def{integer =}"
    "def{matrix =}"
    "def{rational =}"
    "def{real =}"
    "def{text =}"
    )
  "In this variable we have the definitions of variables initialization commands to be used in a document.  Used to get the 'Initialization menu' (thanks to `oef-get-doc-init-types').  See also `oef-storage-types' and `oef-menu-exo-init-types'."
  )

(defvar oef-defined-variables ; in the menu DONE
  '("reply " "choice" "step" "sc_reply" "reply_" "help_subject" "oef_firstname" "oef_lastname" "oef_login" "oef_now" "oef_lang" )
  "Used for highlighting and completion and for a submenu `Defined_Variables'."
  )

(defvar oef-comparison-operators ;  "="  "<"  ">"  tested in another place ; in the menu DONE
  '("==" "<="  ">=" "isin" "notin" "iswordof" "notwordof" "isvarof" "notvarof" "isvariableof" "notvariableof" "isitemof" "notitemof" "islineof" "notlineof" "issamecase" "notsamecase" "issametext" "notsametext" "or" "and")
  "Used for highlighting and completion and for a submenu `Comparisons'."
  )

(defvar oef-language-reserved-words ; in the menu DONE
  '("to" "of" "within" "in" "into" "by" "internal")
  "Used for highlighting and completion and for a submenu `Comparisons'."
  )

(defvar oef-definitions-wims-functions ; in the menu DONE
  '( "wims(append «parameters»)"
     "wims(nonempty «parameters»)"
     "wims(getopt «parameters»)"
     "wims(replace «parameters»)"
     "wims(embraced «parameters»)"
     "wims(randitem «parameters»)"
     "wims(text «parameters»)"
     "wims(select «parameters»)"
     "wims(upper «parameters»)"
     "wims(nospace «parameters»)"
     "wims(sort «parameters»)"
     "wims(makelist «parameters»)"
     "wims(for «parameters»)"
     "wims(values «parameters»)"
     "wims(rows2lines «parameters»)"
     "wims(lines2items «parameters»)"
     "wims(items2words «parameters»)"
     "wims(tolower «parameters»)"
     )
  "Used for highlighting and completion and for a submenu `Wims Functions'."
  )

(defvar oef-wims-functions nil
  "`oef-wims-functions' is automatically  build from `oef-definitions-wims-functions' a list of wims functions definitions.")

(defvar oef-definitions-slib-algebra ; in the menu DONE
  '("slib(algebra/partitionconj «parameters»)"
    "slib(algebra/partitiondraw «parameters»)"
    "slib(algebra/partitionlex «parameters»)"
    "slib(algebra/slopedraw «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Algebra' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-analysis ; in the menu DONE
  '("slib(analysis/inversedomain «parameters»)"
    "slib(analysis/odejs «parameters»)"
    "slib(analysis/odejs2 «parameters»)"
    "slib(analysis/odephase «parameters»)"
    "slib(analysis/rungekutta «parameters»)"
    "slib(analysis/slope.js «parameters»)"
    "slib(analysis/slopefield «parameters»)"
    "slib(analysis/slopefield_img «parameters»)"
    "slib(analysis/slopefield_js «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Analysis' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-chemistry ; in the menu DONE
  '("slib(chemistry/atom «parameters»)"
    "slib(chemistry/brut2html «parameters»)"
    "slib(chemistry/chemeq_add «parameters»)"
    "slib(chemistry/chemeq_compare «parameters»)"
    "slib(chemistry/chemeq_components «parameters»)"
    "slib(chemistry/chemeq_el «parameters»)"
    "slib(chemistry/chemeq_equilibrium «parameters»)"
    "slib(chemistry/chemeq_mass «parameters»)"
    "slib(chemistry/chemeq_rev «parameters»)"
    "slib(chemistry/chemeq_rq «parameters»)"
    "slib(chemistry/chemeq_tex «parameters»)"
    "slib(chemistry/chemshow «parameters»)"
    "slib(chemistry/cram «parameters»)"
    "slib(chemistry/jmolbutton «parameters»)"
    "slib(chemistry/jmolcheckbox «parameters»)"
    "slib(chemistry/jmolradiogroup «parameters»)"
    "slib(chemistry/jmolshow «parameters»)"
    "slib(chemistry/jmolshow_init «parameters»)"
    "slib(chemistry/leftind «parameters»)"
    "slib(chemistry/molarmass «parameters»)"
    "slib(chemistry/molecule «parameters»)"
    "slib(chemistry/moleculeViewer «parameters»)"
    "slib(chemistry/newman «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Chemistry' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-circuits ; in the menu DONE
  '("slib(circuits/complist «parameters»)"
    "slib(circuits/comppos «parameters»)"
    "slib(circuits/draw «parameters»)"
    "slib(circuits/drawcomp «parameters»)"
    "slib(circuits/drawwire «parameters»)"
    "slib(circuits/range «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Circuits' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-data ; in the menu DONE
  '("slib(data/columnsort «parameters»)"
    "slib(data/randline «parameters»)"
    "slib(data/random «parameters»)"
    "slib(data/randrec «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Data' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-draw ; in the menu DONE
  '("slib(draw/balance «parameters»)"
    "slib(draw/brokenlinegraph «parameters»)"
    "slib(draw/clock «parameters»)"
    "slib(draw/convpixel «parameters»)"
    "slib(draw/domino «parameters»)"
    "slib(draw/drtgraduee «parameters»)"
    "slib(draw/graphviz «parameters»)"
    "slib(draw/graphvizpoints «parameters»)"
    "slib(draw/meter «parameters»)"
    "slib(draw/polygon «parameters»)"
    "slib(draw/radar «parameters»)"
    "slib(draw/randpolygon «parameters»)"
    "slib(draw/range «parameters»)"
    "slib(draw/repdroite «parameters»)"
    "slib(draw/repere «parameters»)"
    "slib(draw/thermometer «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Draw' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-function ; in the menu DONE
  '("slib(function/bounds «parameters»)"
    "slib(function/bounds2 «parameters»)"
    "slib(function/integrate «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Function' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-games ; in the menu DONE
  '("slib(games/chessboard «parameters»)"
    "slib(games/chessimage «parameters»)"
    "slib(games/chessmv «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Games' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-geogebra ; in the menu DONE
  '("slib(geo2D/geogebra «parameters»)"
    "slib(geo2D/geogebra3 «parameters»)"
    "slib(geo2D/geogebracommand «parameters»)"
    "slib(geo2D/geogebraoption «parameters»)"
    "slib(geo2D/ggb2jsxgraph «parameters»)"
    "slib(geo2D/jsxgraph «parameters»)"
    "slib(geo3D/3Dviewer «parameters»)"
    "slib(geo3D/CaR «parameters»)"
    "slib(geo3D/Convex3D «parameters»)"
    "slib(geo3D/draw «parameters»)"
    "slib(geo3D/drawtile «parameters»)"
    "slib(geo3D/off2jmol «parameters»)"
    "slib(geo3D/off2xyz «parameters»)"
    "slib(geo3D/polyhedra «parameters»)"
    "slib(geo3D/polyhedradual «parameters»)"
    "slib(geo3D/threeD «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Geogebra' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-graph ; in the menu DONE
  '("slib(graph/connexcomponent «parameters»)"
    "slib(graph/connexity «parameters»)"
    "slib(graph/distance «parameters»)"
    "slib(graph/draw «parameters»)"
    "slib(graph/drawcc «parameters»)"
    "slib(graph/drawtree «parameters»)"
    "slib(graph/gpt «parameters»)"
    "slib(graph/graphviz «parameters»)"
    "slib(graph/path «parameters»)"
    "slib(graph/randomconnex «parameters»)"
    "slib(graph/randomeuler «parameters»)"
    "slib(graph/randtree «parameters»)"
    "slib(graph/shortpath «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Graph' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-graphpaper ; in the menu DONE
  '("slib(graphpaper/correct_milli «parameters»)"
    "slib(graphpaper/func «parameters»)"
    "slib(graphpaper/func_milli «parameters»)"
    "slib(graphpaper/imgpoints «parameters»)"
    "slib(graphpaper/millimetre «parameters»)"
    "slib(graphpaper/strings «parameters»)"
    "slib(graphpaper/tograph «parameters»)"
    "slib(graphpaper/whereclick «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Graphpaper' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-lang ; in the menu DONE
  '("slib(lang/enword2ipa «parameters»)"
    "slib(lang/epd2ipa «parameters»)"
    "slib(lang/fname «parameters»)"
    "slib(lang/fraccord «parameters»)"
    "slib(lang/frapostrophe «parameters»)"
    "slib(lang/frartdef «parameters»)"
    "slib(lang/frcodcoi «parameters»)"
    "slib(lang/frverbconj «parameters»)"
    "slib(lang/images «parameters»)"
    "slib(lang/randomword «parameters»)"
    "slib(lang/sampa2ipa «parameters»)"
    "slib(lang/swac «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Lang' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-life ; in the menu DONE
  '("slib(life/frcommodity «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Life' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-list ; in the menu DONE
  '("slib(list/selshuf «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `List' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-matrix ; in the menu DONE
  '("slib(matrix/concate «parameters»)"
    "slib(matrix/det «parameters»)"
    "slib(matrix/givenrank «parameters»)"
    "slib(matrix/inverse «parameters»)"
    "slib(matrix/invertible «parameters»)"
    "slib(matrix/itriangular «parameters»)"
    "slib(matrix/non0 «parameters»)"
    "slib(matrix/orthogonal «parameters»)"
    "slib(matrix/random «parameters»)"
    "slib(matrix/trace «parameters»)"
    "slib(matrix/transpose «parameters»)"
    "slib(matrix/triangular «parameters»)"
    "slib(matrix/unimodular «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Matrix' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-media ; in the menu DONE
  '("slib(media/audio «parameters»)"
    "slib(media/dewplayer «parameters»)"
    "slib(media/player «parameters»)"
    "slib(media/player_mp3_multi «parameters»)"
    "slib(media/video «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Media' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-numeration ; in the menu DONE
  '("slib(numeration/babylonien «parameters»)"
    "slib(numeration/basep «parameters»)"
    "slib(numeration/ecriturenombre «parameters»)"
    "slib(numeration/egyptien «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Numeration' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-oef ; in the menu DONE
  '("slib(oef/blank «parameters»)"
    "slib(oef/codelim «parameters»)"
    "slib(oef/codename «parameters»)"
    "slib(oef/env «parameters»)"
    "slib(oef/insfilename «parameters»)"
    "slib(oef/newfile «parameters»)"
    "slib(oef/postsrc «parameters»)"
    "slib(oef/presrc «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `OEF' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-polynomial ; in the menu DONE
  '("slib(polynomial/random «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Polynomial' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-set ; in the menu DONE
  '("slib(set/subset «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Set' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-stat ; in the menu DONE
  '("slib(stat/1d «parameters»)"
    "slib(stat/arithmean «parameters»)"
    "slib(stat/beta «parameters»)"
    "slib(stat/betacdf «parameters»)"
    "slib(stat/betainv «parameters»)"
    "slib(stat/betapdf «parameters»)"
    "slib(stat/binomial «parameters»)"
    "slib(stat/binomialcdf «parameters»)"
    "slib(stat/binomialinv «parameters»)"
    "slib(stat/binomialpdf «parameters»)"
    "slib(stat/cauchy «parameters»)"
    "slib(stat/cauchycdf «parameters»)"
    "slib(stat/cauchyinv «parameters»)"
    "slib(stat/cauchypdf «parameters»)"
    "slib(stat/chi2 «parameters»)"
    "slib(stat/chi2cdf «parameters»)"
    "slib(stat/chi2inv «parameters»)"
    "slib(stat/chi2pdf «parameters»)"
    "slib(stat/correlation «parameters»)"
    "slib(stat/covariance «parameters»)"
    "slib(stat/dataproc «parameters»)"
    "slib(stat/deviation «parameters»)"
    "slib(stat/discretelaw «parameters»)"
    "slib(stat/effectif «parameters»)"
    "slib(stat/empiric «parameters»)"
    "slib(stat/expo «parameters»)"
    "slib(stat/exponential «parameters»)"
    "slib(stat/exponentialcdf «parameters»)"
    "slib(stat/exponentialinv «parameters»)"
    "slib(stat/exponentialpdf «parameters»)"
    "slib(stat/fisher «parameters»)"
    "slib(stat/fishercdf «parameters»)"
    "slib(stat/fisherinv «parameters»)"
    "slib(stat/fisherpdf «parameters»)"
    "slib(stat/freq «parameters»)"
    "slib(stat/gamma «parameters»)"
    "slib(stat/gammacdf «parameters»)"
    "slib(stat/gammainv «parameters»)"
    "slib(stat/gammapdf «parameters»)"
    "slib(stat/geomean «parameters»)"
    "slib(stat/geometric «parameters»)"
    "slib(stat/geometric1 «parameters»)"
    "slib(stat/geometric1cdf «parameters»)"
    "slib(stat/geometric1inv «parameters»)"
    "slib(stat/geometric1pdf «parameters»)"
    "slib(stat/geometriccdf «parameters»)"
    "slib(stat/geometricinv «parameters»)"
    "slib(stat/geometricpdf «parameters»)"
    "slib(stat/harmonic «parameters»)"
    "slib(stat/histo «parameters»)"
    "slib(stat/hypergeometric «parameters»)"
    "slib(stat/hypergeometriccdf «parameters»)"
    "slib(stat/hypergeometricinv «parameters»)"
    "slib(stat/hypergeometricpdf «parameters»)"
    "slib(stat/laplace «parameters»)"
    "slib(stat/laplacecdf «parameters»)"
    "slib(stat/laplaceinv «parameters»)"
    "slib(stat/laplacepdf «parameters»)"
    "slib(stat/linearcong «parameters»)"
    "slib(stat/logistic «parameters»)"
    "slib(stat/logisticcdf «parameters»)"
    "slib(stat/logisticinv «parameters»)"
    "slib(stat/logisticpdf «parameters»)"
    "slib(stat/lognormal «parameters»)"
    "slib(stat/lognormalcdf «parameters»)"
    "slib(stat/lognormalinv «parameters»)"
    "slib(stat/lognormalpdf «parameters»)"
    "slib(stat/median «parameters»)"
    "slib(stat/multinomial «parameters»)"
    "slib(stat/nbin «parameters»)"
    "slib(stat/nbincdf «parameters»)"
    "slib(stat/nbininv «parameters»)"
    "slib(stat/nbinpdf «parameters»)"
    "slib(stat/normal «parameters»)"
    "slib(stat/normalcdf «parameters»)"
    "slib(stat/normalinv «parameters»)"
    "slib(stat/normalpdf «parameters»)"
    "slib(stat/pascal «parameters»)"
    "slib(stat/pascalcdf «parameters»)"
    "slib(stat/pascalinv «parameters»)"
    "slib(stat/pascalpdf «parameters»)"
    "slib(stat/poisson «parameters»)"
    "slib(stat/poissoncdf «parameters»)"
    "slib(stat/poissoninv «parameters»)"
    "slib(stat/poissonpdf «parameters»)"
    "slib(stat/posdiscretelaw «parameters»)"
    "slib(stat/prod «parameters»)"
    "slib(stat/quadratic «parameters»)"
    "slib(stat/random «parameters»)"
    "slib(stat/range «parameters»)"
    "slib(stat/student «parameters»)"
    "slib(stat/studentcdf «parameters»)"
    "slib(stat/studentinv «parameters»)"
    "slib(stat/studentpdf «parameters»)"
    "slib(stat/sum «parameters»)"
    "slib(stat/variance «parameters»)"
    "slib(stat/weibull «parameters»)"
    "slib(stat/weibullcdf «parameters»)"
    "slib(stat/weibullinv «parameters»)"
    "slib(stat/weibullpdf «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Stat' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-text ; in the menu DONE
  '("slib(text/approximation «parameters»)"
    "slib(text/balloon «parameters»)"
    "slib(text/cdecomment «parameters»)"
    "slib(text/comblin «parameters»)"
    "slib(text/crossword «parameters»)"
    "slib(text/cutchoice2 «parameters»)"
    "slib(text/cutchoices «parameters»)"
    "slib(text/markerror «parameters»)"
    "slib(text/markgroup «parameters»)"
    "slib(text/marktext «parameters»)"
    "slib(text/marktextpartial «parameters»)"
    "slib(text/markword «parameters»)"
    "slib(text/matrixhtml «parameters»)"
    "slib(text/matrixinsert «parameters»)"
    "slib(text/matrixtex «parameters»)"
    "slib(text/maximamatrix «parameters»)"
    "slib(text/octavematrix «parameters»)"
    "slib(text/sigunits «parameters»)"
    "slib(text/spirale «parameters»)"
    "slib(text/whitespace «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Text' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-triplerelation ; in the menu DONE
  '("slib(triplerelation/tabular «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Triplerelation' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-utilities ; in the menu DONE
  '("slib(utilities/mathcalc «parameters»)"
    "slib(utilities/nopaste «parameters»)"
    "slib(utilities/notepad «parameters»)"
    "slib(utilities/notepad1.html «parameters»)"
    "slib(utilities/notepad2.html «parameters»)"
    "slib(utilities/notepad3.html «parameters»)"
    "slib(utilities/tooltip «parameters»)"
    "slib(utilities/trigo-calc «parameters»)"
    )
  "Used for highlighting and completion and for a submenu `Utilities' in the menu `Script_Library'."
  )

(defvar oef-definitions-slib-scripts (append oef-definitions-slib-algebra oef-definitions-slib-analysis  oef-definitions-slib-chemistry oef-definitions-slib-circuits oef-definitions-slib-data oef-definitions-slib-draw oef-definitions-slib-function oef-definitions-slib-games oef-definitions-slib-geogebra oef-definitions-slib-graph oef-definitions-slib-graphpaper oef-definitions-slib-lang oef-definitions-slib-life oef-definitions-slib-list oef-definitions-slib-matrix oef-definitions-slib-media oef-definitions-slib-numeration oef-definitions-slib-oef oef-definitions-slib-polynomial oef-definitions-slib-set oef-definitions-slib-stat oef-definitions-slib-text oef-definitions-slib-triplerelation oef-definitions-slib-utilities)
  "Used for highlighting and completion and for completion (`oef-completions') and for a submenu `All' in the menu Script Library.

Automatically build from following lists: `oef-definitions-slib-algebra' `oef-definitions-slib-analysis' `oef-definitions-slib-chemistry' `oef-definitions-slib-circuits' `oef-definitions-slib-data' `oef-definitions-slib-draw' `oef-definitions-slib-function' `oef-definitions-slib-games' `oef-definitions-slib-geogebra' `oef-definitions-slib-graph' `oef-definitions-slib-graphpaper' `oef-definitions-slib-lang' `oef-definitions-slib-life' `oef-definitions-slib-list' `oef-definitions-slib-matrix' `oef-definitions-slib-media' `oef-definitions-slib-numeration' `oef-definitions-slib-oef' `oef-definitions-slib-polynomial' `oef-definitions-slib-set' `oef-definitions-slib-stat' `oef-definitions-slib-text' `oef-definitions-slib-triplerelation' `oef-definitions-slib-utilities'")

(defvar oef-slib-scripts nil
  "`oef-slib-scripts' is used for highlighting.  It is automatically  build from `oef-definitions-slib-scripts' a list of slib script definitions.")

(defvar oef-pari-functions ; in the menu TODO
  '("divrem")
  "Used for highlighting and completion."
  )

(defvar oef-maths-functions ; in the menu TODO
  '("evalue" "solve" "simplify" "diff" "int" "int=" "det" "abs" "sqrt" "binomial" "ceil" "floor" "rint" "e" "erf" "erfc" "Euler" "exp" "factorial" "Inf" "gcd" "lcm" "%" "max" "min" "lg" "lgamma" "ln" "log2" "pow" "sgn" "PI" "sin" "cos" "tg" "tan" "sec" "cot" "cotan" "ctg" "csc" "arccos" "acos" "arcsin" "asin" "arctan" "atan" "arctg" "sh" "sinh" "tanh" "tanh" "th" "ch" "cosh" "coth" "cotanh" "Argch")
  "Used for highlighting and completion."
  )

(defvar oef-random-functions ; in the menu DONE
  '("random" "randint" "shuffle" "randomitem" "randomrow")
  "Used for highlighting and completion.")

(defvar oef-draw-commands ; in the menu DONE
  '("affine" "angle" "animate" "arc" "arrow" "arrow2" "arrowhead" "arrows" "arrows2" "audio" "axis" "axisnumbering" "barchart" "bezier" "bgcolor" "bgimage" "blink" "boxplot" "boxplotdata" "canvasdraw" "canvastype" "centerstring" "circle" "circles" "clearbutton" "clock" "copy" "copyresized" "crosshair" "crosshairs" "crosshairsize" "cursor" "curve" "dashed" "dashtype" "delete" "demiline" "demilines" "diamondfill" "disk" "disks" "disks" "display" "dotfill" "drag" "darrow" "dhline" "dline" "dlines" "dvline" "ellipse" "erase" "fcircle" "fcircles" "fill" "fillcolor" "filled" "filltoborder" "floodfill" "fontcolor" "fontfamily" "fontsize" "fpoly" "frect" "frects" "froundrect" "froundrects" "fsquare" "ftriangle" "ftriangles" "functionlabel" "grid" "gridfill" "halfline" "halflines" "hatchfill" "highlight" "hline" "hlines" "horizontalline" "horizontallines" "html" "http" "imagefill" "input" "inputstyle" "intooltip" "jscurve" "jsmath" "jsplot" "killaffine" "killrotate" "killslider" "killtranslate" "killtranslation" "lattice" "legend" "legendcolors" "levelcurve" "line" "linegraph" "lines" "linewidth" "mathml" "mouse" "mouse_degree" "mousex" "mousey" "multidash" "multidraw" "multifill" "multifillcolors" "multifillopacity" "multilabel" "multilinewidth" "multisnaptogrid" "multistrokecolors" "multistrokeopacity" "multiuserinput" "noaxis" "noayis" "note:" "onclick" "opacity" "parallel" "piechart" "pixels" "pixelsize" "plot" "plotsteps" "point" "pointer" "points" "poly" "polyline" "popup" "precision" "protractor" "ranget" "rangex" "rangey" "rays" "rect" "rects" "replyformat" "rotate" "rotationcenter" "roundrect" "roundrects" "ruler" "seg" "segment" "segments" "segs" "setlimits" "setpixel" "sgraph" "size" "slider" "sliderfunction_x" "sliderfunction_y" "snaptofun" "snaptofunction" "snaptogrid" "snaptopoints" "square" "status" "string" "stringup" "strokecolor" "text" "textarea" "textup" "trace_jscurve" "trange" "translate" "translation" "transparent" "triangle" "triangles" "userboxplot" "userboxplotdata" "userdraw" "userinput" "userinput_function" "userinput_textarea" "userinput_xy" "usertextarea_xy" "vector" "vectors" "verticalline" "verticallines" "video" "vline" "vlines" "xaxis" "xaxistext" "xaxistextup" "xaxisup" "xerrorbars" "xlabel" "xlogbase" "xlogscale" "xrange" "xsnaptogrid" "xunit" "xylogscale" "yaxis" "yerrorbars" "ylabel" "ylogbase" "ylogscale" "yrange" "ysnaptogrid" "yunit" "zoom")
  "Used for highlighting and completion.")

(defvar oef-example-files
  nil
  "List of the oef examples files.  This variable is automatically set at Emacs launch.")

(defvar oef-list-commands
  nil
  "List of commands returned by the function `oef-get-list-commands-names'.")

(defvar oef-wims-session nil
  "Active Wims Session in unice wims server."
  )

(defvar oef-highlighted-variable nil
  "Keep the name of the highlighted variable.  Used by the function `oef-highlight-variable'."
  )

(defvar oef-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?< "_" table)  ;Symbol constituent
    (modify-syntax-entry ?> "_" table)  ;Symbol constituent
    table)
  "Syntax table used while in oef mode.")
;; Without removing <> as SGML matching parenthesis from the syntax table
;; oef-mode is not mattching parenthesis well when there is a comparison.
;; the rest of the code work better wit sgml-make-syntax-table

;; idea to test : with-syntax-table temporarily alters the current syntax table of whichever buffer
;; is current at the time the macro execution starts.
;; When the syntax table is not flexible enough to specify the syntax of a language, you can
;; override the syntax table for specific character occurrences in the buffer, by applying a
;; syntax-table text property.

;;---- DEFUNS ------------------------------------------------------------------

(defun oef-hl-on ()
  "The user wants to highlight the variable at point.  
Put the name of the variable at point in `oef-highlighted-variable'.  
Used by the function `oef-highlight-variable'.  
This function call `oef-add-variable-as-keyword-for-completion'."
  (setq oef-highlighted-variable (word-at-point)) ; store word at point as the variable name
  (highlight-regexp  (concat "\\({" oef-highlighted-variable "\\b\\|\\b" oef-highlighted-variable "\\b\\|\\\\" oef-highlighted-variable "\\b\\)")) ; highlight the variable
  (oef-add-variable-as-keyword-for-completion)
  (message (concat "Highlight OEF variable " oef-highlighted-variable)) ; prompt a message
  )

(defun oef-hl-off ()
  "The user wants to unhighlight the variable at point.  Set `oef-highlighted-variable' to nil.  Used by the function `oef-highlight-variable'."
  (message (concat "Unhighlight OEF variable " oef-highlighted-variable))
  (unhighlight-regexp (concat "\\({" oef-highlighted-variable "\\b\\|\\b" oef-highlighted-variable "\\b\\|\\\\" oef-highlighted-variable "\\b\\)"))
  (setq oef-highlighted-variable nil)
  )

(defun oef-add-variable-as-keyword-for-completion ()
  "Add the variable in `oef-completions' for completion with oef-mode-backend."
  (interactive)
  (add-to-list 'oef-completions (substring-no-properties oef-highlighted-variable))
  (oef-make-candidats))


(defun oef-highlight-variable ()
  "Highlight a variable (with the function `oef-hl-on') or unhighlight an highlighted variable (with the function `oef-hl-off')."
  (interactive)
  ;; is an oef-variable highlighted in the buffer?
  (if oef-highlighted-variable
      ;; a VARIABLE is HIGHLIGHTED
      ;; then we have to find out: is the point on a variable definition ?
      (if  (or
	    ;; is the point on an  oef-VARIABLE DEFINITION (exercise format) ?
	    (looking-back "\\\\\\(real\\|complex\\|text\\|integer\\|rational\\|function\\|matrix\\){\\w*" (line-beginning-position))
	    ;; or is the point on an  oef-VARIABLE DEFINITION (document format) ?
	    (looking-back "\\\\def{\\(real\\|complex\\|text\\|integer\\|rational\\|function\\|matrix\\) *\\w*" (line-beginning-position))
	    )
	  ;; we have to check if the variable is the same
	  (if (string-equal (word-at-point) oef-highlighted-variable)
	      ;; THE VARIABLE IS THE SAME: the user wants to unhighlight the variable
	      (oef-hl-off)
	    ;; THE VARIABLE IS DIFFERENT: the user wants to highlight a new variable
	    (progn
	      (oef-hl-off) ; old variable
	      (oef-hl-on)  ; new variable
	      )
	    )
	;; else the point is  NOT on an oef-VARIABLE DEFINITION (exercise format)
	;; perhaps the point is on an oef-command `\commandName{' or oef-variable `\variableName'
	(if  (looking-back "\\\\[[:alpha:]]+[[:alnum:]]*" (line-beginning-position))
	    ;;  yes the point is on an oef-command `\commandName{' or oef-variable `\variableName'
	    ;; then we have to find-out if the point is on a command
	    (if (looking-at "[[:alnum:]]*{")
		;; IT'S AN OEF-COMMAND not an oef-variable
		(oef-hl-off)
	      ;; IT'S AN OEF-VARIABLE
	      ;; we have to check if the variable is the same
	      (if (string-equal (word-at-point) oef-highlighted-variable)
		  ;; THE VARIABLE IS THE SAME: the user wants to unhighlight the variable
		  (oef-hl-off)
		;; THE VARIABLE IS DIFFERENT: the user wants to highlight a new variable
		(progn
		  (oef-hl-off) ; old variable
		  (oef-hl-on)  ; new variable
		  )
		)
	      )
	  ;;  no the point is neither on an oef-command `\commandName{' nor in an oef-variable `\variableName'
	  (oef-hl-off)
	  )
	); end if
    ;; else HIGHLIGHTING is OFF
    ;; then we have to find out: is the point on a variable definition ?
    (if (or
	 ;; is the point on an  oef-VARIABLE DEFINITION (exercise format) ?
	 (looking-back "\\\\\\(real\\|complex\\|text\\|integer\\|rational\\|function\\|matrix\\){\\w*" (line-beginning-position))
	 ;; or is the point on an  oef-VARIABLE DEFINITION (document format) ?
	 (looking-back "\\\\def{\\(real\\|complex\\|text\\|integer\\|rational\\|function\\|matrix\\) *\\w*" (line-beginning-position))
	 )
	(oef-hl-on)
      ;; else the point is  NOT on an oef-VARIABLE DEFINITION (exercise format)
      ;; perhaps the point is on an oef-command `\commandName{' or oef-variable `\variableName'
      (if  (looking-back "\\\\[[:alpha:]]+[[:alnum:]]*" (line-beginning-position))
	  ;;  yes the point is on an oef-command `\commandName{' or oef-variable `\variableName'
	  ;; then we have to find-out if the point is on a command
	  (if (looking-at "[[:alnum:]]*{")
	      ;; IT'S AN OEF-COMMAND not an oef-variable
	      nil ; nothing to do (we keep the variables unlighted)
	    ;; IT'S AN OEF-VARIABLE
	    (oef-hl-on)
	    )
	)
      );end if
    );end if
  )

(defun oef-copy-all-or-region ()
  "Put the whole buffer content to `kill-ring', or text selection if there's one."
  (interactive)
  (if (featurep 'aquamacs)
      ;; aquamacs cop all or region
      (if (use-region-p)
	  (progn
	    (clipboard-kill-ring-save-active-region (region-beginning)(region-end))
	    (message "Text selection copied."))
	(progn
	  (mark-whole-buffer)
	  (clipboard-kill-ring-save (point-min)(point-max))
	  (message "Buffer content copied.")))
    ;; emacs copy all or region
    (if (use-region-p)
	(progn
	  (kill-new (buffer-substring (region-beginning) (region-end)))
	  (message "Text selection copied."))
      (progn
	(kill-new (buffer-string))
	(message "Buffer content copied.")))))

(defun oef-get-wims-session ()
  "Extract the wims session if there's a URL from a wims session on the clipboard."
  (interactive)
  (let ((link (substring-no-properties (gui-get-selection 'CLIPBOARD))))
    (save-match-data
      (if  (and (string-match "https?://\\([^/]+\\)[^=]+=\\([^.]+\\).\\([^&]+\\)&\\+lang=\\([^&]+\\).+" link)
                (setq oef-wims-server (match-string 1 link)
                      oef-wims-session (match-string 2 link)
                      oef-wims-call (match-string 3 link)
                      oef-wims-lang (match-string 4 link)))
          (message (concat "Connected to Wims Session : " oef-wims-session " on server : " oef-wims-server))
        (error "No wims URL with session on the clipboard")))))

(defun oef-edit-in-browser()
  "Edit file in browser. There are two kind of files: exercises and documents. If the file contain 'statement{' it will be considered as an exercise otherwise as a document."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\\statement{" nil t)
	(oef-edit-exercise-in-browser)
      (oef-edit-document-in-browser))))

(defun oef-edit-exercise-in-browser()
  "Edit exercise in browser."
  (interactive)
  (if oef-wims-session
      (progn
        (setq oef-wims-call (number-to-string (+ 1 (string-to-number oef-wims-call))))
	(oef-copy-all-or-region)
	(let ((oef-filename (file-name-nondirectory (buffer-file-name))))
	  (browse-url  (concat "http://" oef-wims-server "/wims/wims.cgi?session=" oef-wims-session "." oef-wims-call "&+lang=" oef-wims-lang "&+module=adm%2Fmodtool&+cmd=reply&+jobreq=edfile&+fname=src%2F" oef-filename))))
    (message-box "You are not connected. You have to connect to a wims session first.")))

(defun oef-edit-document-in-browser()
  "Edit document in browser."
  (interactive)
  (if oef-wims-session
      (progn
        (setq oef-wims-call (number-to-string (+ 1 (string-to-number oef-wims-call))))
	(oef-copy-all-or-region)
	(let ((oef-filename (file-name-nondirectory (buffer-file-name))))
	  (browse-url (replace-regexp-in-string ".oef" "" (concat "http://" oef-wims-server "/wims/wims.cgi?session=" oef-wims-session  "." oef-wims-call "&+lang=" oef-wims-lang "&+module=adm%2Fdoc&+cmd=reply&+job=edit&+doc=1&+block=" oef-filename)))))
    (message-box "You are not connected. You have to connect to a wims session first.\nIn your browser :\n- Connect to Modtool\n- Go to the main page of your document\n- Select and copy the url in the clipboard\n\nIn emacs :\n- Connect to a wims session")))

(defun oef-goto-answers()
  "Goto answers"
  (interactive)
  (goto-char (point-min))
  (search-forward "\\answer{" nil t)
  (beginning-of-line)
  (recenter)
  )

(defun oef-goto-css()
  "Goto CSS"
  (interactive)
  (goto-char (point-min))
  (search-forward "\\css{" nil t)
  (beginning-of-line)
  (recenter)
  )

;; (defun oef-goto-reply()
;;   "From an answer go to the correspondant reply.  From a reply go to the next reply"
;;   (interactive)
;;   (setq oef-grabed-word-for-goto (word-at-point))
;; 					;  (message-box oef-grabed-word-for-goto)
;;   (if (> (length oef-grabed-word-for-goto) 5)
;;       (if (string= oef-grabed-word-for-goto "answer")
;;   	  (progn
;;   	    (beginning-of-sexp)
;;   	    (if (= (char-before) 92) ;char antislash
;;   		(progn
;;   		  (setq oef-answers-index 1)
;;   		  (while (if (search-backward "\\answer{" nil t )
;;   			     (1+ oef-answers-index)
;;   			   ))
;;   		  (message-box "%s" oef-answers-index)
;;   		  )
;;   	      )
;;   	    )
;; 	(progn
;; 	  (search-forward "\\embed{reply" nil t)
;; ;	  (recenter-top-bottom)
;; 	  )
;;   	)
;;     (progn
;;       (beginning-of-buffer)
;;      (search-forward "\\embed{reply" nil t)
;;       (recenter-top-bottom)
;;       )
;;     )
;;   )


(defun oef-goto-reply()
  "From an answer go to the correspondant reply.  From a reply go to the next reply"
  (interactive)
  (setq oef-grabed-word-for-goto (word-at-point))
  (if oef-grabed-word-for-goto
      (progn
	;;	(message-box oef-grabed-word-for-goto)
	(when (string-match-p "reply[0-9]+" (substring-no-properties oef-grabed-word-for-goto))
	  (search-forward "\\embed{reply" nil t)
	  (recenter))
	(beginning-of-line)
	(forward-char 1)
	(if (string= (word-at-point) "answer")
	    (progn
	      (search-backward "\\embed{reply" nil t)
	      (recenter))
	  (progn
	    (beginning-of-buffer)
	    (search-forward "\\embed{reply" nil t)
	    (recenter))))
    (progn
      (beginning-of-buffer)
      (search-forward "\\embed{reply" nil t)
      (recenter))
    )
  )

(defun oef-goto-statement()
  "Goto Statement"
  (interactive)
  (goto-char (point-min))
  (search-forward "\\statement{" nil t)
  (recenter)
  )

(defun oef-select-parameter ()
  "Select the first «parameter» from the beginning of line."
  (interactive)
  (move-beginning-of-line nil)
  (re-search-forward "«")
  (backward-char nil)
  (set-mark-command nil)
  (re-search-forward "»")
  )

(defun oef-insert-math()
  "This function insert a mathematical expression between '\(' and '\)'"
  (interactive  (if (use-region-p)
		    (progn
		      (setq start (region-beginning))
		      (setq end (region-end))
		      (message (string end))
		      (goto-char start)
		      (insert "\\(")
		      (goto-char (+ 2 end))
		      (insert "\\)"))
		  (progn
		    (insert "\\(\\)")
		    (backward-char 2)
		    ))))

(defun oef-insert-tag-b()
  "This function insert the tag <b>"
  (interactive  (if (use-region-p)
		    (progn
		      (setq start (region-beginning))
		      (setq end (region-end))
		      (message (string end))
		      (goto-char start)
		      (insert "<b>")
		      (goto-char (+ 3 end))
		      (insert "</b>"))
		  (progn
		    (insert "<b></b>")
		    (backward-char 4)
		    ))))

(defun oef-insert-tag-code()
  "This function insert the tag <code>"
  (interactive  (if (use-region-p)
		    (progn
		      (setq start (region-beginning))
		      (setq end (region-end))
		      (message (string end))
		      (goto-char start)
		      (insert "<code>")
		      (goto-char (+ 6 end))
		      (insert "</code>"))
		  (progn
		    (insert "<code></code>")
		    (backward-char 7)
		    ))))

(defun oef-insert-tag-mark()
  "This function insert the tag <mark>"
  (interactive  (if (use-region-p)
		    (progn
		      (setq start (region-beginning))
		      (setq end (region-end))
		      (message (string end))
		      (goto-char start)
		      (insert "<mark>")
		      (goto-char (+ 6 end))
		      (insert "</mark>"))
		  (progn
		    (insert "<mark></mark>")
		    (backward-char 7)
		    ))))

(defun oef-insert-tag-sub()
  "This function insert the tag <sub>"
  (interactive  (if (use-region-p)
		    (progn
		      (setq start (region-beginning))
		      (setq end (region-end))
		      (message (string end))
		      (goto-char start)
		      (insert "<sub>")
		      (goto-char (+ 5 end))
		      (insert "</sub>"))
		  (progn
		    (insert "<sub></sub>")
		    (backward-char 6)
		    ))))

(defun oef-insert-tag-sup()
  "This function insert the tag <sup>"
  (interactive  (if (use-region-p)
		    (progn
		      (setq start (region-beginning))
		      (setq end (region-end))
		      (message (string end))
		      (goto-char start)
		      (insert "<sup>")
		      (goto-char (+ 5 end))
		      (insert "</sup>"))
		  (progn
		    (insert "<sup></sup>")
		    (backward-char 6)
		    ))))

(defun oef-new-exercise()
  "This fonction insert minimal blueprint for a new exercise"
  (interactive)
  (insert "\\title{«Exercise Title»}")
  (newline)
  (insert "\\author{")(insert oef-author)(insert "}")
  (newline)
  (insert "\\email{")(insert oef-email)(insert "}")
  (newline)
  (insert "\\language{")(insert oef-language)(insert "}")
  (newline)
  (insert "\\range{-5..5}")
  (newline)
  (insert "\\computeanswer{ no }")
  (newline)
  (insert "<!-- l utilisateur doit lui-même faire les calculs et entrer la valeur finale -->")
  (newline)
  (newline)
  (insert "<!-- VARIABLES -->")
  (newline)
  (newline)
  (insert "\\statement{«exercise statement / énoncé de l'exercice»}")
  )

(defun oef-variable-complex()
  "This function insert initialization for a variable of complex number type in exercise"
  (interactive)
  (insert "\\complex{=}")
  (backward-char 2))

(defun oef-variable-function()
  "This function insert initialization for a variable of function type in exercise"
  (interactive)
  (insert "\\function{=}")
  (backward-char 2))

(defun oef-variable-integer()
  "This function insert initialization for a variable of integer number type in exercise"
  (interactive)
  (insert "\\integer{=}")
  (backward-char 2))

(defun oef-variable-matrix()
  "This function insert initialization for a variable of matrix type in exercise"
  (interactive)
  (insert "\\matrix{=}")
  (backward-char 2))

(defun oef-variable-rational()
  "This function insert initialization for a variable of rational number type (quotient) in exercise"
  (interactive)
  (insert "\\rational{=}")
  (backward-char 2))

(defun oef-variable-real()
  "This function insert initialization for a variable of real number type in exercise"
  (interactive)
  (insert "\\real{=}")
  (backward-char 2))

(defun oef-variable-text()
  "This function insert initialization for a variable of text type in exercise"
  (interactive)
  (insert "\\text{=}")
  (backward-char 2))

(defun oef-insert-embed-reply()
  "This fonction insert embed answer in statement"
  (interactive)
  (insert "\\embed{reply}")
  (backward-char 1)
  )

(defun oef-insert-statement()
  "this function instert the statement blueprint"
  (interactive)
  (insert "\\statement{«exercise statement / énoncé de l'exercice»}")
  )

(defun oef-insert-answer()
  "this function instert an answer blueprint"
  (interactive)
  (insert "\\answer{«Description»}{«good answer or good answer indice»;«propositions»}{type=}{option=}")
  )

(defun oef-insert-electron()
  "This function insert the full isotope symbol for the electron"
  (interactive)
  (insert "{}_{-1}^{\\phantom{-}0}\\mathrm{e}")
  )

(defun oef-insert-proton()
  "This function insert the full isotope symbol for the electron"
  (interactive)
  (insert "{}_{1}^{1}\\mathrm{p}")
  )

(defun oef-insert-neutron()
  "This function insert the full isotope symbol for the neutron"
  (interactive)
  (insert "{}_{0}^{1}\\mathrm{n}")
  )

(defun oef-insert-positron()
  "This function insert the full isotope symbol for the positron"
  (interactive)
  (insert "{}_{1}^{0}\\mathrm{e}")
  )

(defun oef-insert-alpha-particle()
  "This function insert the full isotope symbol for the alpha particule"
  (interactive)
  (insert "{}_{2}^{4}\\mathrm{He}")
  )

(defun oef-insert-isotope()
  "This function insert the full isotope symbol"
  (interactive)
  (insert "{}_{Z}^{A}\\mathrm{X}")
  )
;; voir le code de oef-insert-flash pour saisir A Z et X.

(defun oef-insert-uranium()
  "This function insert the full isotope symbol"
  (interactive)
  (insert "{}_{\\phantom{2}92}^{238}\\mathrm{U}")
  )

(defun oef-insert-carbon()
  "This function insert the full isotope symbol"
  (interactive)
  (insert "{}_{\\phantom{1}6}^{14}\\mathrm{C}")
  )

(defun oef-canvasdraw-example()
  "Insert a canvasdraw example"
  (interactive)
  (insert "\\canvasdraw{400,400}{\nxrange -10,10\nyrange -10,10\nmathml 0,0,0,0,\\M\nstrokecolor blue\nmathml -5,5,0,0,\\N\nopacity 255,30\nfcircle 0,0,130,green\nfrect 0,0,5,-5,orange\n}")
  )

(defun oef-flydraw-commands()
  "Browse Flydraw commands in emacs (english version)"
  (interactive)
  (add-hook 'eww-after-render-hook #'oef-flydraw-commands-highlight)  ;oef-flydraw-commands-highlight is called after EWW is done rendering because eww is asynchronous
  (eww "http://pwet.fr/man/linux/commandes/flydraw/")
  )

(defun oef-flydraw-commands-highlight()
  "Called after EWW is done rendering because eww is asynchronous."
  (visual-line-mode)
  (highlight-regexp "^:\\w*")
  (remove-hook 'eww-after-render-hook #'oef-flydraw-commands-highlight)
  )

(defun oef-flydraw-commands-fr()
  "Browse Flydraw commands in emacs (french version)"
  (interactive)
  (add-hook 'eww-after-render-hook #'oef-flydraw-commands-latin-1)
  (eww "https://subversion.renater.fr/wimsdev/trunk/wims/src/Flydraw/commands.fr")
  )

(defun oef-flydraw-commands-latin-1()
  "Render french documentation in latin-1. Called after EWW is done rendering in UTF8 because eww is asynchronous."
  (remove-hook 'eww-after-render-hook #'oef-flydraw-commands-latin-1)
  (add-hook 'eww-after-render-hook #'oef-flydraw-commands-highlight)
  (eww-set-character-encoding 'latin-1)
  )

(defun oef-get-examples ()
  "This function create a submenu `Examples' with oef examples."
  (easy-menu-create-menu
   "Examples"
   (mapcar                   ; (`mapcar' `function' `sequence') `mapcar' applies `function' to each element of `sequence', and returns a list of the results.
    (lambda                  ; here start the fuction: a lambda expression (witch is an anonymous function object). The first element of a lambda expression is always the symbol lambda.
      (x)                    ; The second element is a list of symbols—the argument variable names. This is called the lambda list.
					; The next element could be The documentation string
					; The next element could be (interactive code-string). This declares how to provide arguments if the function is used interactively.Functions with this declaration are called commands; they can be called using M-x or bound to a key.
					; The rest of the elements are the body of the function: the Lisp code to do the work of the function. The value returned by the function is the value returned by the last element of the body: The rest of the elements in MENU are menu items. A menu item can be a vector of three elements:  [NAME CALLBACK ENABLE]
      (vector (file-name-nondirectory x) ;NAME
              `(lambda () (interactive) (find-file-read-only ,x) ; CALLBACK
                 t) ;ENABLE
              ) ; read-only
      )               ; end of the lamda expression
    oef-example-files ; sequence : here a list of strings (the oef examples files)
    ) ; end of mapcar
   )) ; end of defun oef-get-examples

(defun oef-get-menu-commands ()
  "This function create a submenu `Commands' with ‘oef-commands’ without parameters from commands definitions with parameters in `oef-definitions-commands'."
  (easy-menu-create-menu
   "Commands"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "{\\(.\\|\n\\)*}" "" x) ; each command name in the submenu
              `(lambda () (interactive)
                 (insert  (concat "\\" ,x))
                 t))
      )               ; end of the lamda expression
    oef-definitions-commands ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun oef-get-menu-commands

(defun oef-get-menu-special-commands ()
  "This function create a submenu `Special' with ‘oef-special-commands’."
  (easy-menu-create-menu
   "Special"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "special{" "" (replace-regexp-in-string " «parameters»}" "" x)) ; each special-command name in the submenu
              `(lambda () (interactive)
                 (insert  (concat "\\" ,x))
                 t))
      )               ; end of the lamda expression
    oef-definitions-special-commands ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun oef-get-menu-special-commands

(defun oef-get-wims-functions ()
  "This function create a submenu `Wims_Functions' with ‘oef-wims-functions’."
  (easy-menu-create-menu
   "Wims Functions"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "wims(" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each wims function name in the submenu
              `(lambda () (interactive)
                 (insert   ,x)
                 t))
      )               ; end of the lamda expression
    oef-definitions-wims-functions ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun oef-get-wims-functions

(defun oef-get-slib-scripts ()
  "This function create a submenu `All' for all the Scripts from ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "All"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-scripts ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-scripts

(defun oef-get-slib-algebra ()
  "This function create a submenu `Algebra' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Algebra"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(algebra/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-algebra ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-algebra

(defun oef-get-slib-analysis ()
  "This function create a submenu `Analysis' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Analysis"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(analysis/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-analysis ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-analysis

(defun oef-get-slib-chemistry ()
  "This function create a submenu `Chemistry' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Chemistry"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(chemistry/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-chemistry ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-chemistry

(defun oef-get-slib-circuits ()
  "This function create a submenu `Circuits' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Circuits"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(circuits/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-circuits ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-circuits

(defun oef-get-slib-data ()
  "This function create a submenu `Data' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Data"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(data/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-data ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-data

(defun oef-get-slib-draw ()
  "This function create a submenu `Draw' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Draw"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(draw/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-draw ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-draw

(defun oef-get-slib-function ()
  "This function create a submenu `Function' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Function"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(function/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-function ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-function

(defun oef-get-slib-games ()
  "This function create a submenu `Games' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Games"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(games/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-games ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-games

(defun oef-get-slib-geogebra ()
  "This function create a submenu `Geogebra' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Geogebra"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-geogebra ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-geogebra

(defun oef-get-slib-graph ()
  "This function create a submenu `Graph' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Graph"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(graph/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-graph ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-graph

(defun oef-get-slib-graphpaper ()
  "This function create a submenu `Graphpaper' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Graphpaper"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(graphpaper/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-graphpaper ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-graphpaper

(defun oef-get-slib-lang ()
  "This function create a submenu `Lang' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Lang"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(lang/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-lang ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-lang

(defun oef-get-slib-life ()
  "This function create a submenu `Life' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Life"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(life/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-life ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-life

(defun oef-get-slib-list ()
  "This function create a submenu `List' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "List"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(list/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-list ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-list

(defun oef-get-slib-matrix ()
  "This function create a submenu `Matrix' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Matrix"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(matrix/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-matrix ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-matrix

(defun oef-get-slib-media ()
  "This function create a submenu `Media' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Media"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(media/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-media ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-media

(defun oef-get-slib-numeration ()
  "This function create a submenu `Numeration' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Numeration"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(numeration/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-numeration ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-numeration

(defun oef-get-slib-oef ()
  "This function create a submenu `OEF' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "OEF"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(oef/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-oef ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-oef

(defun oef-get-slib-polynomial ()
  "This function create a submenu `Polynomial' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Polynomial"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(polynomial/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-polynomial ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-polynomial

(defun oef-get-slib-set ()
  "This function create a submenu `Set' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Set"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(set/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-set ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-set

(defun oef-get-slib-stat ()
  "This function create a submenu `Stat' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Stat"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(stat/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-stat ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-stat

(defun oef-get-slib-text ()
  "This function create a submenu `Text' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Text"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(text/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lamda expression
    oef-definitions-slib-text ; sequence : here a list of string
    ) ; end of mapcar
   )
  ) ; end of defun oef-get-slib-text

(defun oef-get-slib-triplerelation ()
  "This function create a submenu `Triplerelation' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Triplerelation"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(triplerelation/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lambda expression
    oef-definitions-slib-triplerelation ; sequence : here a list of string
    ) ; end of triplerelation
   )
  ) ; end of defun oef-get-slib-triplerelation

(defun oef-get-slib-utilities ()
  "This function create a submenu `Utilities' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Utilities"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(utilities/" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
	      `(lambda () (interactive)
		 (insert   ,x)
		 t))
      )               ; end of the lambda expression
    oef-definitions-slib-utilities ; sequence : here a list of string
    ) ; end of triplerelation
   )
  ) ; end of defun oef-get-slib-utilities

(defun oef-get-answers-options ()
  "This function create a submenu `Answers_Types_and_Options' with the types and options of an answer from `oef-answers-options'."
  (easy-menu-create-menu
   "Answers Types and Options"
   (mapcar
    (lambda (x);             
      (vector  x ; each type or option in the submenu
	       `(lambda () (interactive)
		  (insert (replace-regexp-in-string "   " "" (replace-regexp-in-string "* " "" ,x)))
		  t))
      )               ; end of the lamda expression
    oef-menu-answers-options ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun oef-get-answers-options

(defun oef-get-exo-init-types ()
  "This function create a submenu `Exercise' for variables initialization in an exercise." 
  (easy-menu-create-menu
   "Exercise"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "{.*=.*}" "" x) ; each command name in the submenu
              `(lambda () (interactive)
		 (progn
		   (insert  (concat "\\" ,x))
		   (forward-char -2))
		 t))
      )               ; end of the lamda expression
    oef-menu-exo-init-types ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun oef-get-exo-init-types

(defun oef-get-doc-init-types ()
  "This function create a submenu `Document' for variables initialization in an document."
  (easy-menu-create-menu
   "Document"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "Def{" "" (replace-regexp-in-string " =}" "" x)) ; each type name in the submenu
              `(lambda () (interactive)
		 (progn
		   (insert  (concat "\\" ,x))
		   (forward-char -2))
                 t))
      )               ; end of the lamda expression
    oef-menu-doc-init-types ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun oef-get-doc-init-types

(defun oef-get-defined-variables ()
  "This function create a submenu `Defined_Variables' for `oef-defined-variables'."
  (easy-menu-create-menu
   "Defined Variables"
   (mapcar
    (lambda (x);             
      (vector  x ; each type name in the submenu
	       `(lambda () (interactive)
		  (insert  ,x)
		  t))
      )               ; end of the lamda expression
    oef-defined-variables ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun oef-get-defined-variables

(defun oef-get-comparison-operators ()
  "This function create a submenu `Comparison' for `oef-comparison-operators'."
  (easy-menu-create-menu
   "Comparisons"
   (mapcar
    (lambda (x);             
      (vector  x ; each type name in the submenu
	       `(lambda () (interactive)
		  (insert  ,x)
		  t))
      )               ; end of the lamda expression
    oef-comparison-operators ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun oef-get-comparison-operators

(defun oef-get-language-reserved-words ()
  "This function create a submenu `Reserved_Word' for `oef-language-reserved-words'."
  (easy-menu-create-menu
   "Reserved Words"
   (mapcar
    (lambda (x);             
      (vector  x ; each type name in the submenu
	       `(lambda () (interactive)
		  (insert  ,x)
		  t))
      )               ; end of the lamda expression
    oef-language-reserved-words ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun oef-get-language-reserved-words

(defun oef-prompt-wims-session()
  "Prompt the wims session in the submenu Wims Session"
  (easy-menu-create-menu
   "Wims Session"
   (vector oef-wims-session nil t)
   )) ; end of defun oef-prompt-wims-session

;; (defun get-my-oef-files () ;; deactivated because it's too slow with a lot of files
;;  "This function create a submenu with my oef files"
;;   (easy-menu-create-menu
;;    "My Files"
;;    (mapcar                   ; (mapcar function sequence) mapcar applies function to each element of sequence, and returns a list of the results.
;;     (lambda                  ; here start the fuction: a lambda expression (witch is an anonymous function object). The first element of a lambda expression is always the symbol lambda.
;;       (x)                    ; The second element is a list of symbols—the argument variable names. This is called the lambda list.
;;                                         ; The next element could be The documentation string
;;                                         ; The next element could be (interactive code-string). This declares how to provide arguments if the function is used interactively.Functions with this declaration are called commands; they can be called using M-x or bound to a key.
;;                                         ; The rest of the elements are the body of the function: the Lisp code to do the work of the function. The value returned by the function is the value returned by the last element of the body:
;;       (vector (file-name-nondirectory x)
;;               `(lambda () (interactive) (find-file ,x) t))
;;       )               ; end of the lamda expression
;;     (directory-files-recursively "~/Documents/" ".oef$") ; sequence : here a list of strings (my .oef files)
;;     ) ; end of mapcar
;;    )) ; end of defun get-my-files

(defun oef-get-list-commands-names (list-commands-definitions)
  "This function takes a LIST-COMMANDS-DEFINITIONS  (for example  `oef-definitions-commands') and return a list of commands names (for example `oef-commands')."
  (setq oef-list-commands '())
  (dolist
      (command-definition list-commands-definitions)
    (add-to-list
     'oef-list-commands
     (replace-regexp-in-string "{\\(.\\|\n\\)+}" "" command-definition)
     )
    )
  (nreverse oef-list-commands)
  )

(defun oef-get-list-wims-functions (list-functions-definitions)
  "This function is used with `oef-definitions-wims-functions' and will return  `oef-wims-functions'."
  (setq list-functions '())
  (dolist
      (function-definition list-functions-definitions)
    (add-to-list
     'list-functions
     (replace-regexp-in-string "wims(" "" (replace-regexp-in-string " «parameters»)" "" function-definition))
     )
    )
  (nreverse list-functions)
  ) ; end oef-get-list-wims-functions

(defun oef-get-list-slib-scripts (list-functions-definitions)
  "This function is used with  `oef-definitions-slib-scripts') and will return  `oef-slib-scripts'."
  (setq list-functions '())
  (dolist
      (function-definition list-functions-definitions)
    (add-to-list
     'list-functions
     (replace-regexp-in-string "slib(" "" (replace-regexp-in-string " «parameters»)" "" function-definition))
     )
    )
  (nreverse list-functions)
  ) ; end oef-get-list-slib-scripts

(defun oef-get-list-answers-options (list-options-definitions)
  "This function takes a LIST-OPTIONS-DEFINITIONS  and return a list of options to be inserted and highlighted."
  (setq list-options '())
  (dolist
      (option-definition list-options-definitions)
    (add-to-list
     'list-options
     (replace-regexp-in-string "   " "" (replace-regexp-in-string "* " "" option-definition))
     )
    )
  (nreverse list-options)
  )

(defun oef-update-menu ()
  "This function update the oef-menu."
  (easy-menu-add-item oef-menu-bar
		      '("Wims Session")
		      [(if oef-wims-session (concat "Connected to : " oef-wims-session) "Not connected") nil :help "Actual reference to a  Wims Session ."])
  ;; (easy-menu-add-item oef-menu-bar '("Files") (get-my-oef-files)) ; ; ;; desactivate because slowdown aquamacs
  )

(defun oef-mode-open-all ()
  "Opens all files found in the list `oef-example-files' in read-only buffers.
You can add more examples in the examples folder in your `user-emacs-directory'"
  (interactive)
  (dolist (oef-example-file oef-example-files)
    (find-file-read-only oef-example-file))
  (ido-switch-buffer)
  )

(defun oef-find-main ()
  "This function find or create the main file."
  (interactive)
  (find-file "main.oef")
  )

(defun oef-find-block ()
  "This function find or create other block files."
  (interactive)
  (find-file (read-file-name "Enter block name: ")))

(defun oef-insert-image-in-document ()
  "This function insert an image.  The image has first to be uploaded in doc/files."
  (interactive)
  (insert "<img src=\"\\filedir/«file name»\" class=\"«class name»\" width=\"«width»\" height=\"«height»\" alt=\"«alternate text»\"/>")
  )

(defun oef-insert-image-in-exercise ()
  "This function insert an image.  The image has first to be uploaded in images."
  (interactive)
  (insert "<img src=\"\\imagedir/«file name»\" class=\"«class name»\" width=\"«width»\" height=\"«height»\" alt=\"«alternate text»\"/>")
  )

(defun oef-insert-image-by-url ()
  "This function insert an image by url."
  (interactive)
  (insert "<img src=\"«url»\" class=\"«class name»\" width=\"«width»\" height=\"«height»\" alt=\"«alternate text»\"/>")
  )

(defun oef-insert-flash (location)
  "This function insert flash file at point.  LOCATION is `filedir' if we are editing a document and it's `imagedir' if we are editing an exercise."
  (interactive)
  (defvar flash_file "your_file.swf")
  (setq flash_file (read-string "Name of the flash file (your_file.swf has to be upload in \\filedir): " flash_file))
  (insert "<div class=\"wimscenter\">")
  (oef-mode-indent-line)
  (newline) at point
  (insert "<object classid=\"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000\"")
  (oef-mode-indent-line)
  (newline)
  (insert "codebase=\"http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=6,0,29,0\"")
  (oef-mode-indent-line)
  (newline)
  (insert "width=\"800\"")
  (oef-mode-indent-line)
  (newline)
  (insert "height=\"600\">")
  (oef-mode-indent-line)
  (newline)
  (insert "<param name=\"movie\" value=\"\\" location "/" flash_file "\">")
  (oef-mode-indent-line)
  (newline)
  (insert "<param name=\"quality\" value=\"high\">")
  (oef-mode-indent-line)
  (newline)
  (insert  "<embed src=\"\\" location "/" flash_file "\" quality=\"high\"")
  (oef-mode-indent-line)
  (newline)
  (insert "pluginspage=\"http://www.macromedia.com/go/getflashplayer\" type=\"application/x-shockwave-flash\"")
  (oef-mode-indent-line)
  (newline)
  (insert "width=\"800\"")
  (oef-mode-indent-line)
  (newline)
  (insert "height=\"600\"></embed>")
  (oef-mode-indent-line)
  (newline)
  (insert "</object>")
  (oef-mode-indent-line)
  (newline)
  (insert "</div>")
  (oef-mode-indent-line)
  (newline)
  )

(defun oef-insert-flash-in-document ()
  "This function specifies the location of the flash file."
  (interactive)
  (oef-insert-flash "filedir")
  )

(defun oef-insert-flash-in-exercise ()
  "This function specifies the location of the flash file."
  (interactive)
  (oef-insert-flash "imagedir")
  )


(defun oef-insert-folder ()
  (interactive)
  (insert "\\fold{}{«description»}{«content»}")
  )

(defun oef-insert-block-as-folder ()
  (interactive)
  (insert "\\fold{«block name»}{«&opt:description to replace the title»}{}")
  )

(defun oef-insert-block ()
  (interactive)
  (insert "\\embed{«block name»}{«&opt:description to replace the title»}{}")
  )

(defun oef-insert-public-block ()
  (interactive)
  (insert "\\embed{«path»}{}{block=«bloc name»}")
  )

(defun oef-insert-calcform ()
  (interactive)
  (insert "\\calcform{«path»}")
  )

(defun oef-insert-form-current-block ()
  (interactive)
  (insert "\\form{.}{«&opt:anchor»}{«HTML content»}")
  )

(defun oef-insert-form-other-block ()
  (interactive)
  (insert "\\form{«bloc name»}{«&opt:anchor»}{«HTML content»}")
  )

(defun oef-insert-form-outside ()
  (interactive)
  (insert "\\form{«path:serial/name»}{«&opt:anchor»}{«HTML content»}")
  )

(defun oef-insert-input-form ()
  (interactive)
  (insert "<!--BEGIN: EXAMPLE INPUT FORM--> ")
  (newline)
  (insert "\\form{.}{expform}{")
  (newline)
  (insert "Enter your expression:")
  (newline)
  (insert "<input size=\"30\" name=\"parm1\" value=\"\\parm1\"/>")
  (newline)
  (insert "<input type=\"hidden\" value=\"OK\"/>")
  (newline)
  (insert "}")
  (newline)
  (insert "\\def{real value=\\parm1}The expression is evaluated to: \\value.")
  (newline)
  (insert "<!--END: EXAMPLE INPUT FORM--> ")
  )

(defun oef-link-new-tab ()
  (interactive)
  (insert "<a target=\"wims_external\" href=\"«external HTTP link»\">«description»</a>")
  )

(defun oef-link-current-block ()
  (interactive)
  (insert "\\link{.}{«&opt:description&default:bloc title»}{«&opt:anchor»}{«&opt:param1= &param2= ... &param20=»}")
  )

(defun oef-link-other-block ()
  (interactive)
  (insert "\\link{«block name»}{«&opt:description&default:bloc title»}{«&opt:anchor»}{«&opt:param1= &param2= ... &param20=»}")
  )

(defun oef-link-other-document-block ()
  (interactive)
  (insert "\\link{«path:serial/name»}{«&opt:description&default:bloc title»}{«&opt:anchor»}{«&opt:param1= &param2= ... &param20=»}")
  )

(defun oef-link-file ()
  (interactive)
  (insert "\\href{«name»}{«path»}")
  )

(defun oef-reload ()
  (interactive)
  (insert "\\reload{«description»}{«&opt:anchor»}")
  )

(defun oef-tooltip ()
  (interactive)
  (insert "\\tooltip{«description»}{«&opt:options»}{«tooltip text»}")
  )

(defun oef-close-tag ()
  "Close current element.

Depending on context, inserts a matching close-tag, or closes
the current start-tag or the current comment or the current cdata, ..."
  (interactive)
  (with-syntax-table sgml-mode-syntax-table (sgml-close-tag)))

(defun oef-mode-mark-sgml-tag-pair ()
  "Mark the current opening and closing tag.

This function calls `mc/mark-sgml-tag-pair' a `multiple-cursors' command.
This function uses `sgml-mode-syntax-table' because with `oef-mode-syntax-table' there are  problems with tag selection."
  (interactive)
  (with-syntax-table sgml-mode-syntax-table (mc/mark-sgml-tag-pair)))

;; ;sauvegarde au cas ou la tentative de réécriture de oef-mode-indent-line échoue
(defun oef-mode-indent-line ()
  "This function try to smartly indent the line.

It uses `sgml-mode-syntax-table' because with `oef-mode-syntax-table' there are more problems with indentation.
If it fails (it will after '<' or '>' comparison signs) you can use `indent-rigidly' for re-indent manually"
  (interactive)
  (with-syntax-table sgml-mode-syntax-table (sgml-indent-line)))


;; (defun oef-mode-indent-line ()
;;   "This function try to smartly indent the line."
;;   (interactive)
;;   (let* ((savep (point))                ;sauvegarde la position de point dans savep
;; 	 (indent-col
;; 	  (save-excursion               ;Save point, and current buffer; execute BODY; restore those things.
;;             (message "point: %d" (point))
;;             (message "savep: %d" savep)
;; 	    (back-to-indentation)       ;déplace le point sur le premier caractère non blanc sur cette ligne
;;             (if (>= (point) savep) (message "savep nil")) 
;;             (if (>= (point) savep) (setq savep nil)) ;efface savep si le point se trouvait parmi les caractères blancs de début de ligne
;; 	    (oef-mode-calculate-indent)))); calcule l'indentation et place la valeur dans indent-col
;;     (if (null indent-col)
;; 	'noindent
;;       (if savep                         
;; 	  (save-excursion (indent-line-to indent-col)) ;si le point se trouve après le premier caractère non blanc on réalise l'indentation au niveau de indent-col tout en conservant la position du curseur au niveau du même caractère dans la ligne
;;         (indent-line-to indent-col))))) ;si le point se trouve parmi les caractères blancs on réalise l'indentation au niveau de indent-col et on place le point sur le premier caractère non blanc

;; (defun oef-mode-lexical-context (&optional limit)
;;   "Return the lexical context at point as (TYPE . START).
;; START is the location of the start of the lexical element.
;; TYPE is one of `string', `comment', `tag', `cdata', `pi', or `text'.

;; Optional argument LIMIT is the position to start parsing from.
;; If nil, start from a preceding tag at indentation."
;;   (save-excursion
;;     (let ((pos (point))
;; 	  text-start state)
;;       (if limit
;;           (goto-char limit)
;;         ;; Skip tags backwards until we find one at indentation
;;         (while (and (ignore-errors (oef-mode-parse-tag-backward))
;;                     (not (sgml-at-indentation-p)))))
;;       (with-syntax-table sgml-tag-syntax-table
;; 	(while (< (point) pos)
;; 	  ;; When entering this loop we're inside text.
;; 	  (setq text-start (point))
;; 	  (skip-chars-forward "^<" pos)
;;           (setq state
;;                 (cond
;;                  ((= (point) pos)
;;                   ;; We got to the end without seeing a tag.
;;                   nil)
;;                  ((looking-at "<!\\[[A-Z]+\\[")
;;                   ;; We've found a CDATA section or similar.
;;                   (let ((cdata-start (point)))
;;                     (unless (search-forward "]]>" pos 'move)
;;                       (list 0 nil nil 'cdata nil nil nil nil cdata-start))))
;; 		 ((looking-at comment-start-skip)
;; 		  ;; parse-partial-sexp doesn't handle <!-- comments -->,
;; 		  ;; or only if ?- is in sgml-specials, so match explicitly
;; 		  (let ((start (point)))
;; 		    (unless (re-search-forward comment-end-skip pos 'move)
;; 		      (list 0 nil nil nil t nil nil nil start))))
;;                  ((and sgml-xml-mode (looking-at "<\\?"))
;;                   ;; Processing Instructions.
;;                   ;; In SGML, it's basically a normal tag of the form
;;                   ;; <?NAME ...> but in XML, it takes the form <? ... ?>.
;;                   (let ((pi-start (point)))
;;                     (unless (search-forward "?>" pos 'move)
;;                       (list 0 nil nil 'pi nil nil nil nil pi-start))))
;;                  (t
;;                   ;; We've reached a tag.  Parse it.
;;                   ;; FIXME: Handle net-enabling start-tags
;;                   (parse-partial-sexp (point) pos 0))))))
;;       (cond
;;        ((memq (nth 3 state) '(cdata pi)) (cons (nth 3 state) (nth 8 state)))
;;        ((nth 3 state) (cons 'string (nth 8 state)))
;;        ((nth 4 state) (cons 'comment (nth 8 state)))
;;        ((and state (> (nth 0 state) 0)) (cons 'tag (nth 1 state)))
;;        (t (cons 'text text-start))))))




;; (defun oef-mode-parse-tag-backward (&optional limit)
;;   "Parse an SGML tag backward, and return information about the tag.
;; Assume that parsing starts from within a textual context.
;; Leave point at the beginning of the tag."
;;   (catch 'found
;;     (let (tag-type tag-start tag-end name)
;;       (or (sgml--find-<>-backward limit)
;; 	  (error "No tag found"))
;;       (when (eq (char-after) ?<)
;; 	;; Oops!! Looks like we were not in a textual context after all!.
;; 	;; Let's try to recover.
;;         ;; Remember the tag-start so we don't need to look for it later.
;; 	;; This is not just an optimization but also makes sure we don't get
;; 	;; stuck in infloops in cases where "looking back for <" would not go
;; 	;; back far enough.
;;         (setq tag-start (point))
;; 	(with-syntax-table sgml-tag-syntax-table
;; 	  (let ((pos (point)))
;; 	    (condition-case nil
;;                 ;; FIXME: This does not correctly skip over PI an CDATA tags.
;; 		(sgml-forward-sexp 1)
;; 	      (scan-error
;; 	       ;; This < seems to be just a spurious one, let's ignore it.
;; 	       (goto-char pos)
;; 	       (throw 'found (oef-mode-parse-tag-backward limit))))
;; 	    ;; Check it is really a tag, without any extra < or > inside.
;; 	    (unless (sgml-tag-text-p pos (point))
;; 	      (goto-char pos)
;; 	      (throw 'found (oef-mode-parse-tag-backward limit)))
;; 	    (forward-char -1))))
;;       (setq tag-end (1+ (point)))
;;       (cond
;;        ((sgml-looking-back-at "--")	; comment
;; 	(setq tag-type 'comment
;; 	      tag-start (or tag-start (search-backward "<!--" nil t))))
;;        ((sgml-looking-back-at "]]")	; cdata
;; 	(setq tag-type 'cdata
;; 	      tag-start (or tag-start
;;                             (re-search-backward "<!\\[[A-Z]+\\[" nil t))))
;;        ((sgml-looking-back-at "?")      ; XML processing-instruction
;;         (setq tag-type 'pi
;;               ;; IIUC: SGML processing instructions take the form <?foo ...>
;;               ;; i.e. a "normal" tag, handled below.  In XML this is changed
;;               ;; to <?foo ... ?> where "..." can contain < and > and even <?
;;               ;; but not ?>.  This means that when parsing backward, there's
;;               ;; no easy way to make sure that we find the real beginning of
;;               ;; the PI.
;; 	      tag-start (or tag-start (search-backward "<?" nil t))))
;;        (t
;;         (unless tag-start
;;           (setq tag-start
;;                 (with-syntax-table sgml-tag-syntax-table
;;                   (goto-char tag-end)
;;                   (condition-case nil
;;                       (sgml-forward-sexp -1)
;;                     (scan-error
;;                      ;; This > isn't really the end of a tag. Skip it.
;;                      (goto-char (1- tag-end))
;;                      (throw 'found (sgml-parse-tag-backward limit))))
;;                   (point))))
;; 	(goto-char (1+ tag-start))
;; 	(pcase (char-after)
;; 	  (?! (setq tag-type 'decl))    ; declaration
;; 	  (?? (setq tag-type 'pi))      ; processing-instruction
;; 	  (?% (setq tag-type 'jsp))	; JSP tags
;; 	  (?/				; close-tag
;; 	   (forward-char 1)
;; 	   (setq tag-type 'close
;; 		 name (sgml-parse-tag-name)))
;; 	  (_				; open or empty tag
;; 	   (setq tag-type 'open
;; 		 name (sgml-parse-tag-name))
;; 	   (if (or (eq ?/ (char-before (- tag-end 1)))
;; 		   (sgml-empty-tag-p name))
;; 	       (setq tag-type 'empty))))))
;;       (goto-char tag-start)
;;       (sgml-make-tag tag-type tag-start tag-end name))))




;; (defun oef-mode-calculate-indent (&optional lcon)
;;   "Calculate the column to which this line should be indented.
;; LCON is the lexical context, if any.
;; lexical context at point as (TYPE . START)
;; TYPE is one of `string', `comment', `tag', `cdata', `pi', or `text'.
;; START is the location of the start of the lexical element.
;; "
;;   (unless lcon (setq lcon (oef-mode-lexical-context)))
;;   (message "(TYPE. START): %S " lcon)
;;   ;; Indent comment-start markers inside <!-- just like comment-end markers.
;;   (if (and (eq (car lcon) 'tag)
;; 	   (looking-at "--")
;; 	   (save-excursion (goto-char (cdr lcon)) (looking-at "<!--")))
;;       (setq lcon (cons 'comment (+ (cdr lcon) 2))))

;;   (pcase (car lcon)

;;     ('string
;;      ;; Go back to previous non-empty line.
;;      (while (and (> (point) (cdr lcon))
;; 		 (zerop (forward-line -1))
;; 		 (looking-at "[ \t]*$")))
;;      (if (> (point) (cdr lcon))
;; 	 ;; Previous line is inside the string.
;; 	 (current-indentation)
;;        (goto-char (cdr lcon))
;;        (1+ (current-column))))

;;     ('comment
;;      (let ((mark (looking-at "--")))
;;        ;; Go back to previous non-empty line.
;;        (while (and (> (point) (cdr lcon))
;; 		   (zerop (forward-line -1))
;; 		   (or (looking-at "[ \t]*$")
;; 		       (if mark (not (looking-at "[ \t]*--"))))))
;;        (if (> (point) (cdr lcon))
;; 	   ;; Previous line is inside the comment.
;; 	   (skip-chars-forward " \t")
;; 	 (goto-char (cdr lcon))
;; 	 ;; Skip `<!' to get to the `--' with which we want to align.
;; 	 (search-forward "--")
;; 	 (goto-char (match-beginning 0)))
;;        (when (and (not mark) (looking-at "--"))
;; 	 (forward-char 2) (skip-chars-forward " \t"))
;;        (current-column)))

;;     ;; We don't know how to indent it.  Let's be honest about it.
;;     ('cdata nil)
;;     ;; We don't know how to indent it.  Let's be honest about it.
;;     ('pi nil)

;;     ('tag
;;      (goto-char (+ (cdr lcon) sgml-attribute-offset))
;;      (skip-chars-forward "^ \t\n")	;Skip tag name.
;;      (skip-chars-forward " \t")
;;      (if (not (eolp))
;; 	 (current-column)
;;        ;; This is the first attribute: indent.
;;        (goto-char (+ (cdr lcon) sgml-attribute-offset))
;;        (+ (current-column) sgml-basic-offset)))

;;     ('text
;;      (while (looking-at "</")
;;        (sgml-forward-sexp 1)
;;        (skip-chars-forward " \t"))
;;      (let* ((here (point))
;; 	    (unclosed (and ;; (not sgml-xml-mode)
;; 		       (looking-at sgml-tag-name-re)
;; 		       (assoc-string (match-string 1)
;; 				     sgml-unclosed-tags 'ignore-case)
;; 		       (match-string 1)))
;; 	    (context
;; 	     ;; If possible, align on the previous non-empty text line.
;; 	     ;; Otherwise, do a more serious parsing to find the
;; 	     ;; tag(s) relative to which we should be indenting.
;; 	     (if (and (not unclosed) (skip-chars-backward " \t")
;; 		      (< (skip-chars-backward " \t\n") 0)
;; 		      (back-to-indentation)
;; 		      (> (point) (cdr lcon)))
;; 		 nil
;; 	       (goto-char here)
;; 	       (nreverse (sgml-get-context (if unclosed nil 'empty)))))
;; 	    (there (point)))
;;        ;; Ignore previous unclosed start-tag in context.
;;        (while (and context unclosed
;; 		   (eq t (compare-strings
;; 			  (sgml-tag-name (car context)) nil nil
;; 			  unclosed nil nil t)))
;; 	 (setq context (cdr context)))
;;        ;; Indent to reflect nesting.
;;        (cond
;; 	;; If we were not in a text context after all, let's try again.
;; 	((and context (> (sgml-tag-end (car context)) here))
;; 	 (goto-char here)
;; 	 (sgml-calculate-indent
;; 	  (cons (if (memq (sgml-tag-type (car context)) '(comment cdata))
;; 		    (sgml-tag-type (car context)) 'tag)
;; 		(sgml-tag-start (car context)))))
;; 	;; Align on the first element after the nearest open-tag, if any.
;; 	((and context
;; 	      (goto-char (sgml-tag-end (car context)))
;; 	      (skip-chars-forward " \t\n")
;; 	      (< (point) here) (sgml-at-indentation-p))
;; 	 (current-column))
;; 	;; ;; If the parsing failed, try to recover.
;; 	;; ((and (null context) (bobp)
;; 	;; 	(not (eq (char-after here) ?<)))
;; 	;;  (goto-char here)
;; 	;;  (if (and (looking-at "--[ \t\n]*>")
;; 	;; 	    (re-search-backward "<!--" nil t))
;; 	;;      ;; No wonder parsing failed: we're in a comment.
;; 	;;      (sgml-calculate-indent (prog2 (goto-char (match-end 0))
;; 	;; 				  (sgml-lexical-context)
;; 	;; 				(goto-char here)))
;; 	;;    ;; We have no clue what's going on, let's be honest about it.
;; 	;;    nil))
;; 	;; Otherwise, just follow the rules.
;; 	(t
;; 	 (goto-char there)
;; 	 (+ (current-column)
;; 	    (* sgml-basic-offset (length context)))))))

;;     (_
;;      (error "Unrecognized context %s" (car lcon)))

;;     ))









;; No more usefull since indent-line-function targets oef-mode-indent-line
;; (defun oef-mode-indent-region (start end)
;;   "This fuction try to smartly indent the region selected.
;; It uses `sgml-mode-syntax-table' because with `oef-mode-syntax-table' there are more problems with indentation.
;; If it fails (it will after '<' or '>' comparison signs) you can use `indent-rigidly' for re-indent manually
;; the first line which has bad indentation.  Then you can call `oef-mode-indent-region' again for the rest of the code."
;;   (interactive (if (use-region-p)
;;                    (list (region-beginning) (region-end))
;;                  ;; Operate on the current line if region is not to be used.
;;                  (list (line-beginning-position) (line-end-position))))
;;   (with-syntax-table sgml-mode-syntax-table (indent-region start end))
;;   )

(defun oef-comment-toggle (start end)
  "Comment or uncomment a line a region or a command."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 ;; Operate on the current line if region is not to be used.
		 (progn
		   (move-beginning-of-line nil)
		   (delete-horizontal-space)
		   (list (line-beginning-position) (line-end-position)))))
  (if (looking-at "\\\\comment{") ;if the line start with  \comment{
      ;; then we remove the comment to restore  \command{
      (if (looking-at "\\\\comment{\\w*}{") ;if the line start with  \comment{commandName}{
	  ;; it's a commented command
	  (progn
	    (forward-char)
	    (kill-word 1)
	    (delete-char 1)
	    (forward-word)
	    (delete-char 1)
	    ;;(oef-mode-indent-region (line-beginning-position) (line-end-position))
	    (indent-region (line-beginning-position) (line-end-position))
	    )
	;;it's a commented text
	(progn
	  (kill-word 1)
	  (delete-char 1)
	  (move-end-of-line 1)
	  (delete-char -1)
	  ;;(oef-mode-indent-region (line-beginning-position) (line-end-position))
	  (indent-region (line-beginning-position) (line-end-position))
	  ))
    ;; else if the line don't start with a comment
    (if (string= (string (following-char)) "\\") ;
	;;if the line start with a command we turn the line as comment
	(progn
	  (forward-char)
	  (insert "comment{")
	  (forward-word)
	  (insert "}")
	  )
      ;; if not, it's a line or a region to turn in comment
      (progn
	(goto-char start)
	(insert "\\comment{")
	(goto-char (+ 9 end))
	(insert "}")
	)
      ))
  (if (= start end)
      ;; if the line was empty put the cursor between the {} brackets so the user can start typing
      (forward-char -1)
    ;; else go to the beginning of the next line
    (progn
      (move-beginning-of-line nil)
      (forward-line 1)
      )))

(defun  oef-chemistry-simple-bond()
  "Insert a character."
  (interactive)
  (insert " – ")
  )
(defun  oef-insert-french-opening-guillemet()
  "Insert a character."
  (interactive)
  (insert "« ")
  )
(defun  oef-insert-french-closing-guillemet()
  "Insert a character."
  (interactive)
  (insert " »")
  )
(defun  oef-insert-french-guillemets()
  "Insert a character."
  (interactive)
  (insert "«»")
  (backward-char)
  )

(defun  oef-insert-non-breaking-space()
  "Insert a non breaking space character."
  (interactive)
  (insert " ")
  )

(defun  oef-chemistry-double-bond()
  "Insert a character."
  (interactive)
  (insert " = ")
  )

(defun  oef-chemistry-triple-bond()
  "Insert a character."
  (interactive)
  (insert " ≡ ")
  )

(defun  oef-insert-rightarrow()
  "Insert a character."
  (interactive)
  (insert "&rarr;")
  )

(defun  oef-insert-longrightarrow()
  "Insert a character."
  (interactive)
  (insert "\\longrightarrow")
  )

(defun  oef-insert-harpoons()
  "Insert a character."
  (interactive)
  (insert "⇌")
  )

(defun  oef-insert-ell()
  "Insert a character."
  (interactive)
  (insert "ℓ")
  )


;;----------------MENU----------------------------------------

(setq oef-example-files (directory-files-recursively user-emacs-directory ".oef$")) ; list of strings (the oef examples files) needed to build the OEF menu
(setq oef-commands (oef-get-list-commands-names oef-definitions-commands)) ; list of strings (the oef-commands like 'title' and 'author')
(setq oef-special-commands (oef-get-list-commands-names oef-definitions-special-commands)) ; list of strings (the oef-special-commands)
(setq oef-wims-functions (oef-get-list-wims-functions oef-definitions-wims-functions)) ; list of strings (the oef-wims-functions)
(setq oef-slib-scripts (oef-get-list-slib-scripts oef-definitions-slib-scripts)) ; for highlighting
(setq oef-answers-options (oef-get-list-answers-options  oef-menu-answers-options))

(defvar oef-mode-map
  (let ((map (make-sparse-keymap)))
    ;;    (define-key map [menu-bar sgml] 'undefined) ;SGML menu-bar item suppressed
    (define-key map [menu-bar sgml oef-close-tag] '(menu-item "Close Tag" oef-close-tag)) ; `Close Tag' added to Sgml menu-bar
    ;; menu-bar Text
    (define-key map [menu-bar text paragraph-indent-minor-mode] 'undefined) ;Text menu-bar item `Paragraph indent' suppressed
					;    (define-key map [menu-bar text toggle-text-mode-auto-fill] 'undefined) ;Text menu-bar item `Auto Fill' suppressed
    (define-key map [menu-bar text center-region] 'undefined) ;Text menu-bar item `Center region' suppressed
    (define-key map [menu-bar text center-paragraph] 'undefined) ;Text menu-bar item `Center paragraph' suppressed
    (define-key map [menu-bar text center-line] 'undefined) ;Text menu-bar item `Center line' suppressed
    (define-key map [menu-bar text transpose]    (cons "Transpose" (make-sparse-keymap)))
    (define-key map [menu-bar text transpose transpose-lines] '(menu-item "Transpose Lines" transpose-lines)) ;`Transpose Lines' added to Text menu-bar
    (define-key map [menu-bar text transpose transpose-words] '(menu-item "Transpose Words" transpose-words)) ;`Transpose Words' added to Text menu-bar
    (define-key map [menu-bar text transpose transpose-chars] '(menu-item "Transpose Characters" transpose-chars)) ;`Transpose Characters' added to Text menu-bar
    (define-key map [menu-bar text clear]    (cons "Clear Text" (make-sparse-keymap)))
    (define-key map [menu-bar text clear delete-blank-lines] '(menu-item "Delete Blank Lines" delete-blank-lines:help"On blank line, delete all surrounding blank lines, leaving just one.\n
On isolated blank line, delete that one.\n
On nonblank line, delete any immediately following blank lines.")) ;`Delete Blank Lines' added to Text menu-bar
    (define-key map [menu-bar text clear delete-horizontal-space] '(menu-item "Delete All Spaces" delete-horizontal-space)) ;`Delete All Spaces' added to Text menu-bar
    (define-key map [menu-bar text clear just-one-space] '(menu-item "Just One Space" just-one-space)) ;`Just One Space' added to Text menu-bar


    ;;--------------------------------------------------------------------------
    ;; "C-c <LETTER>" are reserved for users
    ;;--------------------------------------------------------------------------
    map)
  "Keymap for `oef-mode'.")

;;-------EASY-MENU-------------------------------------------------------------------

;; Add an OEF menu
(easy-menu-define oef-menu-bar oef-mode-map "OEF-mode menu"
  '("OEF" ; we start by creating a menu that is initially empty. This menu will be called "OEF" in the menu-bar.
    ["Files" nil t]
    ["---" nil t]
    ["Expand Emmet Line" emmet-expand-line t]
    ["Goto" nil t]
    ["Highlight Variable at point (toggle)" oef-highlight-variable t] ;`Highlight oef variable' added to Text menu-bar
    ("Html Tag"
     ["Tag Folding" nil t]
     )
    ["Indent" nil t]
    ["Flyspell (toggle)" flyspell-mode  t]    
    ["Newline (in Multiple-Cursors mode)" electric-newline-and-maybe-indent t]
    ["Rainbow" nil t]
    ["Select «Parameter»" oef-select-parameter t]
    ("Symbol"
     ["Arrows" nil t]
     ["Chemistry Bond" nil t]
     ["Greek" nil t]
     ["Guillemets" nil t]
     ["Nuclear Reaction" nil t]
     )
    ["---" nil t]
    ["Answers Types and Options" nil t]
    ["Commands" nil t]
    ["Comment (toggle)" oef-comment-toggle t]
    ["Defined Variables" nil t]
    ("Draw"
     ["example" oef-canvasdraw-example t]
     ("Flydraw doc"
      ["English" oef-flydraw-commands t]
      ["French" oef-flydraw-commands-fr t]
      )
     ["A" nil t]
     ["B" nil t]
     ["C" nil t]
     ["D" nil t]
     ["E" nil t]
     ["F" nil t]
     ["G" nil t]
     ["H" nil t]
     ["I" nil t]
     ["J" nil t]
     ["K" nil t]
     ["L" nil t]
     ["M" nil t]
     ["N" nil t]
     ["O" nil t]
     ["P" nil t]
     ["Q" nil t]
     ["R" nil t]
     ["S" nil t]
     ["T" nil t]
     ["U" nil t]
     ["V" nil t]
     ["W" nil t]
     ["X" nil t]
     ["Y" nil t]
     ["Z" nil t])
    ["Documents" nil t]
    ["Initializations of Variables (C-o v)" nil t]
    ["Mathematical Expression" oef-insert-math t]
    ["Random" nil t]
    ["Reserved Words" nil t]
    ("Script Library"
     ["All" nil t]
     ["---" nil t]
     ["Algebra" nil t]
     ["Analysis" nil t]
     ["Chemistry" nil t]
     ["Circuits" nil t]
     ["Data" nil t]
     ["Draw" nil t]
     ["Function" nil t]
     ["Games" nil t]
     ["Geogebra" nil t]
     ["Graph" nil t]
     ["Graphpaper" nil t]
     ["Lang" nil t]
     ["Life" nil t]
     ["List" nil t]
     ["Matrix" nil t]
     ["Media" nil t]
     ["Numeration" nil t]
     ["OEF" nil t]
     ["Polynomial" nil t]
     ["Set" nil t]
     ["Stat" nil t]
     ["Text" nil t]
     ["Triplerelation" nil t]
     ["Utilities" nil t])

    ["Wims Functions" nil t]
    ["---" nil t]
    ["Edit in browser" oef-edit-in-browser t]
    ["Wims Session" nil t]
    ))

(easy-menu-add-item oef-menu-bar '("Files") (oef-get-examples)) ; we add the submenu `Examples' to the oef-menu-bar. This menu is not dynamic.
(easy-menu-add-item oef-menu-bar '("Files")["Open All OEF Examples" oef-mode-open-all t]) ; we add the command "Open All OEF Examples" to the submenu `Examples' in the oef-menu-bar.
;; (easy-menu-add-item oef-menu-bar '("Files") (get-my-oef-files)) ; deactivatedd (too slow)
(easy-menu-add-item oef-menu-bar '("Symbol" "Chemistry Bond")["Simple Bond –" oef-chemistry-simple-bond])
(easy-menu-add-item oef-menu-bar '("Symbol" "Chemistry Bond")["Double Bond =" oef-chemistry-double-bond])
(easy-menu-add-item oef-menu-bar '("Symbol" "Chemistry Bond")["Triple Bond ≡" oef-chemistry-triple-bond])
(easy-menu-add-item oef-menu-bar '("Symbol" "Arrows")["Right Arrow" oef-insert-rightarrow])
(easy-menu-add-item oef-menu-bar '("Symbol" "Arrows")["Long Right Arrow" oef-insert-longrightarrow])
(easy-menu-add-item oef-menu-bar '("Symbol" "Arrows")["Harpoons" oef-insert-harpoons])
(easy-menu-add-item oef-menu-bar '("Symbol")["ℓ" oef-insert-ell])
(easy-menu-add-item oef-menu-bar '("Symbol" "Nuclear Reaction")["Alpha Particle" oef-insert-alpha-particle])
(easy-menu-add-item oef-menu-bar '("Symbol" "Nuclear Reaction")["Carbon-14" oef-insert-carbon])
(easy-menu-add-item oef-menu-bar '("Symbol" "Nuclear Reaction")["Electron" oef-insert-electron])
(easy-menu-add-item oef-menu-bar '("Symbol" "Nuclear Reaction")["Isotope" oef-insert-isotope])
(easy-menu-add-item oef-menu-bar '("Symbol" "Nuclear Reaction")["Neutron" oef-insert-neutron])
(easy-menu-add-item oef-menu-bar '("Symbol" "Nuclear Reaction")["Positron" oef-insert-positron])
(easy-menu-add-item oef-menu-bar '("Symbol" "Nuclear Reaction")["Proton" oef-insert-proton])
(easy-menu-add-item oef-menu-bar '("Symbol" "Nuclear Reaction")["Uranium-238" oef-insert-uranium])
(easy-menu-add-item oef-menu-bar '("Symbol" "Guillemets")["« " oef-insert-french-opening-guillemet])
(easy-menu-add-item oef-menu-bar '("Symbol" "Guillemets")[" »" oef-insert-french-closing-guillemet])
(easy-menu-add-item oef-menu-bar '("Symbol" "Guillemets")["« »" oef-insert-french-guillemets])
(easy-menu-add-item oef-menu-bar '("Symbol")["Non Breaking Space  " oef-insert-non-breaking-space])
(easy-menu-add-item oef-menu-bar '("Goto")["Goto Answer" oef-goto-answers :help"Goto Answers"]) ;
(easy-menu-add-item oef-menu-bar '("Goto")["Goto CSS" oef-goto-css :help"Goto CSS"]) ;
(easy-menu-add-item oef-menu-bar '("Goto")["Goto Line" goto-line :help"Goto line"]) ;
(easy-menu-add-item oef-menu-bar '("Goto")["Goto Reply" oef-goto-reply :help"Goto Reply"])
(easy-menu-add-item oef-menu-bar '("Goto")["Goto Statement" oef-goto-statement :help"Goto Statement"])
(easy-menu-add-item oef-menu-bar '("Html Tag")["Select Tag Pair" oef-mode-mark-sgml-tag-pair :help"Mark the current opening and closing tag"]) ;
(easy-menu-add-item oef-menu-bar '("Html Tag")["Select Inner Tag" er/mark-inner-tag :help"Mark the content between current opening and closing tag"]) ;
(easy-menu-add-item oef-menu-bar '("Html Tag")["<b> bold" oef-insert-tag-b]) ;
(easy-menu-add-item oef-menu-bar '("Html Tag")["<code> code" oef-insert-tag-code]) ;
(easy-menu-add-item oef-menu-bar '("Html Tag")["<mark> marked text" oef-insert-tag-mark]) ;
(easy-menu-add-item oef-menu-bar '("Html Tag")["<sub> superscript" oef-insert-tag-sub]) ;
(easy-menu-add-item oef-menu-bar '("Html Tag")["<sup> subscript" oef-insert-tag-sup]) ;
(easy-menu-add-item oef-menu-bar '("Html Tag" "Tag Folding")["Toogle      [C-o t t]" yafolding-toggle-element])
(easy-menu-add-item oef-menu-bar '("Html Tag" "Tag Folding")["Show (All)  [C-o t s]" yafolding-show-all])
(easy-menu-add-item oef-menu-bar '("Html Tag" "Tag Folding")["Hide (All)  [C-o t h]" yafolding-hide-all])
(easy-menu-add-item oef-menu-bar '("Rainbow")["Delimiters (toogle)" rainbow-delimiters-mode])
(easy-menu-add-item oef-menu-bar '("Rainbow")["Colors (toogle)" rainbow-mode])
(easy-menu-add-item oef-menu-bar '("Indent")["Indent line" oef-mode-indent-line])
(easy-menu-add-item oef-menu-bar '("Indent")["Indent Region" indent-region])
(easy-menu-add-item oef-menu-bar '("Indent")["Indent Rigidly" indent-rigidly])
(easy-menu-add-item oef-menu-bar '()["Wims Session" nil t]); it's not a real connection (It just extract the session id from the URL)
(easy-menu-add-item oef-menu-bar '("Wims Session")["Connect to a Wims Session" oef-get-wims-session :help "Connect emacs to the active Wims Session if the URL is in the CLIPBOARD."]); it's not a real connection (It just extract the session id from the URL)
(easy-menu-add-item oef-menu-bar '("Wims Session")["Edit Exercise in Browser" oef-edit-exercise-in-browser :help "If the connection with the server is active,\n edit the Exercise wich has the same name on the WIMS server.\n Also copy the buffer content in the CLIPBOARD."]);
(easy-menu-add-item oef-menu-bar '("Wims Session")["Edit Document in Browser" oef-edit-document-in-browser :help "If the connection with the server is active,\n edit the Document wich has the same name on the WIMS server.\n Also copy the buffer content in the CLIPBOARD."]);

(easy-menu-add-item oef-menu-bar '()["Random" nil t])
(easy-menu-add-item oef-menu-bar '("Random")["Random Integer" (lambda () (interactive) (insert "randint(..)") (forward-char -3)) :help "Syntax: randint(n1..n2)\n\nReturns a random integer between n1 and n2 (inclusive)."])
(easy-menu-add-item oef-menu-bar '("Random")["Random Float" (lambda () (interactive) (insert "random(..)") (forward-char -3)) :help "Syntax: random(n1..n2)\n\nReturns a random float between n1 and n2 (inclusive)."])
(easy-menu-add-item oef-menu-bar '("Random")["Random Item" (lambda () (interactive) (insert "randitem()") (forward-char -1)) :help "Syntax: randitem(n1,n2,n3,n4) or random(n1,n2,n3,n4) or randomitem(n1,n2,n3,n4)\n\nReturns a random item of a list (comma separated values)."])
(easy-menu-add-item oef-menu-bar '("Random")["Random Row" (lambda () (interactive) (insert "randomrow()") (forward-char -1)) :help "Syntax: randomrow(\\mat)\n\nReturns a random line of a matrix."])
(easy-menu-add-item oef-menu-bar '("Random")["Shuffle" (lambda () (interactive) (insert "shuffle()") (forward-char -1)) :help "Syntax: shuffle(n)\n\nReturns a randomly permuted list  of the n first positive integers."])
(easy-menu-add-item oef-menu-bar '("Random")["Shuffle List" (lambda () (interactive) (insert "shuffle()") (forward-char -1)) :help "Syntax: shuffle(\\list)\n\nA new list with randomly permuted items in list is returned."])
(easy-menu-add-item oef-menu-bar '("Symbol" "Greek")["Ɣ" (lambda () (interactive) (insert "Ɣ"))])
(easy-menu-add-item oef-menu-bar '()["Rainbow" nil t])
(easy-menu-add-item oef-menu-bar '("Initializations of Variables (C-o v)") (oef-get-exo-init-types)) ; we add the submenu `Exercises' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '("Initializations of Variables (C-o v)") (oef-get-doc-init-types)) ; we add the submenu `Documents' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (oef-get-menu-commands)) ; we add the submenu `Commands' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '("Commands") (oef-get-menu-special-commands)) ; we add the submenu `Special' in menu `Commands' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (oef-get-answers-options)) ; we add the submenu `Answers types and options' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (oef-get-defined-variables)) ; we add the submenu `oef-defined-variables' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (oef-get-language-reserved-words)) ; we add the submenu `oef-language-reserved-words' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (oef-get-wims-functions)) ; we add the submenu `Wims Functions' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-scripts)) ; we add the submenu `All' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-algebra)) ; we add the submenu `Algebra' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-analysis)) ; we add the submenu `Analysis' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-chemistry)) ; we add the submenu `Chemistry' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-circuits)) ; we add the submenu `Circuits' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-data)) ; we add the submenu `Data' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-draw)) ; we add the submenu `Draw' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-function)) ; we add the submenu `Function' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-games)) ; we add the submenu `Games' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-geogebra)) ; we add the submenu `Geogebra' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-graph)) ; we add the submenu `Graph' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-graphpaper)) ; we add the submenu `Graphpaper' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-lang)) ; we add the submenu `Lang' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-life)) ; we add the submenu `Life' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-list)) ; we add the submenu `List' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-matrix)) ; we add the submenu `Matrix' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-media)) ; we add the submenu `Media' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-numeration)) ; we add the submenu `Numeration' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-oef)) ; we add the submenu `OEF' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-polynomial)) ; we add the submenu `Polynomial' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-set)) ; we add the submenu `Set' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-stat)) ; we add the submenu `Stat' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-text)) ; we add the submenu `Text' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-triplerelation)) ; we add the submenu `Triplerelation' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Script Library") (oef-get-slib-utilities)) ; we add the submenu `Utilities' to the `Script_Library' menu.
(easy-menu-add-item oef-menu-bar '("Documents")["Files" nil]); create submenu `Files'  in `Documents'
(easy-menu-add-item oef-menu-bar '("Documents" "Files")["Entrance" oef-find-main :help "The Entrance block of the document is always named `main'"])
(easy-menu-add-item oef-menu-bar '("Documents" "Files")["Other" oef-find-block :help "Other block of the document"])
(easy-menu-add-item oef-menu-bar '("Documents")["Insert" nil]); create submenu `Insert' in submenu  `Documents'
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["Image in Document" oef-insert-image-in-document :help "Insert an image.\n\nThe link `Other files' allows you to upload the image file to the wims server.\nCSS properties are defined online in properties of the main file."])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["Image in Exercise" oef-insert-image-in-exercise :help "Insert an image.\n\nThe link `Other files' allows you to upload the image file to the wims server.\nCSS properties are defined online in properties of the main file."])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["Image by URL" oef-insert-image-by-url :help "Insert an image by url."])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["--" nil])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["Flash in Document" oef-insert-flash-in-document :help "Insert a flash animation  in the current page by specifying content"])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["Flash in Exercise" oef-insert-flash-in-exercise :help "Insert a flash animation  in the current exercise by specifying content"])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["---" nil])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["Folder" oef-insert-folder :help "Insert a folder in the current page by specifying content.\n\nAttention : The foldable parts within a same block do not allow automatic formatting of mathematical formulas, nor execution of commands. "])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["Block as Folder" oef-insert-block-as-folder :help "Insert a block as a folder in the current page by specifying the name of the block."])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["---" nil])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["Block" oef-insert-block :help "Insert a block in the current page by specifying the name of the block."])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["Public Block" oef-insert-public-block :help "Insert a block of a public document in the current page\n\nby specifying the path to public document and the name of the block."])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["----" nil])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["Calcform" oef-insert-calcform :help " Insert popup tool forms of WIMS.\n\nTo find the address of forms, make a search of such forms in the home page of WIMS.\nThen the address of each form can be found in the source of the returned page."])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["-----" nil])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["HTML Form (here)" oef-insert-form-current-block :help "Insert  an HTML form within the current block."])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["HTML input Form (example)" oef-insert-input-form :help "For example, the following form allows the input of an arbitrary numerical expression.\nThis expression is then evaluated in the document."])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["HTML Form (in block)" oef-insert-form-other-block :help "Insert  an HTML form within another block."])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["HTML Form (outside)" oef-insert-form-outside :help "Insert  an HTML form within a block in another document.\n\nThe path must be under the form serial/name,\n where serial is the serial number of the other document,\n and name the name of the block."])
(easy-menu-add-item oef-menu-bar '("Documents")["Link" nil]); create submenu `Link' in submenu  `Documents'
(easy-menu-add-item oef-menu-bar '("Documents" "Link")["External Target in New Tab" oef-link-new-tab :help "HTML link. External page will appear in a new Tab of the browser."])
(easy-menu-add-item oef-menu-bar '("Documents" "Link")["Current Block" oef-link-current-block :help "Link towards the current block.\n\nArgument 4 (optional) is for parameters transmission.\nParameters should be of the form parm1=...&parm2=...&parm3=...\nUp to 20 parameters can be used.\nThese parameters can then be used in the linked document,\n under the names of \\parm1, \\parm2, ..."])
(easy-menu-add-item oef-menu-bar '("Documents" "Link")["Other Block in Document" oef-link-other-block :help "Link towards an other block target by its name.\n\nArgument 4 (optional) is for parameters transmission.\nParameters should be of the form parm1=...&parm2=...&parm3=...\nUp to 20 parameters can be used.\nThese parameters can then be used in the linked document,\n under the names of \\parm1, \\parm2, ..."])
(easy-menu-add-item oef-menu-bar '("Documents" "Link")["Block in Another Document" oef-link-other-document-block :help "Link towards a block in another document.\n\n The block is target by serial/name,\nwhere serial is the serial number of the other document,\n and name the name of the block.\nArgument 4 (optional) is for parameters transmission.\nParameters should be of the form parm1=...&parm2=...&parm3=...\nUp to 20 parameters can be used.\nThese parameters can then be used in the linked document,\n under the names of \\parm1, \\parm2, ..."])
(easy-menu-add-item oef-menu-bar '("Documents" "Link")["Uploaded File" oef-link-file :help "Link towards an uploaded file.\n\nThe link `Other files' allows you to upload files to the wims server."])
(easy-menu-add-item oef-menu-bar '("Documents" "Link")["Reload" oef-reload :help " Reload the page.\n\nUp to 2 arguments: the text to show on the link and the position to go (anchor).\nReloading a page is interesting when it contains random variables.\nIn this case, to each reloading, the resulting page is different."])
(easy-menu-add-item oef-menu-bar '("Documents")["Tooltip" oef-tooltip :help "Tooltip on words.\n\nUp to 3 arguments:\nArgument 1: the prompt. You may change the style of the prompt by using HTML tags\nor by defining the css style class span.tooltip.\nArgument 2 (optional): the options of the tooltip between [ ]. If the word nojs\nis added (outside the brackets), the used javascript is not reload (it is sufficient to load it once at the begining of the html page).\nArgument 3 : the text inside the tooltip."])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["affine" (lambda () (interactive) (insert "affine a,b,c,d,tx,ty ")) :help "affine a,b,c,d,tx,ty\n\n\t○ defines a transformation matrix for subsequent objects\n\t○ images drawn by setting skew params a & d will be very different from Flydraw's \"affine a,b,c,d,e,tx,ty\" !!\n\t○ use keyword 'killaffine' to end the transformation\n\t○ note 1: only 'draggable' / 'noclick' objects can be transformed.\n\t○ note 2: do not use 'onclick' or 'drag xy' with tranformation objects : the mouse coordinates do not get transformed (yet)\n\t○ note 3: no matrix operations on the transformation matrix implemented (yet)\n\t○ a : Scales the drawings horizontally\n\t○ b : Skews the drawings horizontally\n\t○ c : Skews the drawings vertically\n\t○ d : Scales the drawings vertically\n\t○ tx: Moves the drawings horizontally in xrange coordinate system\n\t○ ty: Moves the drawings vertically in yrange coordinate system\n\t○ the data precision may be set by preceding command \"precision int\""])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["angle" (lambda () (interactive) (insert "angle xc,yc,width,start_angle,end_angle,color ")) :help "angle xc,yc,width,start_angle,end_angle,color\n\n\t○ width is in x-range\n\t○ will zoom in/out\n\t○ if size is controlled by command 'slider' use radians to set limits of slider."])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["animate" (lambda () (interactive) (insert "animate type ")) :help "animate type\n\n\t○ REMOVED : this should be done with a slider\n\t○ type may be \"point\" (nothing else , yet...)\n\t○ the point is a filled rectangle ; adjust colour with command 'fillcolor colorname/hexnumber'\n\t○ will animate a point on the next plot/curve command\n\t○ the curve will not be draw\n\t○ moves repeatedly from xmin to xmax"])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["arc" (lambda () (interactive) (insert "arc xc,yc,width,height,start_angle,end_angle,color ")) :help "arc xc,yc,width,height,start_angle,end_angle,color\n\n\t○ can not be set \"onclick\" or \"drag xy\"\n\t○ attention: width in height in x/y-range\n\t○ will not zoom in or zoom out (because radius is given in pixels an not in x/y-system !). Panning will work\n\t○ use command 'angle' for scalable angle"])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["arrow" (lambda () (interactive) (insert "arrow x1,y1,x2,y2,h,color ")) :help "arrow x1,y1,x2,y2,h,color\n\n\t○ alternative command:vector\n\t○ draw a single headed arrow / vector from (x1:y1) to (x2:y2)\n\t○ with arrowhead size h in px and in color 'color'\n\t○ use command 'linewidth int' to adjust thickness of the arrow\n\t○ may be set draggable / onclick"])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["arrow2" (lambda () (interactive) (insert "arrow2 ")) :help "arrow2 x1,y1,x2,y2,h,color\n\n\t○ draw a double headed arrow/vector from (x1:y1) to (x2:y2)\n\t○ with arrowhead size h in px and in color 'color'\n\t○ use command 'arrowhead int' to adjust the arrow head size\n\t○ use command 'linewidth int' to adjust thickness of the arrow\n\t○ may be set draggable / onclick"])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["arrowhead" (lambda () (interactive) (insert "arrowhead int ")) :help "arrowhead int\n\n\t○ default 8 (pixels)"])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["arrows" (lambda () (interactive) (insert "arrows color,head (px),x1,y1,x2,y2...x_n,y_n ")) :help "arrows color,head (px),x1,y1,x2,y2...x_n,y_n\n\n\t○ alternative command:vectors\n\t○ draw single headed arrows / vectors from (x1:y1) to (x2:y2) ... (x3:y3) to (x4:y4) etc ... in color 'color'\n\t○ use command 'linewidth int' to adjust thickness of the arrow\n\t○ may be set draggable / onclick individually"])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["arrows2" (lambda () (interactive) (insert "arrows2 color,head (px),x1,y1,x2,y2...x_n,y_n ")) :help "arrows2 color,head (px),x1,y1,x2,y2...x_n,y_n\n\n\t○ draw double headed arrows / vectors from (x1:y1) to (x2:y2) ... (x3:y3) to (x4:y4) etc ... in color 'color'\n\t○ use command 'linewidth int' to adjust thickness of the arrows\n\t○ may be set draggable / onclick individually"])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["audio" (lambda () (interactive) (insert "audio x,y,w,h,loop,visible,audiofile location ")) :help "audio x,y,w,h,loop,visible,audiofile location\n\n\t○ x,y : left top corner of audio element (in xrange / yrange)\n\t○ w,y : width and height in pixels\n\t○ loop : 0 or 1 ( 1 = loop audio fragment)\n\t○ visible : 0 or 1 (1 = show controls)\n\t○ audio format may be in *.mp3 or *.ogg\n\t○ If you are using *.mp3 : be aware that FireFox will not (never) play this ! (Pattented format)\n\t○ if you are using *.ogg : be aware that Microsoft based systems not support it natively\n\t○ To avoid problems supply both types (mp3 and ogg) of audiofiles.\n\t the program will use both as source tag\n\t○ example: upload both audio1.ogg and audio1.mp3 to http://server/files/\n\t audio 0,0,http://server/files/audio1.mp3\n\t svdraw will copy html-tag audio1.mp3 to audio1.ogg\n\t and the browser will play the compatible file (audio1.ogg or audio1.mp3)"])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["axis" (lambda () (interactive) (insert "axis ")) :help "axis\n\n\t○ keyword (no arguments required)\n\t○ to be used before command grid (see command grid)"])

(easy-menu-add-item oef-menu-bar '("Draw" "A")["axisnumbering" (lambda () (interactive) (insert "axisnumbering ")) :help "axisnumbering\n\n\t○ keyword (no arguments required)\n\t○ for special numbering of x-axis or y-axis see grid related commands axis xaxis , xaxisup, noxaxis ,yaxis , yaxisup, noyaxis\n\t○ to be used before command grid (see command grid)"])

(easy-menu-add-item oef-menu-bar '("Draw" "B")["barchart" (lambda () (interactive) (insert "barchart x_1:y_1:color_1:x_2:y_2:color_2:...x_n:y_n:color_n ")) :help "barchart x_1:y_1:color_1:x_2:y_2:color_2:...x_n:y_n:color_n\n\n\t○ may only to be used together with command 'grid'\n\t○ can be used together with freestyle x-axis/y-axis texts : see commands 'xaxis','xaxisup' and 'yaxis'\n\t○ use command 'legend' to provide an optional legend in right-top-corner\n\t○ multiple barchart command may be used in a single script\n\t○ also see command 'piechart'\n\t○ note: your arguments are not checked by canvasdraw : use your javascript console in case of trouble..."])

(easy-menu-add-item oef-menu-bar '("Draw" "B")["bezier" (lambda () (interactive) (insert "bezier color,x_start,y_start,x_first,y_first,x_second,y_second,x_end,y_end ")) :help "bezier color,x_start,y_start,x_first,y_first,x_second,y_second,x_end,y_end\n\n\t○ draw a bezier curve between points, starting from (x_start:y_start)\n\t○ can not be dragged or set onclick"])

(easy-menu-add-item oef-menu-bar '("Draw" "B")["bgcolor" (lambda () (interactive) (insert "bgcolor colorname or #hex ")) :help "bgcolor colorname or #hex\n\n\t○ use this color as background of the "div" containing the canvas(es)"])

(easy-menu-add-item oef-menu-bar '("Draw" "B")["bgimage" (lambda () (interactive) (insert "bgimage image_location ")) :help "bgimage image_location\n\n\t○ use an image as background .\n\t○ technical: we use the background of 'canvas_div'\n\t○ the background image will be resized to match \"width = xsize\" and \"height = ysize\""])

(easy-menu-add-item oef-menu-bar '("Draw" "B")["blink" (lambda () (interactive) (insert "blink time(seconds) ")) :help "blink time(seconds)\n\n\t○ NOT IMPLEMETED -YET"])

(easy-menu-add-item oef-menu-bar '("Draw" "B")["boxplot" (lambda () (interactive) (insert "boxplot x_or_y,box-height_or_box-width,position,min,Q1,median,Q3,max ")) :help "boxplot x_or_y,box-height_or_box-width,position,min,Q1,median,Q3,max\n\n\t○ example:\n\t\t xrange 0,300\n\t\t yrange 0,10\n\t\t boxplot x,4,8,120,160,170,220,245\n\t\t meaning: create a boxplot in x-direction, with height 4 (in yrange) and centered around line y=8\n\t○ example:\n\t\t xrange 0,10\n\t\t yrange 0,300\n\t\t boxplot y,4,8,120,160,170,220,245\n\t\t meaning: create a boxplot in y-direction, with width 4 (in xrange) and centered around line x=8\n\t○ use command 'filled' to fill the box\n\t\t note: the strokecolor is used for filling Q1, the fillcolor is used for filling Q3\n\t○ use command 'opacity' to adjust fill_opacity of stroke and fill colours\n\t○ use command 'legend' to automatically create a legend\n\t\t unicode allowed in legend\n\t\t use command 'fontfamily' to set the font of the legend.\n\t○ there is no limit to the number of boxplots used.\n\t○ can not be set draggable ('onclick' is not ready ,yet)\n\t○ use keyword 'userboxplot' before command boxplot, if a pupil must draw a boxplot (using his own min,Q1,median,Q3,max data)\n\t○ use keyword 'userboxplotdata' before command boxplot, if a pupil must generate the data by some means.\n\t○ use command 'boxplotdata' when the boxplot should be drawn from wims-generated raw statistical date"])

(easy-menu-add-item oef-menu-bar '("Draw" "B")["boxplotdata" (lambda () (interactive) (insert "boxplotdata some_data ")) :help "boxplotdata some_data\n\n\t○ 'some_data' are a list of numbers separated by a comma \",\" (items)\n\t○ only be used before command 'boxplot': the command 'boxplot' will provide the boxplot drawing of the data.\n\t○ xrange 0,100\n\t\t yrange 0,10\n\t\t boxplotdata 11,22,13,15,23,43,12,12,14,2,45,32,44,13,21,24,13,19,35,21,24,23\n\t\t boxplot x,4,5\n\t○ note: wims will not check your data input | format. use js-error console to debug any problems.\n\t○ a javascript function 'statistics()' will parse the data and calculate the values [min,Q1,median,Q3,max] and hand them to the boxplot draw\n\t○ function."])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["\\canvasdraw" (lambda () (interactive) (insert "\\canvasdraw{«width»,«height»}{\\mydraw} ")) :help "First you have to define a text variable (i.e. \\mydraw) containing the drawing code"])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["canvastype" (lambda () (interactive) (insert "canvastype TYPE ")) :help "canvastype TYPE\n\n\t○ for now only usefull before commands filltoborder / floodfill / clickfill etc operations\n\t\t Only the images of this TYPE will be scanned and filled\n\t○ default value of TYPE is DRAG_CANVAS e.g. 5\n\t○ use another TYPE if you know what you are doing...\n\t○ other possible canvasses (transparent PNG pictures xsize x ysize on top of eachother)\n\t\t◦ EXTERNAL_IMAGE_CANVAS = 0\n\t\t◦ BG_CANVAS = 1\n\t\t◦ STATIC_CANVAS = 2\n\t\t◦ MOUSE_CANVAS = 3 : used for command \"mouse\"\n\t\t◦ GRID_CANVAS = 4 :used for command \"grid\"\n\t\t◦ DRAG_CANVAS = 5 :default\n\t\t◦ DRAW_CANVAS = 6 :used for some static drawings\n\t\t◦ TEXT_CANVAS = 7 : used for text-strings\n\t\t◦ CLOCK_CANVAS = 8 : used for command \"clock\"\n\t\t◦ ANIMATE_CANVAS = 9 : not used for now\n\t\t◦ TRACE_CANVAS = 10 : used for command \"trace_jscurve\"\n\t\t◦ JSPLOT_CANVAS = 111 : will be increased with every new command \"jscurve\"\n\t\t◦ FILL_CANVAS = 12 : this will be filled...so do not use !\n\t\t◦ USERDRAW_JSPLOT 13 : will be increased with every new command \"userinput function\""])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["centerstring" (lambda () (interactive) (insert "centerstring color,y-value,the text string ")) :help "centerstring color,y-value,the text string\n\n\t○ title color,y-value,the text string\n\t○ draw a string centered on the canvas at y = y-value\n\t○ can not be set \"onclick\" or \"drag xy\" (...)\n\t○ unicode supported: centerstring red,5,\\u2232\n\t○ use a command like 'fontfamily italic 24px Ariel'\n\t○ to set fonts on browser that support font change"])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["circle" (lambda () (interactive) (insert "circle xc,yc,width (2*r in pixels),color ")) :help "circle xc,yc,width (2*r in pixels),color\n\n\t○ use command 'fcircle xc,yc,d,color'\n\t○ alternative: disk for a filled circle\n\t○ use command 'fillcolor color' to set the fillcolor\n\t○ may be set draggable / onclick\n\t○ will shrink / expand on zoom out / zoom in"])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["circles" (lambda () (interactive) (insert "circles color,xc1,yc1,r1,xc2,yc2,r2...xc_n,yc_n,r_n ")) :help "circles color,xc1,yc1,r1,xc2,yc2,r2...xc_n,yc_n,r_n\n\n\t○ attention r = radius in x-range (!)\n\t○ use keyword 'filled' or command 'fcircles' to produce solid circles\n\t○ alternative command:disks\n\t○ use command 'fillcolor color' to set the fillcolor\n\t○ may be set draggable / onclick (individually)\n\t○ will shrink / expand on zoom out / zoom in"])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["clearbutton" (lambda () (interactive) (insert "clearbutton value ")) :help "clearbutton value\n\n\t○ alternative command:delete\n\t○ alternative command:erase\n\t○ adds a button to clear the userdraw canvas with text 'value'\n\t○ attention command 'clearbutton' is incompatible with multidraw based drawings\n\t(in 'multidraw' there is always a remove_object_button for every drawprimitive)\n\t○ normally userdraw primitives have the option to use middle/right mouse button on\n\ta point of the object to remove this specific object...this clear button will remove all drawings\n\t○ uses the tooltip placeholder div element: may not be used with command 'intooltip'\n\t○ use command 'inputstyle' to style the button...\n\t○ the clearbutton will have id=\"canvas_scripts[%d]\" ; starting with %d=0 for the first script\n\tto change the style of all \"clearbutton\" of all included canvasdraw scripts, use something like\n\tif(document.getElementById(\"clearbutton\"+canvas_scripts[0])){\n\tvar p = 0;\n\twhile(document.getElementById(\"clearbutton\"+canvas_scripts[p])){\n\tdocument.getElementById(\"clearbutton\"+canvas_scripts[p]).className=\"some_class_name\";\n\t<!−− or document.getElementById(\"clearbutton\"+canvas_scripts[p]).setAttribute(\"style\",\"some_style\"); −−>\n\tp++;\n\t};\n\t};"])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["clock" (lambda () (interactive) (insert "clock x,y,r(px),H,M,S,type hourglass,interactive [ ,H_color,M_color,S_color,background_color,foreground_color ] ")) :help "clock x,y,r(px),H,M,S,type hourglass,interactive [ ,H_color,M_color,S_color,background_color,foreground_color ]\n\n\t○ use command 'opacity stroke-opacity,fill-opacity' to adjust foreground (stroke) and background (fill) transparency\n\t○ type hourglass:\n\t type = 0 : only segments\n\t type = 1 : only numbers\n\t type = 2 : numbers and segments\n\t○ colors are optional: if not defined, default values will be used\n\t default colours: clock 0,0,60,4,35,45,1,2\n\t custom colours: clock 0,0,60,4,35,45,1,2,,,,yellow,red\n\t custom colours: clock 0,0,60,4,35,45,1,2,white,green,blue,black,yellow\n\t○ if you don't want a seconds hand (or minutes...), just make it invisible by using the background color of the hourglass...\n\t○ interactive\n\t\t◦ 0 : not interactive, just clock(s)\n\t\t◦ 1 : function read_canvas() will read all active clocks in H:M:S format\n\t\t\t The active clock(s) can be adjusted by pupils\n\t\t◦ 2 : function read_canvas() will return the clicked clock\n\t\t\t (like multiplechoice; first clock in script in nr. 0 )\n\t\t◦ 3: no prefab buttons...create your own buttons (or other means) to make the clock(s) adjustable by javascript function"])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["copy" (lambda () (interactive) (insert "copy x,y,x1,y1,x2,y2,[filename URL] ")) :help "copy x,y,x1,y1,x2,y2,[filename URL]\n\n\t○ The image may be \"bitmap\" or \"SVG\"\n\t○ Insert the region from (x1,y1) to (x2,y2) (in pixels) of [filename] to (x,y) in x/y-range\n\t○ If x1=y1=x2=y2=-1, the whole [filename URL] is copied.\n\t○ [filename] is the URL of the image\n\t○ URL is normal URL of network reachable image file location\n\t○ if command 'drag x/y/xy' is set before command 'copy', the images will be draggable\n\t○ if keyword 'onclick' is set before command 'copy' the image(s) is clickable (marked with a green rectangle around the image)\n\t○ 'onclick' for external images may be mixed with canvas generated stuff (like lines,curves etc)\n\t○ you may draw / userdraw / drag other stuff on top of an \"imported\" image"])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["copyresized" (lambda () (interactive) (insert "copyresized x1,y2,x2,y2,dx1,dy1,dx2,dy2,image_file_url ")) :help "Insert the region from (x1,y1) to (x2,y2) (in pixels) of [ filename], possibly resized, to the region of (dx1,dy1) to (dx2,dy2) in x/y-range "])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["crosshair" (lambda () (interactive) (insert "crosshair x,y,color ")) :help "use command 'crosshairsize int' and / or 'linewidth int' to adjust "])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["crosshairs" (lambda () (interactive) (insert "crosshairs color,x1,y1,x2,y2,...,x_n,y_n ")) :help "use command 'crosshairsize int' and / or 'linewidth int' to adjust "])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["crosshairsize" (lambda () (interactive) (insert "crosshairsize int ")) :help "default 8 (px) "])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["cursor" (lambda () (interactive) (insert "cursor 'some CSS cursor_style' ")) :help "style can be any valid CSS property value, like crosshair, grabbing, move etc "])

(easy-menu-add-item oef-menu-bar '("Draw" "C")["curve" (lambda () (interactive) (insert "curve color,formula(x) ")) :help "use only basic math in your curve: sqrt,^,asin,acos,atan,log,pi,abs,sin,cos,tan,e "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["dashed" (lambda () (interactive) (insert "dashed ")) :help "next object will be drawn with a dashed line "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["dashtype" (lambda () (interactive) (insert "dashtype line_width_px,space_width_px ")) :help "default value "dashtype 2,2" e.g. 2px line and 2px space "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["delete" (lambda () (interactive) (insert "delete value")) :help "adds a button to clear the userdraw canvas with text 'value' "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["demiline" (lambda () (interactive) (insert "demiline x1,y1,x2,y2,color ")) :help "draws a halfline starting in (x1:y1) and through (x2:y2) in color 'color' (colorname or hex)
 "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["demilines" (lambda () (interactive) (insert "demilines color,x1,y1,x2,y2,.... ")) :help "draws halflines starting in (x1:y1) and through (x2:y2) in color 'color' (colorname or hex) etc etc "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["diamondfill" (lambda () (interactive) (insert "diamondfill x0,y0,dx,dy,color ")) :help "distances dx,dy in pixels "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["disk" (lambda () (interactive) (insert "disk xc,yc,width (2*r in pixels),color ")) :help "filled circle "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["disks" (lambda () (interactive) (insert "disks color,xc1,yc1,r1,xc2,yc2,r2...xc_n,yc_n,r_n ")) :help "filled circles "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["display" (lambda () (interactive) (insert "display TYPE,color,fontsize ")) :help "will display the mouse cursor coordinates. TYPE may be x | y | xy | degree | radian | radius "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["dotfill" (lambda () (interactive) (insert "dotfill x0,y0,dx,dy,color ")) :help "distances dx,dy in pixels "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["drag" (lambda () (interactive) (insert "drag [x][y][xy] ")) :help "the next object will be draggable in x / y / xy direction "])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["darrow" (lambda () (interactive) (insert "darrow x1,y1,x2,y2,l,[color] ")) :help "(Synonym: dasharrow dashedarrow) Dashed arrow (x1,y1)- - ->(x2,y2), where l is  the length (in pixels) of arrowhead."])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["dhline" (lambda () (interactive) (insert "dhline x,y,[color] ")) :help "(Synonym:   dashedhorizontalline  dashhorizontalline  hdline  horizontaldashedline) Dashed horizontal line through (x,y)."])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["dline" (lambda () (interactive) (insert "dline x1,y1,x2,y2,[color] ")) :help "(Synonym: dashedline dashline) Dashed line segment (x1,y1)---(x2,y2)."])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["dlines" (lambda () (interactive) (insert "dlines [color],x1,y1,x2,y2,x3,y3... ")) :help "(Synonym: dashedlines) dashlines n dashed line segments (x1,y1)---(x2,y2)---(x3,y3)..."])

(easy-menu-add-item oef-menu-bar '("Draw" "D")["dvline" (lambda () (interactive) (insert "dvline [x,y,[color] ")) :help "(Synonym:  dashedverticaline  dashverticalline  vdline  verticaldashedline)  Dashed vertical line through (x,y)."])

(easy-menu-add-item oef-menu-bar '("Draw" "E")["ellipse" (lambda () (interactive) (insert "ellipse xc,yc,radius_x,radius_y,color ")) :help "radius_x and radius_y are in pixels "])

(easy-menu-add-item oef-menu-bar '("Draw" "E")["erase" (lambda () (interactive) (insert "erase value ")) :help "adds a button to clear the userdraw canvas with text 'value' "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["fcircle" (lambda () (interactive) (insert "fcircle xc,yc,width (2*r in pixels),color ")) :help "alternative: disk for a filled circle "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["fcircles" (lambda () (interactive) (insert "fcircles color,xc1,yc1,r1,xc2,yc2,r2...xc_n,yc_n,r_n ")) :help "alternative command: disks for filled circles "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["fill" (lambda () (interactive) (insert "fill x,y,color ")) :help "fill the region of point (x:y) with color 'color'. \nNote: filltoborder is a very (client) CPU intensive operation! Filling is done pixel by pixel. "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["fillcolor" (lambda () (interactive) (insert "fillcolor colorname or #hex ")) :help "set the color for a filled object "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["filled" (lambda () (interactive) (insert "filled ")) :help "the next 'fillable' object (only) will be filled "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["filltoborder" (lambda () (interactive) (insert "filltoborder x,y,bordercolor,color ")) :help "fill the region of point (x:y) with color 'color'. \nNote: filltoborder is a very (client) CPU intensive operation! Filling is done pixel by pixel. "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["floodfill" (lambda () (interactive) (insert "floodfill x,y,color ")) :help "fill the region of point (x:y) with color 'color'. \nNote: floodfill is a very (client) cpu intensive operation! "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["fontcolor" (lambda () (interactive) (insert "fontcolor color(hexcolor or colorname) ")) :help "default: black "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["fontfamily" (lambda () (interactive) (insert "fontfamily 'font style' 'font size'px 'fontfamily'")) :help "like 'fontfamily italic 24px Ariel' "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["fontsize" (lambda () (interactive) (insert "fontsize int")) :help "default 12px "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["fpoly" (lambda () (interactive) (insert "fpoly color,x1,y1,x2,y2...x_n,y_n")) :help "filled polygon "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["frect" (lambda () (interactive) (insert "frect x1,y1,x2,y2,color ")) :help "filled rectangle "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["frects" (lambda () (interactive) (insert "frects color,x1,y1,x2,y2,..... ")) :help "filled rectangles "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["froundrect" (lambda () (interactive) (insert "froundrect x1,y1,x2,y2,radius in px,color ")) :help "filled rectangle "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["froundrects" (lambda () (interactive) (insert "froundrects color,radius in px,x1,y1,x2,y2,x3,y3,x4,y4,.... ")) :help "filled rectangles "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["fsquare" (lambda () (interactive) (insert "fsquare x,y,side (px) ,color ")) :help "draw a filled square with left top corner (x:y) with side 'side' in color 'color' "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["ftriangle" (lambda () (interactive) (insert " x1,y1,x2,y2,x3,y3,color ")) :help "filled triangle "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["ftriangles" (lambda () (interactive) (insert "ftriangles color,x1,y1,x2,y2,x3,y3,... ")) :help "filled triangles "])

(easy-menu-add-item oef-menu-bar '("Draw" "F")["functionlabel" (lambda () (interactive) (insert "functionlabel some_string ")) :help "define the inputfield text : default value 'f(x)=' "])

(easy-menu-add-item oef-menu-bar '("Draw" "G")["grid" (lambda () (interactive) (insert "grid step_x,step_y,gridcolor ")) :help "if keywords "axis" or "axisnumbering" are set, use : grid step_x,step_y,major_color,minor_x,minor_y,tics height in px,axis_color \nwhere minor x step = step_x / minor_x "])

(easy-menu-add-item oef-menu-bar '("Draw" "G")["gridfill" (lambda () (interactive) (insert "gridfill x0,y0,dx,dy,color ")) :help "distances dx,dy in pixels "])

(easy-menu-add-item oef-menu-bar '("Draw" "H")["halfline" (lambda () (interactive) (insert "halfline x1,y1,x2,y2,color ")) :help "draws a halfline starting in (x1:y1) and through (x2:y2) in color 'color' (colorname or hex) "])

(easy-menu-add-item oef-menu-bar '("Draw" "H")["halflines" (lambda () (interactive) (insert "halflines color,x1,y1,x2,y2,.... ")) :help "draws halflines starting in (x1:y1) and through (x2:y2) in color 'color' (colorname or hex) etc etc "])

(easy-menu-add-item oef-menu-bar '("Draw" "H")["hatchfill" (lambda () (interactive) (insert "hatchfill x0,y0,dx,dy,color ")) :help "distances dx,dy in pixels"])

(easy-menu-add-item oef-menu-bar '("Draw" "H")["highlight" (lambda () (interactive) (insert "highlight color,opacity,linewidth ")) :help "NOT IMPLEMENTED "])

(easy-menu-add-item oef-menu-bar '("Draw" "H")["hline" (lambda () (interactive) (insert "hline x,y,color ")) :help "draw a horizontal line through point (x:y) in color 'color' "])

(easy-menu-add-item oef-menu-bar '("Draw" "H")["hlines" (lambda () (interactive) (insert "hlines color,x1,y1,x2,y2,... ")) :help "draw horizontal lines through points (x1:y1)...(xn:yn) in color 'color' "])

(easy-menu-add-item oef-menu-bar '("Draw" "H")["horizontalline" (lambda () (interactive) (insert "horizontalline x,y,color ")) :help "draw a horizontal line through point (x:y) in color 'color' "])

(easy-menu-add-item oef-menu-bar '("Draw" "H")["horizontallines" (lambda () (interactive) (insert "horizontallines color,x1,y1,x2,y2,... ")) :help "draw horizontal lines through points (x1:y1)...(xn:yn) in color 'color' "])

(easy-menu-add-item oef-menu-bar '("Draw" "H")["html" (lambda () (interactive) (insert "html x1,y1,x2,y2,html_string ")) :help "all tags are allowed "])

(easy-menu-add-item oef-menu-bar '("Draw" "H")["http" (lambda () (interactive) (insert "http x1,y1,x2,y2,http://some_adress.com ")) :help "an active html-page will be displayed in an "iframe" rectangle left top (x1:y1) , right bottom (x2:y2) "])

(easy-menu-add-item oef-menu-bar '("Draw" "I")["imagefill" (lambda () (interactive) (insert "imagefill dx,dy,image_url ")) :help "The next suitable filled object will be filled with 'image_url' tiled. \nIf dx,dy is larger than the image, the whole image will be background to the next object. "])

(easy-menu-add-item oef-menu-bar '("Draw" "I")["input" (lambda () (interactive) (insert "input x,y,size,editable,value ")) :help "only active inputfields (editable = 1) will be read with read_canvas() "])

(easy-menu-add-item oef-menu-bar '("Draw" "I")["inputstyle" (lambda () (interactive) (insert "inputstyle style_description ")) :help "example: inputstyle color:blue;font-weight:bold;font-style:italic;font-size:16pt "])

(easy-menu-add-item oef-menu-bar '("Draw" "I")["intooltip" (lambda () (interactive) (insert "intooltip link_text ")) :help "link_text is a single line (span-element) "])

(easy-menu-add-item oef-menu-bar '("Draw" "J")["jscurve" (lambda () (interactive) (insert "jscurve color,formula(x) ")) :help "use only basic math in your curve: sqrt,^,asin,acos,atan,log,pi,abs,sin,cos,tan,e "])

(easy-menu-add-item oef-menu-bar '("Draw" "J")["jsmath" (lambda () (interactive) (insert "jsmath formula(x) ")) :help "use command 'jsmath formula(x)` for calculating and displaying indiviual points on the curve "])

(easy-menu-add-item oef-menu-bar '("Draw" "J")["jsplot" (lambda () (interactive) (insert "jsplot color,formula(x) ")) :help "use only basic math in your curve: sqrt,^,asin,acos,atan,log,pi,abs,sin,cos,tan,e "])

(easy-menu-add-item oef-menu-bar '("Draw" "K")["killaffine" (lambda () (interactive) (insert "killaffine ")) :help "resets the transformation matrix to 1,0,0,1,0,0 "])

(easy-menu-add-item oef-menu-bar '("Draw" "K")["killrotate" (lambda () (interactive) (insert "killrotate ")) :help "will reset the command rotationcenter xc,yc. \nA following rotate command will have the first object point as rotation center "])

(easy-menu-add-item oef-menu-bar '("Draw" "K")["killslider" (lambda () (interactive) (insert "killslider ")) :help "ends grouping of object under a previously defined slider "])

(easy-menu-add-item oef-menu-bar '("Draw" "K")["killtranslate" (lambda () (interactive) (insert "killtranslate ")) :help "resets the translation matrix to 1,0,0,1,0,0 "])
(easy-menu-add-item oef-menu-bar '("Draw" "K")["killtranslation" (lambda () (interactive) (insert "killtranslation ")) :help "resets the translation matrix to 1,0,0,1,0,0 "])

(easy-menu-add-item oef-menu-bar '("Draw" "L")["lattice" (lambda () (interactive) (insert "lattice x0,y0,xv1,yv1,xv2,yv2,n1,n2,color ")) :help "A lattice of n1xn2 points starting with (x0,y0), with n1 rows in direction of (xv1,yv1) and n2 rows in direction of (xv2,yv2). "])

(easy-menu-add-item oef-menu-bar '("Draw" "L")["legend" (lambda () (interactive) (insert "legend ")) :help "Sorry. There is no help for legend "])
(easy-menu-add-item oef-menu-bar '("Draw" "L")["legendcolors" (lambda () (interactive) (insert "legendcolors ")) :help "Sorry. There is no help for legendcolors "])
(easy-menu-add-item oef-menu-bar '("Draw" "L")["levelcurve" (lambda () (interactive) (insert "levelcurve ")) :help "Sorry. There is no help for levelcurve "])
(easy-menu-add-item oef-menu-bar '("Draw" "L")["line" (lambda () (interactive) (insert "line ")) :help "Sorry. There is no help for line "])
(easy-menu-add-item oef-menu-bar '("Draw" "L")["linegraph" (lambda () (interactive) (insert "linegraph ")) :help "Sorry. There is no help for linegraph "])
(easy-menu-add-item oef-menu-bar '("Draw" "L")["lines" (lambda () (interactive) (insert "lines ")) :help "Sorry. There is no help for lines "])
(easy-menu-add-item oef-menu-bar '("Draw" "L")["linewidth" (lambda () (interactive) (insert "linewidth ")) :help "Sorry. There is no help for linewidth "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["mathml" (lambda () (interactive) (insert "mathml x1,y1,x2,y2,mathml_string")) :help "mathml will be displayed in a rectangle left top (x1:y1) , right bottom (x2:y2) "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["mouse" (lambda () (interactive) (insert "mouse ")) :help "Sorry. There is no help for mouse "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["mouse_degree" (lambda () (interactive) (insert "mouse_degree ")) :help "Sorry. There is no help for mouse_degree "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["mousex" (lambda () (interactive) (insert "mousex ")) :help "Sorry. There is no help for mousex "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["mousey" (lambda () (interactive) (insert "mousey ")) :help "Sorry. There is no help for mousey "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["multidash" (lambda () (interactive) (insert "multidash ")) :help "Sorry. There is no help for multidash "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["multidraw" (lambda () (interactive) (insert "multidraw ")) :help "Sorry. There is no help for multidraw "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["multifill" (lambda () (interactive) (insert "multifill ")) :help "Sorry. There is no help for multifill "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["multifillcolors" (lambda () (interactive) (insert "multifillcolors ")) :help "Sorry. There is no help for multifillcolors "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["multifillopacity" (lambda () (interactive) (insert "multifillopacity ")) :help "Sorry. There is no help for multifillopacity "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["multilabel" (lambda () (interactive) (insert "multilabel ")) :help "Sorry. There is no help for multilabel "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["multilinewidth" (lambda () (interactive) (insert "multilinewidth ")) :help "Sorry. There is no help for multilinewidth "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["multisnaptogrid" (lambda () (interactive) (insert "multisnaptogrid ")) :help "Sorry. There is no help for multisnaptogrid "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["multistrokecolors" (lambda () (interactive) (insert "multistrokecolors ")) :help "Sorry. There is no help for multistrokecolors "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["multistrokeopacity" (lambda () (interactive) (insert "multistrokeopacity ")) :help "Sorry. There is no help for multistrokeopacity "])
(easy-menu-add-item oef-menu-bar '("Draw" "M")["multiuserinput" (lambda () (interactive) (insert "multiuserinput ")) :help "Sorry. There is no help for multiuserinput "])
(easy-menu-add-item oef-menu-bar '("Draw" "N")["noaxis" (lambda () (interactive) (insert "noaxis ")) :help "Sorry. There is no help for noaxis "])
(easy-menu-add-item oef-menu-bar '("Draw" "N")["noayis" (lambda () (interactive) (insert "noayis ")) :help "Sorry. There is no help for noayis "])
(easy-menu-add-item oef-menu-bar '("Draw" "N")["note" (lambda () (interactive) (insert "note ")) :help "Sorry. There is no help for note "])
(easy-menu-add-item oef-menu-bar '("Draw" "O")["onclick" (lambda () (interactive) (insert "onclick ")) :help "Sorry. There is no help for onclick "])
(easy-menu-add-item oef-menu-bar '("Draw" "O")["opacity" (lambda () (interactive) (insert "opacity ")) :help "Sorry. There is no help for opacity "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["parallel" (lambda () (interactive) (insert "parallel ")) :help "Sorry. There is no help for parallel "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["piechart" (lambda () (interactive) (insert "piechart ")) :help "Sorry. There is no help for piechart "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["pixels" (lambda () (interactive) (insert "pixels ")) :help "Sorry. There is no help for pixels "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["pixelsize" (lambda () (interactive) (insert "pixelsize ")) :help "Sorry. There is no help for pixelsize "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["plotsteps" (lambda () (interactive) (insert "plotsteps ")) :help "Sorry. There is no help for plotsteps "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["point" (lambda () (interactive) (insert "point x,y,color ")) :help "draw a single point at (x;y) in color 'color' "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["pointer" (lambda () (interactive) (insert "pointer ")) :help "Sorry. There is no help for pointer "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["points" (lambda () (interactive) (insert "points color,x1,y1,x2,y2,...,x_n,y_n ")) :help "draw multiple points at given coordinates in color 'color' "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["poly" (lambda () (interactive) (insert "poly ")) :help "Sorry. There is no help for poly "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["polyline" (lambda () (interactive) (insert "polyline ")) :help "Sorry. There is no help for polyline "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["popup" (lambda () (interactive) (insert "popup ")) :help "Sorry. There is no help for popup "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["precision" (lambda () (interactive) (insert "precision ")) :help "Sorry. There is no help for precision "])
(easy-menu-add-item oef-menu-bar '("Draw" "P")["protractor" (lambda () (interactive) (insert "protractor ")) :help "Sorry. There is no help for protractor "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["ranget" (lambda () (interactive) (insert "ranget ")) :help "Sorry. There is no help for ranget "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["rangex" (lambda () (interactive) (insert "rangex ")) :help "Sorry. There is no help for rangex "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["rangey" (lambda () (interactive) (insert "rangey ")) :help "Sorry. There is no help for rangey "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["rays" (lambda () (interactive) (insert "rays ")) :help "Sorry. There is no help for rays "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["rect" (lambda () (interactive) (insert "rect ")) :help "Sorry. There is no help for rect "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["rects" (lambda () (interactive) (insert "rects ")) :help "Sorry. There is no help for rects "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["replyformat" (lambda () (interactive) (insert "replyformat ")) :help "Sorry. There is no help for replyformat "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["rotate" (lambda () (interactive) (insert "rotate ")) :help "Sorry. There is no help for rotate "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["rotationcenter" (lambda () (interactive) (insert "rotationcenter ")) :help "Sorry. There is no help for rotationcenter "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["roundrect" (lambda () (interactive) (insert "roundrect ")) :help "Sorry. There is no help for roundrect "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["roundrects" (lambda () (interactive) (insert "roundrects ")) :help "Sorry. There is no help for roundrects "])
(easy-menu-add-item oef-menu-bar '("Draw" "R")["ruler" (lambda () (interactive) (insert "ruler ")) :help "Sorry. There is no help for ruler "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["seg" (lambda () (interactive) (insert "seg ")) :help "Sorry. There is no help for seg "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["segment" (lambda () (interactive) (insert "segment ")) :help "Sorry. There is no help for segment "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["segments" (lambda () (interactive) (insert "segments ")) :help "Sorry. There is no help for segments "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["segs" (lambda () (interactive) (insert "segs ")) :help "Sorry. There is no help for segs "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["setlimits" (lambda () (interactive) (insert "setlimits ")) :help "Sorry. There is no help for setlimits "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["setpixel" (lambda () (interactive) (insert "setpixel ")) :help "Sorry. There is no help for setpixel "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["sgraph" (lambda () (interactive) (insert "sgraph ")) :help "Sorry. There is no help for sgraph "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["size" (lambda () (interactive) (insert "size ")) :help "Sorry. There is no help for size "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["slider" (lambda () (interactive) (insert "slider ")) :help "Sorry. There is no help for slider "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["sliderfunction_x" (lambda () (interactive) (insert "sliderfunction_x ")) :help "Sorry. There is no help for sliderfunction_x "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["sliderfunction_y" (lambda () (interactive) (insert "sliderfunction_y ")) :help "Sorry. There is no help for sliderfunction_y "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["snaptofun" (lambda () (interactive) (insert "snaptofun ")) :help "Sorry. There is no help for snaptofun "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["snaptofunction" (lambda () (interactive) (insert "snaptofunction ")) :help "Sorry. There is no help for snaptofunction "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["snaptogrid" (lambda () (interactive) (insert "snaptogrid ")) :help "Sorry. There is no help for snaptogrid "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["snaptopoints" (lambda () (interactive) (insert "snaptopoints ")) :help "Sorry. There is no help for snaptopoints "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["square" (lambda () (interactive) (insert "square ")) :help "Sorry. There is no help for square "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["status" (lambda () (interactive) (insert "status ")) :help "Sorry. There is no help for status "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["string" (lambda () (interactive) (insert "string color,x,y,the text string ")) :help "Sorry. There is no help for string "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["stringup" (lambda () (interactive) (insert "stringup ")) :help "Sorry. There is no help for stringup "])
(easy-menu-add-item oef-menu-bar '("Draw" "S")["strokecolor" (lambda () (interactive) (insert "strokecolor ")) :help "Sorry. There is no help for strokecolor "])
(easy-menu-add-item oef-menu-bar '("Draw" "T")["text" (lambda () (interactive) (insert "text fontcolor,x,y,font,text_string")) :help "font may be described by keywords : giant,huge,normal,small,tiny"])
(easy-menu-add-item oef-menu-bar '("Draw" "T")["textarea" (lambda () (interactive) (insert "textarea ")) :help "Sorry. There is no help for textarea "])
(easy-menu-add-item oef-menu-bar '("Draw" "T")["textup" (lambda () (interactive) (insert "textup ")) :help "Sorry. There is no help for textup "])
(easy-menu-add-item oef-menu-bar '("Draw" "T")["trace_jscurve" (lambda () (interactive) (insert "trace_jscurve ")) :help "Sorry. There is no help for trace_jscurve "])
(easy-menu-add-item oef-menu-bar '("Draw" "T")["trange" (lambda () (interactive) (insert "trange ")) :help "Sorry. There is no help for trange "])
(easy-menu-add-item oef-menu-bar '("Draw" "T")["translate" (lambda () (interactive) (insert "translate ")) :help "Sorry. There is no help for translate "])
(easy-menu-add-item oef-menu-bar '("Draw" "T")["translation" (lambda () (interactive) (insert "translation ")) :help "Sorry. There is no help for translation "])
(easy-menu-add-item oef-menu-bar '("Draw" "T")["transparent" (lambda () (interactive) (insert "transparent ")) :help "Sorry. There is no help for transparent "])
(easy-menu-add-item oef-menu-bar '("Draw" "T")["triangle" (lambda () (interactive) (insert "triangle ")) :help "Sorry. There is no help for triangle "])
(easy-menu-add-item oef-menu-bar '("Draw" "T")["triangles" (lambda () (interactive) (insert "triangles ")) :help "Sorry. There is no help for triangles "])
(easy-menu-add-item oef-menu-bar '("Draw" "U")["userboxplot" (lambda () (interactive) (insert "userboxplot ")) :help "Sorry. There is no help for userboxplot "])
(easy-menu-add-item oef-menu-bar '("Draw" "U")["userboxplotdata" (lambda () (interactive) (insert "userboxplotdata ")) :help "Sorry. There is no help for userboxplotdata "])
(easy-menu-add-item oef-menu-bar '("Draw" "U")["userdraw" (lambda () (interactive) (insert "userdraw ")) :help "Sorry. There is no help for userdraw "])
(easy-menu-add-item oef-menu-bar '("Draw" "U")["userinput" (lambda () (interactive) (insert "userinput ")) :help "Sorry. There is no help for userinput "])
(easy-menu-add-item oef-menu-bar '("Draw" "U")["userinput_function" (lambda () (interactive) (insert "userinput_function ")) :help "Sorry. There is no help for userinput_function "])
(easy-menu-add-item oef-menu-bar '("Draw" "U")["userinput_textarea" (lambda () (interactive) (insert "userinput_textarea ")) :help "Sorry. There is no help for userinput_textarea "])
(easy-menu-add-item oef-menu-bar '("Draw" "U")["userinput_xy" (lambda () (interactive) (insert "userinput_xy ")) :help "Sorry. There is no help for userinput_xy "])
(easy-menu-add-item oef-menu-bar '("Draw" "U")["usertextarea_xy" (lambda () (interactive) (insert "usertextarea_xy ")) :help "Sorry. There is no help for usertextarea_xy "])
(easy-menu-add-item oef-menu-bar '("Draw" "V")["vector" (lambda () (interactive) (insert "vector ")) :help "Sorry. There is no help for vector "])
(easy-menu-add-item oef-menu-bar '("Draw" "V")["vectors" (lambda () (interactive) (insert "vectors ")) :help "Sorry. There is no help for vectors "])
(easy-menu-add-item oef-menu-bar '("Draw" "V")["verticalline" (lambda () (interactive) (insert "verticalline ")) :help "Sorry. There is no help for verticalline "])
(easy-menu-add-item oef-menu-bar '("Draw" "V")["verticallines" (lambda () (interactive) (insert "verticallines ")) :help "Sorry. There is no help for verticallines "])
(easy-menu-add-item oef-menu-bar '("Draw" "V")["video" (lambda () (interactive) (insert "video ")) :help "Sorry. There is no help for video "])
(easy-menu-add-item oef-menu-bar '("Draw" "V")["vline" (lambda () (interactive) (insert "vline ")) :help "Sorry. There is no help for vline "])
(easy-menu-add-item oef-menu-bar '("Draw" "V")["vlines" (lambda () (interactive) (insert "vlines ")) :help "Sorry. There is no help for vlines "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xaxis" (lambda () (interactive) (insert "xaxis ")) :help "Sorry. There is no help for xaxis "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xaxistext" (lambda () (interactive) (insert "xaxistext ")) :help "Sorry. There is no help for xaxistext "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xaxistextup" (lambda () (interactive) (insert "xaxistextup ")) :help "Sorry. There is no help for xaxistextup "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xaxisup" (lambda () (interactive) (insert "xaxisup ")) :help "Sorry. There is no help for xaxisup "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xerrorbars" (lambda () (interactive) (insert "xerrorbars ")) :help "Sorry. There is no help for xerrorbars "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xlabel" (lambda () (interactive) (insert "xlabel ")) :help "Sorry. There is no help for xlabel "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xlogbase" (lambda () (interactive) (insert "xlogbase ")) :help "Sorry. There is no help for xlogbase "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xlogscale" (lambda () (interactive) (insert "xlogscale ")) :help "Sorry. There is no help for xlogscale "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xrange" (lambda () (interactive) (insert "xrange ")) :help "Sorry. There is no help for xrange "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xsnaptogrid" (lambda () (interactive) (insert "xsnaptogrid ")) :help "Sorry. There is no help for xsnaptogrid "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xunit" (lambda () (interactive) (insert "xunit ")) :help "Sorry. There is no help for xunit "])
(easy-menu-add-item oef-menu-bar '("Draw" "X")["xylogscale" (lambda () (interactive) (insert "xylogscale ")) :help "Sorry. There is no help for xylogscale "])
(easy-menu-add-item oef-menu-bar '("Draw" "Y")["yaxis" (lambda () (interactive) (insert "yaxis ")) :help "Sorry. There is no help for yaxis "])
(easy-menu-add-item oef-menu-bar '("Draw" "Y")["yerrorbars" (lambda () (interactive) (insert "yerrorbars ")) :help "Sorry. There is no help for yerrorbars "])
(easy-menu-add-item oef-menu-bar '("Draw" "Y")["ylabel" (lambda () (interactive) (insert "ylabel ")) :help "Sorry. There is no help for ylabel "])
(easy-menu-add-item oef-menu-bar '("Draw" "Y")["ylogbase" (lambda () (interactive) (insert "ylogbase ")) :help "Sorry. There is no help for ylogbase "])
(easy-menu-add-item oef-menu-bar '("Draw" "Y")["ylogscale" (lambda () (interactive) (insert "ylogscale ")) :help "Sorry. There is no help for ylogscale "])
(easy-menu-add-item oef-menu-bar '("Draw" "Y")["yrange" (lambda () (interactive) (insert "yrange ")) :help "Sorry. There is no help for yrange "])
(easy-menu-add-item oef-menu-bar '("Draw" "Y")["ysnaptogrid" (lambda () (interactive) (insert "ysnaptogrid ")) :help "Sorry. There is no help for ysnaptogrid "])
(easy-menu-add-item oef-menu-bar '("Draw" "Y")["yunit" (lambda () (interactive) (insert "yunit ")) :help "Sorry. There is no help for yunit "])
(easy-menu-add-item oef-menu-bar '("Draw" "Z")["zoom" (lambda () (interactive) (insert "zoom ")) :help "Sorry. There is no help for zoom "])

(add-hook 'menu-bar-update-hook 'oef-update-menu) ; 


;;----------------COMPANY BACKEND-----------------------------

(defvar oef-completions '()
  "The content of this variable is a list of keyphrases (keywords...).  It's generated automatically for COMPANY completion." 
  )

(defvar oef-grabed-word nil
  "The content of this variable is get before point by company-mode."
  )

(defvar oef-candidats nil
  "List of candidats.  Made of Three first letters of items in oef-completion."
  )

(setq oef-completions
      (append  oef-answers-options
	       oef-definitions-commands
	       oef-commands
	       oef-definitions-special-commands
	       oef-doc-commands
	       oef-storage-types
	       oef-menu-exo-init-types
	       oef-menu-doc-init-types
	       oef-defined-variables
	       oef-comparison-operators
	       oef-language-reserved-words
	       oef-definitions-wims-functions
	       oef-wims-functions
	       oef-definitions-slib-scripts
	       oef-slib-scripts
	       oef-pari-functions
	       oef-maths-functions
	       oef-random-functions
	       oef-draw-commands))

(defun oef-make-candidats()
  "Make a list of candidats (3 first characters) to detect if oef-mode-backend is required."
  (interactive)
  (let ((mylist oef-completions)(myword))
    (while mylist
      (setq myword (pop mylist))
      (if (> (length myword) 2)
	  (add-to-list 'oef-candidats (substring myword 0 3))
	))))

(defun company-oef-mode-backend (command &optional arg &rest ignored)
  "Detect if company-oef-mode-bakend is required.  If yes company-oef-mode-backend will sugest competions.  If not the next backend is called."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-oef-mode-backend))
    (prefix
     (setq oef-grabed-word (company-grab-word))
     (if (and (eq major-mode 'oef-mode)
	      (member
	       oef-grabed-word
	       oef-candidats
	       ))
	 oef-grabed-word
       nil)
     )		
    (candidates
     (remove-if-not
      (lambda (c) (string-prefix-p arg c))
      oef-completions))
    )
  )

;;-----------MAJOR MODE----------------------------------------
;;;###autoload
(define-derived-mode oef-mode sgml-mode
  "oef-mode"
  "'Online Exercise Format' mode"
  (mapc
   (lambda (package)
     (or (package-installed-p package)
	 (package-install package)))
   '(emmet-mode company rainbow-delimiters rainbow-mode yafolding expand-region))

  (setq-local indent-line-function 'oef-mode-indent-line)
  (setq-local line-spacing oef-line-spacing)
  (if (string= (frame-parameter nil 'background-mode) "light") ; test if the background is light (or dark)
      (progn      ; if the background is light
        (set-face-attribute 'oef-font-h1text-face nil :inherit 'oef-font-h1text-lightbg-face)
        (set-face-attribute 'oef-font-h2text-face nil :inherit 'oef-font-h2text-lightbg-face))
    (progn    ; if the background is dark
      (set-face-attribute 'oef-font-h1text-face nil :inherit 'oef-font-h1text-darkbg-face)
      (set-face-attribute 'oef-font-h2text-face nil :inherit 'oef-font-h2text-darkbg-face)))
  (oef-make-candidats) ; candidats for completions with company-oef-mode-backend
  (add-to-list 'company-backends 'company-oef-mode-backend)
  
  ;; key binding
  (define-key oef-mode-map (kbd "/") nil) ; to have forward-slash with multiple-cursors
  (define-key oef-mode-map (kbd "C-C C-e") 'oef-close-tag)
  (define-key oef-mode-map (kbd "M-[") 'insert-pair)
  (define-key oef-mode-map (kbd "M-{") 'insert-pair)
  (define-key oef-mode-map (kbd "M-\"") 'insert-pair)
  (define-key oef-mode-map (kbd "M-RET") 'electric-newline-and-maybe-indent)
  (define-key oef-mode-map (kbd "M--") 'oef-insert-endash)                   ; —
  (define-key oef-mode-map (kbd "C-<") 'oef-insert-french-opening-guillemet) ; french «
  (define-key oef-mode-map (kbd "C->") 'oef-insert-french-closing-guillemet) ; french »
  (define-key oef-mode-map (kbd "C-M-<") 'oef-insert-french-guillemets)      ; french «»
  (define-key oef-mode-map (kbd "C-M-SPC") 'oef-insert-non-breaking-space)
  (define-key oef-mode-map (kbd "TAB") 'oef-mode-indent-line)
  (define-key oef-mode-map (kbd "M-g a") 'oef-goto-answers)
  (define-key oef-mode-map (kbd "M-g c") 'oef-goto-css)  
  (define-key oef-mode-map (kbd "M-g r") 'oef-goto-reply)  
  (define-key oef-mode-map (kbd "M-g s") 'oef-goto-statement)
  (define-key oef-mode-map (kbd "C-:") 'indent-region) ; alias for indent-region because C-M-\ is not working with a french keyboard on osx
  (define-key oef-mode-map (kbd "C-o") nil) ;
  (define-key oef-mode-map (kbd "C-o f") 'flyspell-mode) ;  
  (define-key oef-mode-map (kbd "C-o n") 'oef-new-exercise) ;
  (define-key oef-mode-map (kbd "C-o vc") 'oef-variable-complex) ;    
  (define-key oef-mode-map (kbd "C-o vf") 'oef-variable-function) ;  
  (define-key oef-mode-map (kbd "C-o vi") 'oef-variable-integer) ;            
  (define-key oef-mode-map (kbd "C-o vm") 'oef-variable-matrix) ;          
  (define-key oef-mode-map (kbd "C-o vq") 'oef-variable-rational) ;        
  (define-key oef-mode-map (kbd "C-o vr") 'oef-variable-real) ;      
  (define-key oef-mode-map (kbd "C-o vt") 'oef-variable-text) ;    
  (define-key oef-mode-map (kbd "C-o tt") 'yafolding-toggle-element)
  (define-key oef-mode-map (kbd "C-o ts") 'yafolding-show-all)
  (define-key oef-mode-map (kbd "C-o th") 'yafolding-hide-all)
  (define-key oef-mode-map (kbd "C-o C-p") 'oef-select-parameter) ;
  (define-key oef-mode-map (kbd "C-o m") 'oef-insert-math) ;
  (define-key oef-mode-map (kbd "C-o tp") 'oef-mode-mark-sgml-tag-pair) ;
  (define-key oef-mode-map (kbd "C-o ti") 'er/mark-inner-tag) ;  
  (define-key oef-mode-map (kbd "C-o tb") 'oef-insert-tag-b) ;  
  (define-key oef-mode-map (kbd "C-o tm") 'oef-insert-tag-mark) ;      
  (define-key oef-mode-map (kbd "C-o t_") 'oef-insert-tag-sub) ;      
  (define-key oef-mode-map (kbd "C-o t^") 'oef-insert-tag-sup) ;
  (define-key oef-mode-map (kbd "C-o tc") 'oef-insert-tag-code) ;  
  (define-key oef-mode-map (kbd "C-o s") 'oef-insert-statement) ;  
  (define-key oef-mode-map (kbd "C-o a") 'oef-insert-answer) ;
  (define-key oef-mode-map (kbd "C-o er") 'oef-insert-embed-reply) ;  
  (define-key oef-mode-map (kbd "C-o ;") 'oef-comment-toggle) ;
  (define-key oef-mode-map (kbd "C-o ws") 'oef-get-wims-session) ;
  (define-key oef-mode-map (kbd "C-o C-o") 'oef-highlight-variable) ;
  (define-key oef-mode-map (kbd "C-o ee") 'oef-edit-exercise-in-browser) ;
  (define-key oef-mode-map (kbd "C-o ed") 'oef-edit-document-in-browser) ;
  (define-key oef-mode-map (kbd "C-c C-c") 'oef-edit-in-browser) ;
  (define-key oef-mode-map (kbd "<down-mouse-1>") ;  (de)highlighting the variable on mouse click and add the variable to completion list
    (lambda (event)
      "Toogle `oef-variable' highlighting on mouse click"
      (interactive "e")
					;      (message "%s" event)
      (let ((posn (elt event 1)))
	(with-selected-window (posn-window posn)
	  (goto-char (posn-point posn))
	  (oef-highlight-variable)))))

  ;; Warning: Major mode commands must not call font-lock-add-keywords under any
  ;; circumstances, either directly or indirectly, except through their mode hooks. (Doing
  ;; so would lead to incorrect behavior for some minor modes.) They should set up their
  ;; rules for search-based fontification by setting font-lock-keywords.
  ;; So the following code may cause troubles.

  (font-lock-add-keywords
   nil
   `(
     ("\\\\comment{.*}" . 'oef-font-comment-face) ; comments
     ("^[:blank:]*#.*" . 'oef-font-comment-face) ; comments
     ("^[:blank:]*:%%.*" . 'oef-font-comment-face) ; comments
     ("«[^»]*\n?[^»]*»" . 'oef-font-documentation-face) ; documentation
     ("<\\(h1\\)\\( \\(class\\|id\\) ?=.*\\)?>\\(.+\\)<\\(/h1\\)>" (1 'oef-font-htag-face)(4 'oef-font-h1text-face)(5 'oef-font-htag-face)) ; sections
     ("<\\(h2\\)\\( \\(class\\|id\\) ?=.*\\)?>\\(.+\\)<\\(/h2\\)>" (1 'oef-font-htag-face)(4 'oef-font-h2text-face)(5 'oef-font-htag-face)) ; sub-sections
     ("<\\(h3\\)\\( \\(class\\|id\\) ?=.*\\)?>\\(.+\\)<\\(/h3\\)>" (1 'oef-font-htag-face)(4 'oef-font-h3text-face)(5 'oef-font-htag-face)) ; sub-sections

     (,(regexp-opt oef-these-phrases-are-made-of-words-not-keywords 'words) . 'default)
					;     ("^ *<\\(li\\)>.*?</\\(li\\)> *$"(1 'oef-font-litag-face)(2 'oef-font-litag-face)) ; <li> </li>
     ("<\\(li\\)[^>]*>"(1 'oef-font-litag-face)) ; <li>
     ("</\\(li\\)>"(1 'oef-font-litag-face)) ;  </li>
     ("wims\\s(\\(for\\) " 1 'oef-font-keyword-face) ; exception (for is a wims function not only a oef-doc-command)
     (,(regexp-opt oef-comparison-operators 'symbols) . 'oef-font-keyword-face)
     ("{[^}^{]*\\(>\\|<\\|!=\\)[^{]+}" 1 'oef-font-keyword-face) ;  "<" ">" "!=" comparison (must be after the precedent line)
     ;; There are text properties here: (face oef-font-keyword-face fontified t) see describe-char
     ("\\(real\\|complex\\|text\\|integer\\|rational\\|function\\|matrix\\){\\\\\\w* ?=" . 'oef-font-warning-face) ; warning '\varName=' instead of 'varName='
     (,(regexp-opt oef-slib-scripts 'words) . 'oef-font-keyword-face) ; slib scripts (some scripts stats with text/)
     ("^text " . 'oef-font-draw-face)     ; text is also a draw command      
     (,(regexp-opt oef-storage-types 'words) . 'oef-font-type-face) ; types : text, integer, real...
     ("\\(^\\\\statement{\\)\\(.*\\)\\(}\\)" (1 'oef-font-statement-command-face)(3 'oef-font-statement-command-face)) ; command statement just one line    
     ("^\\\\statement{" . 'oef-font-statement-command-face) ; command statement if multiple lines
     ("^}[:newline:]* *$". 'oef-font-statement-command-face) ; command statement if multiple lines
     ("^\\\\answer{[^}]*}" . 'oef-font-answer-command-face) ; command answer
     ("^\\\\hint{[^}]*}" . 'oef-font-hint-command-face) ; command hint
     (,(regexp-opt oef-commands 'words) . 'oef-font-command-face) ; other oef-commands : embed...
     (,(regexp-opt oef-doc-commands 'words) . 'oef-font-command-face) ;  oef-doc-commands : def... (for is a oef-doc-command)
     ("\\(\\\\special\\){[ \\\n]*\\(expandlines\\|imagefill\\|help\\|tabs2lines\\|rename\\|tooltip\\|codeinput\\|imageinput\\|mathmlinput\\|drawinput\\)" (1 'oef-font-function-name-face)(2 'oef-font-keyword-face)) ; special OEF
     ("\\\\\\(for\\|if\\|else\\) *{" 1 'oef-font-control-face)	     ;controls
     (,(regexp-opt oef-language-reserved-words 'words) . 'oef-font-keyword-face)
     (,(regexp-opt oef-answers-options 'symbols) . 'oef-font-answer-type-face)
     ("{weight=[0-9]?[0-9]?}" . 'oef-font-answer-type-face)
     (,(regexp-opt oef-defined-variables 'words) . 'oef-font-variable-name-face)
     (,(regexp-opt oef-wims-functions 'words) . 'oef-font-keyword-face)
     (,(regexp-opt oef-pari-functions 'words) . 'oef-font-keyword-face)
     (,(regexp-opt oef-maths-functions 'words) . 'oef-font-keyword-face)
     (,(regexp-opt oef-random-functions 'words) . 'oef-font-keyword-face)
     ("\\(\\w*\\)\\(pari\\|maxima\\|yacas\\|wims\\|draw\\|slib\\|teximg)\\)(" 2 'oef-font-keyword-face) ; advanced functions
     ("\\(\\\\\\w+\\){" 1 'oef-font-warning-face) ; unknown '\command{'
     ("\\(\\\\(\\)\\([^ ]*\\)\\(\\\\)\\)" (1 'oef-font-formula-braces-face)(3 'oef-font-formula-braces-face)) ;  \(mathematical formula\)
     ("<mark>\\([^>]*\\)</mark>" (1 'oef-font-mark-face)) ;  <mark></mark>
     ("\\(^ *<p class=\"mark\">\\)" (1 'oef-font-mark-face)) ;  <p class="mark"></p>
     ("\\(\\\\\\){" 1 'oef-font-positivenumber-face) ; latex expression \{}
     ("\\\\\\w+\\([0-9]?_?\\w?\\)*" . 'oef-font-variable-name-face) ; '\variable'
     (,(regexp-opt oef-draw-commands 'words) . 'oef-font-draw-face)
     ("[^\\w]\\([0-9]+\\(\\.[0-9]+\\)?\\)" 1 'oef-font-positivenumber-face) ; a number
     ("=" . 'oef-font-equal-face) ; equal sign
     )
   )
  )

;;---- AUTO-ACTIVATION of Mode When Opening File -------------------------------

(add-to-list 'auto-mode-alist '("\\.oef?\\'" . oef-mode)) ;wims file

(provide 'oef-mode)
;;; oef-mode.el ends here
