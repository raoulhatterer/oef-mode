;;; oef-mode.el --- major mode for editing oef templates
;;; -*- coding: utf-8 -*-

;; Copyright 2017-20.. Raoul HATTERER

;; Version: 1.0.0
;; Package-Version: 
;; Author: Raoul Hatterer <hatterer.raoul AT gmail.com>
;; Maintainer: Raoul HATTERER
;; Package-Requires: ((emacs "24")(rainbow-mode))
;; URL: http://http://wims.unice.fr
;; Repository: http://github.com/raoulhatterer/oef
;; Created: July 2017
;; Keywords: languages oef wims 'multiple choice'
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
;; witch should have ".oef" or ".cgi" extension to be recognized. 	
;;==============================================================================

;;; manually installation:

;;==============================================================================
;; This section is a tutorial on how to install oef-mode emacs package manually. 
;; First method for trying: "Load the File Manually"
;; To use the package, all you have to do is to make emacs load the file 'oef.el'.
;; alt+x load-file then give the file path.
;; Now, emacs is aware of the package. To activate, call “oef-mode” (with alt+x).
;; Other method: "Load File at Startup"
;; * emacs (Linux):                                   
;; If you want emacs to load the file 'oef.el' when it starts, put the file 'oef.el'
;; in the dir "~/.emacs.d/lisp/", (create that directory if it doesn't exist).
;; By convention, the dir ~/.emacs.d/lisp/ is for packages you manually installed. 
;; Then put the following (without ;;) in your emacs init file "~/.emacs"
;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;;  (add-to-list 'load-path "~/.emacs.d/lisp/") ;; Tell emacs where is your personal elisp lib dir
;;  (load "oef") ;; load the packaged named oef (best not to include the ending “.el” or “.elc”)
;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; before the line (package-initialize).
;; * aquamacs (OSX):
;; If you want aquamacs to load the file 'oef.el' when it starts, put the file 'oef.el'
;; in the dir "~/Library/Application Support/Aquamacs Emacs/myPlugin"
;; (create that directory if it doesn't exist).
;; Then put the following (without ;;) in your aquamacs init file
;; "~/Library/Preferences/Aquamacs Emacs/Preferences.el"
;;  ~/.emacs  (deprecated -- meaning 'should not be used for new installations,
;; but will continue to be supported' -- in Aquamacs on OS X)
;;vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;;  (load "oef") ;; load the packaged named oef (best not to include the ending “.el” or “.elc”)
;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; before the line (package-initialize).
;; How to Debug Aquamacs if you need to:
;; Past the following command in a terminal:
;; /Applications/Aquamacs.app/Contents/MacOS/Aquamacs -nw --debug-init

;;==============================================================================

;;;; customize

;;==============================================================================
;; alt+x customize, then search for oef
;;==============================================================================

;;; Code:
(unless (featurep 'aquamacs); subdir inclusion so oef dir will be included to emacs load path
  (let ((default-directory  "~/.emacs.d/"))
    (normal-top-level-add-subdirs-to-load-path)))

;;---- KILL-ALL-LOCAL-VARIABLES-------------------------------------------------

(kill-all-local-variables)
;; This function eliminates all the buffer-local variable bindings of the current buffer except for variables marked as permanent and local hook functions that have a non-nil permanent-local-hook property (see Setting Hooks). As a result, the buffer will see the default values of most variables.
;; This function also resets certain other information pertaining to the buffer: it sets the local keymap to nil, the syntax table to the value of (standard-syntax-table), the case table to (standard-case-table), and the abbrev table to the value of fundamental-mode-abbrev-table.
;; The very first thing this function does is run the normal hook change-major-mode-hook.

;;---- AUTO-START --------------------------------------------------------------

(require 'rainbow-mode) ;; Auto-start CSS colorization
(add-hook 'sgml-mode-hook 'oef-sgml-mode-hook) 
(defun oef-sgml-mode-hook ()
  (setq rainbow-html-colors t)
  (rainbow-mode 1)
  (rainbow-delimiters-mode 1)
  )

(require 'emmet-mode) ;; Auto-start emmet-mode
(emmet-mode 1)

;;---- CONSTS ------------------------------------------------------------------

(defconst oef-mode-version "1.0.0"
  "oef Mode version.")

;;---- GROUPS ------------------------------------------------------------------

(defgroup oef-mode nil
  "Mode for editing OEF (wims) files"
  :group 'languages
  :prefix "oef-"
  :link '(url-link :tag "Site" "http://wims.unice.fr")
  :link '(url-link :tag "Repository" "https://github.com/raoulhatterer/oef"))

(defgroup oef-mode-faces nil
  "Faces for syntax highlighting."
  :group 'oef-mode
  :group 'faces)

;;---- FACES -------------------------------------------------------------------

(defface oef-font-command-face
  '((t :inherit font-lock-function-name-face))
  "Face for commands"
  :group 'oef-mode-faces)

(defface oef-font-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for functions"
  :group 'oef-mode-faces)

(defface oef-font-answer-command-face
  '((t
     (:box
      (:line-width 4 :color "blue" :style nil)
      :inverse-video t :inherit
      (oef-font-command-face))))
  "Face for answer command"
  :group 'oef-mode-faces)

(defface oef-font-statement-command-face
  '((t
     (:height 1.2 :weight extra-bold :inherit
	      (oef-font-answer-command-face))))
  "Face for statement command"
  :group 'oef-mode-faces)

(defface oef-font-positivenumber-face
  '((t (:foreground "#0000EE")))
  "Face for positive number"
  :group 'oef-mode-faces)

(defface oef-font-htag-face
  '((t (:foreground "snow4")))
  "Face for h1 h2 h3 tags"
  :group 'oef-mode-faces)

(defface oef-font-litag-face
  '((t (:foreground "magenta")))
  "Face for li tags"
  :group 'oef-mode-faces)


(defface oef-font-h1text-lightbg-face
  '((t
     (:width normal :height 1.1 :weight bold :underline
             (:color foreground-color :style line)
             :foreground "black")))
  "Face for h1 (sections) tag when the background is light"
  :group 'oef-mode-faces)

(defface oef-font-h1text-darkbg-face
  '((t
     (:width normal :height 1.1 :weight bold :underline
             (:color foreground-color :style line)
             :foreground "white")))
  "Face for h1 (sections) tag when the background is dark"
  :group 'oef-mode-faces)

(defface oef-font-h1text-face
  '((t
     (:inherit
      (oef-font-h1text-lightbg-face))))
  "Face for h1 (sections) tag"
  :group 'oef-mode-faces)


(defface oef-font-h2text-lightbg-face
  '((t
     (:width normal :height 1.05 :weight bold :foreground "black")))
  "Face for h2 (sections) tag when the background is light"
  :group 'oef-mode-faces)

(defface oef-font-h2text-darkbg-face
  '((t
     (:width normal :height 1.05 :weight bold :foreground "white")))
  "Face for h2 (sections) tag when the background is dark"
  :group 'oef-mode-faces)

(defface oef-font-h2text-face
  '((t
     (:inherit
      (oef-font-h2text-lightbg-face))))
  "Face for h2 (sub-sections) tag"
  :group 'oef-mode-faces)

;; (defface oef-font-h2text-face
;;   '((t
;;      (:width normal :height 1.0 :weight bold :foreground "black")))
;;   "Face for h2 (sub-sections) tag"
;;   :group 'oef-mode-faces)

(defface oef-font-answer-type-face
  '((t (:foreground "#CC9900")))
  "Face for answer type and options"
  :group 'oef-mode-faces)

(defface oef-font-control-face
  '((t (:foreground "#FF8C00")))
  "dark orange"
  :group 'oef-mode-faces)

(defface oef-font-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments"
  :group 'oef-mode-faces)

(defface oef-font-warning-face
  '((t :inherit font-lock-warning-face))
  "Face for warning"
  :group 'oef-mode-faces)

(defface oef-font-type-face
  '((t :inherit font-lock-type-face))
  "Face for type"
  :group 'oef-mode-faces)

(defface oef-font-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keywords"
  :group 'oef-mode-faces)

(defface oef-font-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for variables"
  :group 'oef-mode-faces)

;;---- VARS --------------------------------------------------------------------

(defvar oef-answers-options
  '("type=" "option=" "weight=" "type=default"
    "type=raw"
    "option=noaccent" "option=nocase" "option=nodigit" "option=nomathop" "option=noparenthesis" "option=nopunct" "option=noquote" "option=nospace" "option=reaccent" "option=singlespace" "option=symtext"
    "type=numeric"
    "option=comma" "option=absolute" "option=nonstop noanalyzeprint" "option=noanalyzeprint nonstop" "option=nonstop" "option=noanalyzeprint"
    "type=function"
    "option=integer"
    "type=equation"
    "option=eqsign=yes"
    "type=algexp"
    "type=litexp"
    "type=formal"
    "type=case"
    "option=noreaccent"
    "type=nocase"
    "type=atext"
    "type=checkbox"
    "type=radio"
    "type=flashcard"
    "type=multipleclick"
    "option=nolegend"
    "option=split"
    "option=shuffle"
    "option=sort"
    "option=eqweight"
    "type=menu"
    "option=shuffle multiple"
    "option=sort multiple"
    "type=mark"
    "option=color"
    "type=click"
    "type=multipleclick"
    "type=flashcard"
    "option=show"
    "type=chembrut"
    "type=chemdraw"
    "type=chemclick"
    "type=chemeq"
    "type=chset"
    "type=clickfill"
    "type=dragfill"
    "type=clicktile"
    "type=clock"
    "type=compose"
    "type=complex"
    "type=coord"
    "type=correspond"
    "type=crossword"
    "type=draw"
    "type=geogebra"
    "type=javacurve"
    "type=jmolclick"
    "type=jsxgraph"
    "type=jsxgraphcurve"
    "type=keyboard"
    "type=matrix"
    "type=numexp"
    "type=puzzle"
    "type=range"
    "type=reorder"
    "type=set"
    "type=fset"
    "type=aset"
    "type=units"
    "type=sigunits"
    "type=symtext"
    "type=time"
    ))

(defvar oef-commands
  '("title" "language" "author" "email" "format" "css" "keywords" "credits" "description" "observation" "precision" "range" "computeanswer" "statement" "answer" "choice" "condition" "solution" "hint" "help" "feedback" "steps" "nextstep" "conditions" "latex" "embed" "special"))

(defvar oef-storage-types
  '("real" "complex" "text" "integer" "rational" "function" "matrix" ))

(defvar oef-defined-variables
  '("reply " "choice" "step" "sc_reply" "reply_" "help_subject" "oef_firstname" "oef_lastname" "oef_login" "oef_now" "oef_lang" ))

(defvar oef-comparison-operators
  '("isin" "notin" "iswordof" "notwordof" "isvarof" "notvarof" "isvariableof" "notvariableof" "isitemof" "notitemof" "islineof" "notlineof" "issamecase" "notsamecase" "issametext" "notsametext" "or" "and")) ; "==" "=" "!=" "<" "<=" ">" ">=" out of test

(defvar oef-language-reserved-words
  '("to" "of" "within" "in" "into" "by" "internal"))

(defvar oef-wims-functions
  '("append" "nonempty" "getopt" "replace" "embraced" "randitem" "text" "select" "upper" "nospace" "sort" "makelist" "for" "values" "rows2lines" "lines2items" "items2words" "tolower"))

(defvar oef-pari-functions
  '("divrem"))

(defvar oef-maths-functions
  '("evalue" "solve" "simplify" "diff" "int" "int=" "det" "abs" "sqrt" "binomial" "ceil" "floor" "rint" "e" "erf" "erfc" "Euler" "exp" "factorial" "Inf" "gcd" "lcm" "%" "max" "min" "lg" "lgamma" "ln" "log2" "pow" "sgn" "PI" "sin" "cos" "tg" "tan" "sec" "cot" "cotan" "ctg" "csc" "arccos" "acos" "arcsin" "asin" "arctan" "atan" "arctg" "sh" "sinh" "tanh" "tanh" "th" "ch" "cosh" "coth" "cotanh" "Argch"))

(defvar oef-random-functions
  '("random" "randint" "shuffle" "randomitem" "randomrow"))

(defvar oef-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?< "_" table)
    (modify-syntax-entry ?> "_" table)
    table)
  "oef Syntax Table")
;; Without removing <> as SGML matching parenthesis from the syntax table
;; oef-mode is not mattching parenthesis well when there is a comparaison.
;; The down side is that SGML toogle tag visibility is not working anymore
;; and should be (TODO) removed from the SGML menu or rewrite in a different way.

(defvar oef-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [menu-bar oef]             (cons "OEF" (make-sparse-keymap)))
    ;;--------------------------------------------------------------------------
    ;; "C-c <LETTER>" are reserved for users
    ;;--------------------------------------------------------------------------
    map)
  "Keymap for `oef-mode'.")

;;----------------MODE----------------------------------------

(define-derived-mode oef-mode sgml-mode
  "oef-mode"
  "'Online Exercise Format' mode"

  (if (string= (frame-parameter nil 'background-mode) "light") ; test if the background is light (or dark)
      (progn      ; if the background is light
        (set-face-attribute 'oef-font-h1text-face nil :inherit 'oef-font-h1text-lightbg-face)
        (set-face-attribute 'oef-font-h2text-face nil :inherit 'oef-font-h2text-lightbg-face))
    (progn    ; if the background is dark
      (set-face-attribute 'oef-font-h1text-face nil :inherit 'oef-font-h1text-darkbg-face)
      (set-face-attribute 'oef-font-h2text-face nil :inherit 'oef-font-h2text-darkbg-face)))

  (font-lock-add-keywords
   nil
   `(
     ("\\\\comment{.*}" . 'oef-font-comment-face) ; comments
     ("^ *<\\(li\\)>.*</\\(li\\)> *$"(1 'oef-font-litag-face)(2 'oef-font-litag-face)) ; <li> </li>
     (,(regexp-opt oef-comparison-operators 'symbols) . 'oef-font-keyword-face)
     ("\\(real\\|complex\\|text\\|integer\\|rational\\|function\\|matrix\\){\\\\\\w* ?=" . 'oef-font-warning-face) ; warning '\varName=' instead of 'varName='
     (,(regexp-opt oef-storage-types 'words) . 'oef-font-type-face) ; types : text, integer, real...
     ("^\\\\statement{" . 'oef-font-statement-command-face) ; command statement
     ("^\\\\answer{[^}]*}" . 'oef-font-answer-command-face) ; command answer
     (,(regexp-opt oef-commands 'words) . 'oef-font-command-face) ; other oef-commands : embed...
     ("\\(\\\\special\\){[ \\\n]*\\(expandlines\\|imagefill\\|help\\|tabs2lines\\|rename\\|tooltip\\|codeinput\\|imageinput\\|mathmlinput\\|drawinput\\)" (1 'oef-font-function-name-face)(2 'oef-font-keyword-face)) ; special OEF
     ("\\\\\\(for\\|if\\|else\\) *{" 1 'oef-font-control-face)	     ;controls
     ("-[0-9]+\\(\\.[0-9]+\\)?" . 'oef-font-warning-face) ; warning negative number
     ("<\\(h1\\)\\( \\(class\\|id\\) ?=.*\\)?>\\(.+\\)<\\(/h1\\)>" (1 'oef-font-htag-face)(4 'oef-font-h1text-face)(5 'oef-font-htag-face)) ; sections
     ("<\\(h2\\)\\( \\(class\\|id\\) ?=.*\\)?>\\(.+\\)<\\(/h2\\)>" (1 'oef-font-htag-face)(4 'oef-font-h2text-face)(5 'oef-font-htag-face)) ; sub-sections
     ("[^\\w]\\([0-9]+\\(\\.[0-9]+\\)?\\)" 1 'oef-font-positivenumber-face) ; a number in a variable name is not a number in blue (it's a part of the name) 
     (,(regexp-opt oef-language-reserved-words 'words) . 'oef-font-keyword-face)
     (,(regexp-opt oef-answers-options 'symbols) . 'oef-font-answer-type-face)
     (,(regexp-opt oef-defined-variables 'words) . 'oef-font-variable-name-face)
     (,(regexp-opt oef-wims-functions 'words) . 'oef-font-keyword-face)
     (,(regexp-opt oef-pari-functions 'words) . 'oef-font-keyword-face)
     (,(regexp-opt oef-maths-functions 'words) . 'oef-font-keyword-face)
     (,(regexp-opt oef-random-functions 'words) . 'oef-font-keyword-face)
     ("\\(\\w*\\)\\(pari\\|maxima\\|yacas\\|wims\\|draw\\|slib\\|teximg)\\)(" 2 'oef-font-function-name-face) ; advanced functions
     ("\\(\\\\\\w+\\){" 1 'oef-font-warning-face) ; unknown '\command{'
     ("\\(\\\\\\){" 1 'oef-font-positivenumber-face) ; latex expression \{}
     ("\\\\\\w+\\([0-9]?_?\\w?\\)*" . 'oef-font-variable-name-face) ; '\variable'

     )
   )
  )

;;---- AUTO-ACTIVATION of Mode When Opening File -------------------------------

(add-to-list 'auto-mode-alist '("\\.cgi?\\'" . oef-mode)) ;wims file
(add-to-list 'auto-mode-alist '("\\.oef?\\'" . oef-mode)) ;wims file

(provide 'oef-mode)

;;; oef.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
