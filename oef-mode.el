;;; oef-mode.el --- Major mode for editing oef templates
;;; -*- coding: utf-8 -*-

;; Copyright 2017-2017 Raoul HATTERER

;; Author: Raoul Hatterer <hatterer.raoul@gmail.com>
;; Version: 0.0.1
;; Maintainer: Raoul Hatterer <hatterer.raoul@gmail.com>

;; Created: July 2017
;; Keywords: languages
;; URL: http://github.com/raoulhatterer/oef-mode
;; Package-Requires: ((rainbow-mode)(emmet-mode)(rainbow-delimiters))
;; News: First publication on elpa
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
;;  (load "oef") ;; load the packaged named oef (best not to include the ending “.el” or “.elc”)
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
;;  (add-to-list 'load-path "~/Library/Preferences/Aquamacs Emacs/Packages/lisp/oef-mode/") ;; Tell Emacs where is your personal elisp lib dir
;;  (load "oef-mode") ;; load the packaged named oef-mode (best not to include the ending “.el” or “.elc”)
;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; before the line (package-initialize).
;; How to Debug Aquamacs if you need to:
;; Past the following command in a terminal:
;; /Applications/Aquamacs.app/Contents/MacOS/Aquamacs -nw --debug-init

;;==============================================================================

;;; Code:
(unless (featurep 'aquamacs); subdir inclusion so oef-mode dir will be included to emacs load path
  (let ((default-directory  "~/.emacs.d/"))
    (normal-top-level-add-subdirs-to-load-path)))


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

(require 'rainbow-mode) ;; Auto-start CSS colorization
(require 'rainbow-delimiters) ;; Auto-start parens matching
(add-hook 'sgml-mode-hook 'oef-mode-hook)
(defun oef-mode-hook ()
  "Activation of some usefull minor modes."
  (setq rainbow-html-colors t)
  (rainbow-mode 1)
  (rainbow-delimiters-mode 1)
  )

(require 'emmet-mode) ;; Auto-start emmet-mode
(emmet-mode 1)

;;---- CONSTS ------------------------------------------------------------------

(defconst oef-mode-version "0.0.1"
  "Oef-mode version.")

;;---- GROUPS ------------------------------------------------------------------

(defgroup oef-mode nil
  "Mode for editing OEF (wims) files"
  :group 'languages
  :prefix "oef-mode"
  :link '(url-link :tag "Site" "http://wims.unice.fr")
  :link '(url-link :tag "Repository" "https://github.com/raoulhatterer/oef-mode"))

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

(defface oef-font-hint-command-face
  '((t
     (:box
      (:line-width 2 :color "blue" :style nil)
      :inverse-video t :inherit
      (oef-font-command-face))))
  "Face for hint command"
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

(defface oef-font-documentation-face
   '((t :inherit font-lock-doc-face))
  "Face for documentation"
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

(defvar oef-answers-options ; in the menu TODO
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

(defvar oef-menu-commands ; in the menu DONE
  '("title{«exerciseTitle»}"
 "language{«en» or «fr»}"
 "author{«forename1»,«name1»;«forename2»,«name2»}"
 "email{«email1»,«email2»}"
 "format{html}"
 "css{<style></style>}"
 "keywords{«keyword1»,«keyword2»}"
 "credits{«acknowledgement of those who contributed to the document or exercice, whether through ideas or in a more direct sense»}"
 "description{«forTheStudent»}"
 "observation{«forTheTeacher»}"
 "precision{1000}"
 "range{«n1..n2»}"
 "computeanswer{«yes» or «no»}"
 "steps{&opt«choice1,»«reply1»
&opt«choice2,»«reply2»,«reply3»
&opt«choice3»}"
 "nextstep{<>}"
 "statement{}"
 "answer{«message»}{«goodAnswer»}{&opt«type=»}{&opt«option=»}{&opt«weight=»}"
 "choice{«message»}{«goodAnswers»}{«badAnswers»}{&opt«option=»}{&opt«weight=»}"
 "condition{«message»}{«conditions»}{&opt«option=»}{&opt«weight=»}"
 "solution{«solution»}"
 "hint{«hint»}"
 "help{«popupHelp»}"
 "feedback{«condition»}{«message»}"
 "conditions{«conditionsNumbers»}"
 "latex{}"
 "embed{«reply1»,&opt«option»}"
 )
  "In this variable we have the definitions of `oef-commands'.  Used to get `oef-commands' (thanks to `get-list-commands-names') for highlighting.  Also used to get the 'Commands menu' (thanks to `get-oef-commands')."
  )

(defvar oef-commands nil
  "`oef-commands' is automatically  build from `oef-menu-commands' a list of commands definitions.")

(defvar oef-menu-special-commands ; in the menu DONE
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
  "In this variable we have the definitions of `oef-special-commands'.  Used to get `oef-special-commands' (thanks to `get-list-commands-names') for highlighting.  Also used to get the 'Commands menu' (thanks to `get-oef-special-commands')."
  )

(defvar oef-special-commands nil
  "`oef-special-commands' is automatically  build from `oef-menu-special-commands' a list of special commands definitions.")

(defvar oef-doc-commands ; used for highlighting. ; in the menu TODO
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
    "slib"
    "tooltip"
    "while")
  )

(defvar oef-storage-types
  '("real" "complex" "text" "integer" "rational" "function" "matrix" )
  "List of Oef Variable Types.  Used for highlighting.  See also `oef-menu-exo-init-types' and `oef-menu-doc-init-types'."
  )

(defvar oef-menu-exo-init-types ; in the menu DONE
  '(
    "real{}"
    "complex{}"
    "text{}"
    "integer{}"
    "rational{}"
    "function{}"
    "matrix{}"
    )
  "In this variable we have the definitions of variables initialization commands to be used in an exercise.  Used to get the 'Initialization menu' (thanks to `get-oef-exo-init-types').  See also `oef-storage-types' and `oef-menu-doc-init-types'."
  )

(defvar oef-menu-doc-init-types ; in the menu DONE
  '(
    "def{real }"
    "def{complex }"
    "def{text }"
    "def{integer }"
    "def{rational }"
    "def{function }"
    "def{matrix }"
    )
 "In this variable we have the definitions of variables initialization commands to be used in a document.  Used to get the 'Initialization menu' (thanks to `get-oef-doc-init-types').  See also `oef-storage-types' and `oef-menu-exo-init-types'."
  )

(defvar oef-defined-variables
  '("reply " "choice" "step" "sc_reply" "reply_" "help_subject" "oef_firstname" "oef_lastname" "oef_login" "oef_now" "oef_lang" )
  "Used for highlighting."
  )

(defvar oef-comparison-operators ;  "="  "<"  ">"  tested in another place
  '("==" "<="  ">=" "isin" "notin" "iswordof" "notwordof" "isvarof" "notvarof" "isvariableof" "notvariableof" "isitemof" "notitemof" "islineof" "notlineof" "issamecase" "notsamecase" "issametext" "notsametext" "or" "and")
  "Used for highlighting."
  )

(defvar oef-language-reserved-words
  '("to" "of" "within" "in" "into" "by" "internal")
  "Used for highlighting."
  )

(defvar oef-wims-functions
  '("append" "nonempty" "getopt" "replace" "embraced" "randitem" "text" "select" "upper" "nospace" "sort" "makelist" "for" "values" "rows2lines" "lines2items" "items2words" "tolower")
  "Used for highlighting."
  )

(defvar oef-pari-functions
  '("divrem")
  "Used for highlighting."
  )

(defvar oef-maths-functions
  '("evalue" "solve" "simplify" "diff" "int" "int=" "det" "abs" "sqrt" "binomial" "ceil" "floor" "rint" "e" "erf" "erfc" "Euler" "exp" "factorial" "Inf" "gcd" "lcm" "%" "max" "min" "lg" "lgamma" "ln" "log2" "pow" "sgn" "PI" "sin" "cos" "tg" "tan" "sec" "cot" "cotan" "ctg" "csc" "arccos" "acos" "arcsin" "asin" "arctan" "atan" "arctg" "sh" "sinh" "tanh" "tanh" "th" "ch" "cosh" "coth" "cotanh" "Argch")
  "Used for highlighting."
  )

(defvar oef-random-functions
  '("random" "randint" "shuffle" "randomitem" "randomrow")
    "Used for highlighting.")

(defvar oef-example-files
  nil
  "List of the oef examples files.  This variable is automatically set at Emacs launch.")

(defvar list-commands
  nil
  "List of commands returned by the function `get-list-commands-names'.")

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

(defun get-examples ()
 "This function create a submenu with oef examples."
  (easy-menu-create-menu
   "Examples"
   (mapcar                   ; (mapcar function sequence) mapcar applies function to each element of sequence, and returns a list of the results.
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
   )) ; end of defun get-examples

(defun get-oef-commands ()
 "This function create a submenu with ‘oef-commands’ from commands definitions in `oef-menu-commands'."
  (easy-menu-create-menu
   "Commands"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "{\\(.\\|\n\\)*}" "" x) ; each command name in the submenu
              `(lambda () (interactive)
                 (insert  (concat "\\" ,x))
                 t))
      )               ; end of the lamda expression
    oef-menu-commands ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun get-oef-commands

(defun get-oef-special-commands ()
 "This function create a submenu special with ‘oef-special-commands’."
  (easy-menu-create-menu
   "Special"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "special{" "" (replace-regexp-in-string " «parameters»}" "" x)) ; each special-command name in the submenu
              `(lambda () (interactive)
                 (insert  (concat "\\" ,x))
                 t))
      )               ; end of the lamda expression
    oef-menu-special-commands ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun get-oef-commands

(defun get-oef-answers-options ()
  "This function create a submenu with the types and options of an answer from `oef-answers-options'."
  (easy-menu-create-menu
   "Answers types and options"
   (mapcar
    (lambda (x);             
      (vector  x ; each type or option in the submenu
              `(lambda () (interactive)
                 (insert ,x)
                 t))
      )               ; end of the lamda expression
    oef-answers-options ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun get-oef-answers-options

  
(defun get-oef-exo-init-types ()
 "This function create a submenu for variables initialization in an exercice."
  (easy-menu-create-menu
   "Exercice"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "{\\(.\\|\n\\)*}" "" x) ; each command name in the submenu
              `(lambda () (interactive)
                 (insert  (concat "\\" ,x))
                 t))
      )               ; end of the lamda expression
    oef-menu-exo-init-types ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun get-oef-commands

(defun get-oef-doc-init-types ()
 "This function create a submenu for variables initialization in an document."
  (easy-menu-create-menu
   "Document"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "Def{" "" (replace-regexp-in-string " }" "" x)) ; each type name in the submenu
              `(lambda () (interactive)
                 (insert  (concat "\\" ,x))
                 t))
      )               ; end of the lamda expression
    oef-menu-doc-init-types ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun get-oef-commands

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

(defun get-list-commands-names (list-commands-definitions)
  "This function takes a LIST-COMMANDS-DEFINITIONS  (for example  `oef-menu-commands') and return a list of commands names (for example `oef-commands')."
  (setq list-commands '())
  (dolist
      (command-definition list-commands-definitions)
    (add-to-list
     'list-commands
     (replace-regexp-in-string "{\\(.\\|\n\\)+}" "" command-definition)
     )
    )
   (nreverse list-commands)
  )

;; (defun update-oef-menu () ; ;; desactivate because slowdown aquamacs
;;   "This function update the oef-menu"
;;   (easy-menu-add-item oef-menu-bar '("Files") (get-my-oef-files))
;;   )

(defun oef-mode-open-all ()
  "Opens all files found in the list `oef-example-files' in read-only buffers.
You can add more examples in the examples folder in your `user-emacs-directory'"
  (interactive)
  (dolist (oef-example-file oef-example-files)
    (find-file-read-only oef-example-file))
  (ido-switch-buffer)
  )

(defun oef-mode-indent-region (start end)
  "This fuction try to smartly indent the region selected.

It uses `sgml-mode-syntax-table' because with `oef-mode-syntax-table' there are more problems with indentation.
If it fails (it will after '<' or '>' comparison signs) you can use `indent-rigidly' for re-indent manually
the first line which has bad indentation.  Then you can call `oef-mode-indent-region' again for the rest of the code."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 ;; Operate on the current line if region is not to be used.
                 (list (line-beginning-position) (line-end-position))))

  (with-syntax-table sgml-mode-syntax-table
    (indent-region start end)
    ))

;;----------------MENU----------------------------------------

(setq oef-example-files (directory-files-recursively user-emacs-directory ".oef$")) ; list of strings (the oef examples files) needed to build the OEF menu
(setq oef-commands (get-list-commands-names oef-menu-commands)) ; list of strings (the oef-commands like 'title' and 'author')
(setq oef-special-commands (get-list-commands-names oef-menu-special-commands)) ; list of strings (the oef-special-commands)

(defvar oef-mode-map
  (let ((map (make-sparse-keymap)))
    ;;    (define-key map [menu-bar sgml] 'undefined) ;SGML menu-bar item suppressed
    ;; menu-bar Text
    (define-key map [menu-bar text paragraph-indent-minor-mode] 'undefined) ;Text menu-bar item `Paragraph indent' suppressed
    (define-key map [menu-bar text toggle-text-mode-auto-fill] 'undefined) ;Text menu-bar item `Auto Fill' suppressed
    (define-key map [menu-bar text center-region] 'undefined) ;Text menu-bar item `Center region' suppressed
    (define-key map [menu-bar text center-paragraph] 'undefined) ;Text menu-bar item `Center paragraph' suppressed
    (define-key map [menu-bar text center-line] 'undefined) ;Text menu-bar item `Center line' suppressed
    (define-key map [menu-bar text indent]    (cons "Indent" (make-sparse-keymap)))
    (define-key map [menu-bar text indent indent-line] '(menu-item "Smart Indent Line" oef-mode-indent-line)) ;`Smart Indent Line' added to Text menu-bar
    (define-key map [menu-bar text indent indent-region] '(menu-item "Smart Indent Region" oef-mode-indent-region)) ;`Smart Indent Region' added to Text menu-bar
    (define-key map [menu-bar text indent indent-rigidly] '(menu-item "Indent Region" indent-rigidly)) ;`Indent Region' added to Text menu-bar
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
       ;;["Open All OEF Examples" oef-mode-open-all t] ; item in the OEF menu
    ))

(easy-menu-add-item oef-menu-bar '("Files") (get-examples)) ; we add the submenu `Examples' to the oef-menu-bar. This menu is not dynamic.
(easy-menu-add-item oef-menu-bar '("Files")["Open All OEF Examples" oef-mode-open-all t]) ; we add the command "Open All OEF Examples" to the submenu `Examples' in the oef-menu-bar.
;; (easy-menu-add-item oef-menu-bar '("Files") (get-my-oef-files)) ; deactivatedd (too slow)
(easy-menu-add-item oef-menu-bar '("Initializations") (get-oef-exo-init-types)) ; we add the submenu `Exercices' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '("Initializations") (get-oef-doc-init-types)) ; we add the submenu `Documents' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (get-oef-commands)) ; we add the submenu `Commands' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '("Commands") (get-oef-special-commands)) ; we add the submenu `Special' in menu `Commands' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (get-oef-answers-options)) ; we add the submenu `Answers types and options' to the oef-menu-bar.

;; deactivated because slowdown aquamacs
;; (add-hook 'menu-bar-update-hook 'update-oef-menu) ;add the function update-oef-menu to a hook that runs each time the menu opens so the 'My Files' in oef menu is dynamic

;;-----------MAJOR MODE----------------------------------------
;;;###autoload
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


  ;; key binding
  (define-key oef-mode-map (kbd "C-x RET RET") 'oef-mode-indent-region) ; indent-region with sgml-mode-syntax-table because with oef-syntax-table there are ploblems with the indentation

  ;; not working:
  ;; (define-key oef-mode-map (kbd "C-*") '(lambda ()
  ;;          (interactive)
  ;;          (with-syntax-table sgml-mode-syntax-table (newline-and-indent))))

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
;     ("^ *<\\(li\\)>.*?</\\(li\\)> *$"(1 'oef-font-litag-face)(2 'oef-font-litag-face)) ; <li> </li>
     ("<\\(li\\)[^>]*>"(1 'oef-font-litag-face)) ; <li>
     ("</\\(li\\)>"(1 'oef-font-litag-face)) ;  </li>
     (,(regexp-opt oef-comparison-operators 'symbols) . 'oef-font-keyword-face)
     ("{[^}^{]*\\(>\\|<\\|!=\\)[^{]+}" 1 'oef-font-keyword-face) ;  "<" ">" "!=" comparison (must be after the precedent line)
     ;; There are text properties here: (face oef-font-keyword-face fontified t) see describe-char
     ("\\(real\\|complex\\|text\\|integer\\|rational\\|function\\|matrix\\){\\\\\\w* ?=" . 'oef-font-warning-face) ; warning '\varName=' instead of 'varName='
     (,(regexp-opt oef-storage-types 'words) . 'oef-font-type-face) ; types : text, integer, real...
     ("^\\\\statement{" . 'oef-font-statement-command-face) ; command statement
     ("^\\\\answer{[^}]*}" . 'oef-font-answer-command-face) ; command answer
     ("^\\\\hint{[^}]*}" . 'oef-font-hint-command-face) ; command hint
     (,(regexp-opt oef-commands 'words) . 'oef-font-command-face) ; other oef-commands : embed...
     (,(regexp-opt oef-doc-commands 'words) . 'oef-font-command-face) ;  oef-doc-commands : def...
     ("\\(\\\\special\\){[ \\\n]*\\(expandlines\\|imagefill\\|help\\|tabs2lines\\|rename\\|tooltip\\|codeinput\\|imageinput\\|mathmlinput\\|drawinput\\)" (1 'oef-font-function-name-face)(2 'oef-font-keyword-face)) ; special OEF
     ("\\\\\\(for\\|if\\|else\\) *{" 1 'oef-font-control-face)	     ;controls
     ("-[0-9]+\\(\\.[0-9]+\\)?" . 'oef-font-warning-face) ; warning negative number
     ("<\\(h1\\)\\( \\(class\\|id\\) ?=.*\\)?>\\(.+\\)<\\(/h1\\)>" (1 'oef-font-htag-face)(4 'oef-font-h1text-face)(5 'oef-font-htag-face)) ; sections
     ("<\\(h2\\)\\( \\(class\\|id\\) ?=.*\\)?>\\(.+\\)<\\(/h2\\)>" (1 'oef-font-htag-face)(4 'oef-font-h2text-face)(5 'oef-font-htag-face)) ; sub-sections
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
     ("[^\\w]\\([0-9]+\\(\\.[0-9]+\\)?\\)" 1 'oef-font-positivenumber-face) ; a number in a variable name is not a number in blue (it's a part of the name)
     )
   )
  )

;;---- AUTO-ACTIVATION of Mode When Opening File -------------------------------

(add-to-list 'auto-mode-alist '("\\.oef?\\'" . oef-mode)) ;wims file

(provide 'oef-mode)
;;; oef-mode.el ends here
