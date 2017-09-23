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
					;(when (featurep 'aquamacs)
					;  (require 'osx-browse)
					;  (osx-browse-mode 1))


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

(defface oef-font-function-name-face
  '((t (:foreground "orange red")))
  "Face for functions"
  :group 'oef-mode-faces)

(defface oef-font-equal-face
  '((t :inherit oef-font-function-name-face))
  "Face for equal sign"
  :group 'oef-mode-faces)

(defface oef-font-command-face
  '((t :inherit oef-font-function-name-face))
  "Face for commands"
  :group 'oef-mode-faces)

(defface oef-font-answer-command-face
  '((t
     (:box
      (:line-width 1 :color "orange red" :style nil)
      :inherit
      (oef-font-command-face))))
  "Face for answer command"
  :group 'oef-mode-faces)

(defface oef-font-hint-command-face
  '((t
     (:box
      (:line-width 1 :color "orange red" :style nil)
      :inherit
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
  '((t (:foreground "#555555")))
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

(defvar oef-line-spacing   0.1
  "Additional space to put between lines when displaying a buffer.")

(defvar oef-french-words-same-as-keywords
  '("la solution" "une solution" "d'une solution"))

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
    )
  "Used for a dedicated submenu thanks to `get-oef-answers-options'.")

(defvar oef-answers-options nil
  "Used for highlighting `oef-answers-options' is automatically  build from `oef-menu-answer-options' a list of answers types and options.")

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
    "statement{}"
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
  "In this variable we have the definitions of `oef-commands'.  Used to get `oef-commands' (thanks to `get-list-commands-names') for highlighting.  Also used to get the 'Commands menu' (thanks to `get-menu-oef-commands')."
  )

(defvar oef-commands nil
  "`oef-commands' is automatically  build from `oef-definitions-commands' a list of commands definitions.")

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
  "In this variable we have the definitions of `oef-special-commands'.  Used to get `oef-special-commands' (thanks to `get-list-commands-names') for highlighting.  Also used to get the 'Special Commands menu' (thanks to `get-menu-oef-special-commands')."
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

(defvar oef-defined-variables ; in the menu DONE
  '("reply " "choice" "step" "sc_reply" "reply_" "help_subject" "oef_firstname" "oef_lastname" "oef_login" "oef_now" "oef_lang" )
  "Used for highlighting and for a submenu `Defined Variables'."
  )

(defvar oef-comparison-operators ;  "="  "<"  ">"  tested in another place ; in the menu DONE
  '("==" "<="  ">=" "isin" "notin" "iswordof" "notwordof" "isvarof" "notvarof" "isvariableof" "notvariableof" "isitemof" "notitemof" "islineof" "notlineof" "issamecase" "notsamecase" "issametext" "notsametext" "or" "and")
  "Used for highlighting and for a submenu `Comparisons'."
  )

(defvar oef-language-reserved-words ; in the menu DONE
  '("to" "of" "within" "in" "into" "by" "internal")
  "Used for highlighting and for a submenu `Comparisons'."
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
  "Used for highlighting and for a submenu `Wims Functions'."
  )

(defvar oef-wims-functions nil
  "`oef-wims-functions' is automatically  build from `oef-definitions-wims-functions' a list of wims functions definitions.")

(defvar oef-definitions-slib-scripts ; in the menu DONE but has to be split (too big)
  '("slib(algebra/partitionconj «parameters»)"
    "slib(algebra/partitiondraw «parameters»)"
    "slib(algebra/partitionlex «parameters»)"
    "slib(algebra/slopedraw «parameters»)"
    "slib(analysis/inversedomain «parameters»)"
    "slib(analysis/odejs «parameters»)"
    "slib(analysis/odejs2 «parameters»)"
    "slib(analysis/odephase «parameters»)"
    "slib(analysis/rungekutta «parameters»)"
    "slib(analysis/slope.js «parameters»)"
    "slib(analysis/slopefield «parameters»)"
    "slib(analysis/slopefield_img «parameters»)"
    "slib(analysis/slopefield_js «parameters»)"
    "slib(chemistry/atom «parameters»)"
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
    "slib(circuits/complist «parameters»)"
    "slib(circuits/comppos «parameters»)"
    "slib(circuits/draw «parameters»)"
    "slib(circuits/drawcomp «parameters»)"
    "slib(circuits/drawwire «parameters»)"
    "slib(circuits/range «parameters»)"
    "slib(data/columnsort «parameters»)"
    "slib(data/randline «parameters»)"
    "slib(data/random «parameters»)"
    "slib(data/randrec «parameters»)"
    "slib(draw/balance «parameters»)"
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
    "slib(function/bounds «parameters»)"
    "slib(function/bounds2 «parameters»)"
    "slib(function/integrate «parameters»)"
    "slib(games/chessboard «parameters»)"
    "slib(games/chessimage «parameters»)"
    "slib(games/chessmv «parameters»)"
    "slib(geo2D/geogebra «parameters»)"
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
    "slib(graph/connexcomponent «parameters»)"
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
    "slib(graphpaper/correct_milli «parameters»)"
    "slib(graphpaper/func «parameters»)"
    "slib(graphpaper/func_milli «parameters»)"
    "slib(graphpaper/imgpoints «parameters»)"
    "slib(graphpaper/millimetre «parameters»)"
    "slib(graphpaper/strings «parameters»)"
    "slib(graphpaper/tograph «parameters»)"
    "slib(graphpaper/whereclick «parameters»)"
    "slib(lang/enword2ipa «parameters»)"
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
    "slib(life/frcommodity «parameters»)"
    "slib(list/selshuf «parameters»)"
    "slib(matrix/concate «parameters»)"
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
    "slib(media/audio «parameters»)"
    "slib(media/dewplayer «parameters»)"
    "slib(media/player «parameters»)"
    "slib(media/player_mp3_multi «parameters»)"
    "slib(media/video «parameters»)"
    "slib(numeration/babylonien «parameters»)"
    "slib(numeration/basep «parameters»)"
    "slib(numeration/ecriturenombre «parameters»)"
    "slib(numeration/egyptien «parameters»)"
    "slib(oef/blank «parameters»)"
    "slib(oef/codelim «parameters»)"
    "slib(oef/codename «parameters»)"
    "slib(oef/env «parameters»)"
    "slib(oef/insfilename «parameters»)"
    "slib(oef/newfile «parameters»)"
    "slib(oef/postsrc «parameters»)"
    "slib(oef/presrc «parameters»)"
    "slib(polynomial/random «parameters»)"
    "slib(set/subset «parameters»)"
    "slib(stat/1d «parameters»)"
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
    "slib(text/approximation «parameters»)"
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
    "slib(triplerelation/tabular «parameters»)"
    "slib(utilities/mathcalc «parameters»)"
    "slib(utilities/nopaste «parameters»)"
    "slib(utilities/notepad «parameters»)"
    "slib(utilities/notepad1.html «parameters»)"
    "slib(utilities/notepad2.html «parameters»)"
    "slib(utilities/notepad3.html «parameters»)"
    "slib(utilities/tooltip «parameters»)"
    "slib(utilities/trigo-calc «parameters»)"
    )
  "Used for highlighting and for a submenu `Script Library'."
  )

(defvar oef-slib-scripts nil
  "`oef-slib-scripts' is automatically  build from `oef-definitions-slib-scripts' a list of slib script definitions.")

(defvar oef-pari-functions ; in the menu TODO
  '("divrem")
  "Used for highlighting."
  )

(defvar oef-maths-functions ; in the menu TODO
  '("evalue" "solve" "simplify" "diff" "int" "int=" "det" "abs" "sqrt" "binomial" "ceil" "floor" "rint" "e" "erf" "erfc" "Euler" "exp" "factorial" "Inf" "gcd" "lcm" "%" "max" "min" "lg" "lgamma" "ln" "log2" "pow" "sgn" "PI" "sin" "cos" "tg" "tan" "sec" "cot" "cotan" "ctg" "csc" "arccos" "acos" "arcsin" "asin" "arctan" "atan" "arctg" "sh" "sinh" "tanh" "tanh" "th" "ch" "cosh" "coth" "cotanh" "Argch")
  "Used for highlighting."
  )

(defvar oef-random-functions ; in the menu TODO
  '("random" "randint" "shuffle" "randomitem" "randomrow")
  "Used for highlighting.")

(defvar oef-example-files
  nil
  "List of the oef examples files.  This variable is automatically set at Emacs launch.")

(defvar list-commands
  nil
  "List of commands returned by the function `get-list-commands-names'.")

(defvar oef-wims-session nil
  "Active Wims Session in unice wims server."
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


(defvar oef-highlighted-variable
  nil
  )

(defun oef-hl-on()
  "The user wants to highlight the variable at point."
  (setq oef-highlighted-variable (word-at-point)) ; store word at point as the variable name
  (highlight-phrase (concat "\\({" oef-highlighted-variable "\\b\\|\\b" oef-highlighted-variable "\\b\\|\\\\" oef-highlighted-variable "\\b\\)")) ; highlight the variable
  (message (concat "Highlight OEF variable " oef-highlighted-variable)) ; prompt a message
  )

(defun oef-hl-off ()
  "The user wants to unhighlight the variable."
  (message (concat "Unhighlight OEF variable " oef-highlighted-variable))
  (unhighlight-regexp (concat "\\({" oef-highlighted-variable "\\b\\|\\b" oef-highlighted-variable "\\b\\|\\\\" oef-highlighted-variable "\\b\\)"))
  (setq oef-highlighted-variable nil)
  )

(defun oef-highlight-variable ()
  "Highlight a variable or unhighlight an highlighted variable."
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
  (let ((link (substring-no-properties (gui-get-selection 'CLIPBOARD)))
        (url  "http://wims.unice.fr/wims/wims.cgi\\?session="))
    (save-match-data
      (if (string-match url link)
	  (progn
	    (setq oef-wims-session  (substring-no-properties (replace-regexp-in-string ".*session=" "" (gui-get-selection 'CLIPBOARD)) 0 10))
	    (message (concat "Connected to Wims Session : " oef-wims-session)))
        (error "No wims URL with session on the clipboard")))))

(defun oef-edit-exercise-in-browser()
  "Edit file in browser."
  (interactive)
  (oef-copy-all-or-region)  
  (let ((oef-filename (file-name-nondirectory (buffer-file-name))))
    (browse-url  (concat "http://wims.unice.fr/wims/wims.cgi?session=" oef-wims-session  ".3&+lang=fr&+module=adm%2Fmodtool&+cmd=reply&+jobreq=edfile&+fname=src%2F" oef-filename))))

(defun oef-edit-document-in-browser()
  "Edit file in browser."
  (interactive)
  (oef-copy-all-or-region)
  (let ((oef-filename (file-name-nondirectory (buffer-file-name))))
    (browse-url (replace-regexp-in-string ".oef" "" (concat "http://wims.unice.fr/wims/wims.cgi?session=" oef-wims-session  ".3&+lang=fr&+module=adm%2Fdoc&+cmd=reply&+job=edit&+doc=1&+block=" oef-filename)))))

(defun oef-select-parameter ()
  "Select the first «parameter» from the point."
  (interactive)
  (move-beginning-of-line nil)
  (re-search-forward "«")
  (backward-char nil)
  (set-mark-command nil)
  (re-search-forward "»")
  )

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

(defun get-menu-oef-commands ()
  "This function create a submenu with ‘oef-commands’ from commands definitions in `oef-definitions-commands'."
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
   )) ; end of defun get-menu-oef-commands

(defun get-menu-oef-special-commands ()
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
   )) ; end of defun get-menu-oef-special-commands

(defun get-oef-wims-functions ()
  "This function create a submenu `Wims Functions' with ‘oef-wims-functions’."
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
   )) ; end of defun get-oef-wims-functions

(defun get-oef-slib-scripts ()
  "This function create a submenu `Script Library' with ‘oef-slib-scripts’."
  (easy-menu-create-menu
   "Script Library"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "slib(" "" (replace-regexp-in-string " «parameters»)" "" x)) ; each script name in the submenu
              `(lambda () (interactive)
                 (insert   ,x)
                 t))
      )               ; end of the lamda expression
    oef-definitions-slib-scripts ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun get-oef-slib-scripts

(defun get-oef-answers-options ()
  "This function create a submenu with the types and options of an answer from `oef-answers-options'."
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
   )) ; end of defun get-oef-answers-options

(defun get-oef-exo-init-types ()
  "This function create a submenu for variables initialization in an exercise."
  (easy-menu-create-menu
   "Exercise"
   (mapcar
    (lambda (x);             
      (vector (replace-regexp-in-string "{\\(.\\|\n\\)*}" "" x) ; each command name in the submenu
              `(lambda () (interactive)
                 (insert  (concat "\\" ,x))
                 t))
      )               ; end of the lamda expression
    oef-menu-exo-init-types ; sequence : here a list of string
    ) ; end of mapcar
   )) ; end of defun get-oef-exo-init-types

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
   )) ; end of defun get-oef-doc-init-types

(defun get-oef-defined-variables ()
  "This function create a submenu for `oef-defined-variables'."
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
   )) ; end of defun get-oef-defined-variables

(defun get-oef-comparison-operators ()
  "This function create a submenu for `oef-comparison-operators'."
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
   )) ; end of defun get-oef-comparison-operators

(defun get-oef-language-reserved-words ()
  "This function create a submenu for `oef-language-reserved-words'."
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
   )) ; end of defun get-oef-language-reserved-words

(defun prompt-wims-session()
  "Prompt the wims session in the submenu Wims Session"
  (easy-menu-create-menu
   "Wims Session"
   (vector oef-wims-session nil t)
   ))

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
  "This function takes a LIST-COMMANDS-DEFINITIONS  (for example  `oef-definitions-commands') and return a list of commands names (for example `oef-commands')."
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

(defun get-list-wims-functions (list-functions-definitions)
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
  ) ; end get-list-wims-functions

(defun get-list-slib-scripts (list-functions-definitions)
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
  ) ; end get-list-slib-scripts

(defun get-list-answers-options (list-options-definitions)
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

(defun update-oef-menu ()
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



(defun oef-insert-image ()
  "This function insert an image."
  (interactive)
  (insert "<img src=\"\\filedir/«file name»\" class=\"«class name»\"/>")
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

(defun oef-comment-toggle ()
  "Turn a command on or off by adding comment on the beginning of the line."
  (interactive)
  (move-beginning-of-line nil)
  (delete-horizontal-space)
  (if (looking-at "\\\\comment{") ;if the line start with  \comment{
      ;; then we remove the comment to restore  \command{
      (progn
	(forward-char)
	(kill-word 1)
	(delete-char 1)
	(forward-word)
	(delete-char 1)
	;;(move-beginning-of-line nil)
	(oef-mode-indent-region (line-beginning-position) (line-end-position)))
    ;; else if the line don't start with a comment
    (when (string= (string (following-char)) "\\") ;if the line start with a command we turn the line as comment
      (forward-char)
      (insert "comment{")
      (forward-word)
      (insert "}")
      (move-beginning-of-line nil)
      )))

;;----------------MENU----------------------------------------

(setq oef-example-files (directory-files-recursively user-emacs-directory ".oef$")) ; list of strings (the oef examples files) needed to build the OEF menu
(setq oef-commands (get-list-commands-names oef-definitions-commands)) ; list of strings (the oef-commands like 'title' and 'author')
(setq oef-special-commands (get-list-commands-names oef-definitions-special-commands)) ; list of strings (the oef-special-commands)
(setq oef-wims-functions (get-list-wims-functions oef-definitions-wims-functions)) ; list of strings (the oef-wims-functions)
(setq oef-slib-scripts (get-list-slib-scripts oef-definitions-slib-scripts)) ; for highlighting
(setq oef-answers-options (get-list-answers-options  oef-menu-answers-options))

(defvar oef-mode-map
  (let ((map (make-sparse-keymap)))
    ;;    (define-key map [menu-bar sgml] 'undefined) ;SGML menu-bar item suppressed
    ;; menu-bar Text
    (define-key map [menu-bar text paragraph-indent-minor-mode] 'undefined) ;Text menu-bar item `Paragraph indent' suppressed
    (define-key map [menu-bar text toggle-text-mode-auto-fill] 'undefined) ;Text menu-bar item `Auto Fill' suppressed
    (define-key map [menu-bar text center-region] 'undefined) ;Text menu-bar item `Center region' suppressed
    (define-key map [menu-bar text center-paragraph] 'undefined) ;Text menu-bar item `Center paragraph' suppressed
    (define-key map [menu-bar text center-line] 'undefined) ;Text menu-bar item `Center line' suppressed
    (define-key map [menu-bar text indent]    (cons "OEF Indentation" (make-sparse-keymap)))
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
    ["Comment (toogle)" oef-comment-toggle t] ; toogle a command as comment ;
    ["Highlight Variable at point (toggle)" oef-highlight-variable t] ;`Highlight oef variable' added to Text menu-bar
    ))

(easy-menu-add-item oef-menu-bar '()["Select Parameter" oef-select-parameter :help "Select the fist «parameter»."])
(easy-menu-add-item oef-menu-bar '()["Wims Session" nil t]); it's not a real connection (It just extract the session id from the URL)
(easy-menu-add-item oef-menu-bar '("Wims Session")["Connect to a Wims Session" oef-get-wims-session :help "Connect emacs to the active Wims Session if the URL is in the CLIPBOARD."]); it's not a real connection (It just extract the session id from the URL)
(easy-menu-add-item oef-menu-bar '("Wims Session")["Edit Exercise in Browser" oef-edit-exercise-in-browser :help "If the connection with the server is active,\n edit the Exercice wich has the same name on the WIMS server.\n Also copy the buffer content in the CLIPBOARD."]);
(easy-menu-add-item oef-menu-bar '("Wims Session")["Edit Document in Browser" oef-edit-document-in-browser :help "If the connection with the server is active,\n edit the Document wich has the same name on the WIMS server.\n Also copy the buffer content in the CLIPBOARD."]);
(easy-menu-add-item oef-menu-bar '("Files") (get-examples)) ; we add the submenu `Examples' to the oef-menu-bar. This menu is not dynamic.
(easy-menu-add-item oef-menu-bar '("Files")["Open All OEF Examples" oef-mode-open-all t]) ; we add the command "Open All OEF Examples" to the submenu `Examples' in the oef-menu-bar.
;; (easy-menu-add-item oef-menu-bar '("Files") (get-my-oef-files)) ; deactivatedd (too slow)
(easy-menu-add-item oef-menu-bar '("Initializations") (get-oef-exo-init-types)) ; we add the submenu `Exercises' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '("Initializations") (get-oef-doc-init-types)) ; we add the submenu `Documents' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (get-menu-oef-commands)) ; we add the submenu `Commands' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '("Commands") (get-menu-oef-special-commands)) ; we add the submenu `Special' in menu `Commands' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (get-oef-answers-options)) ; we add the submenu `Answers types and options' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (get-oef-defined-variables)) ; we add the submenu `oef-defined-variables' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (get-oef-language-reserved-words)) ; we add the submenu `oef-language-reserved-words' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (get-oef-wims-functions)) ; we add the submenu `Wims Functions' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '() (get-oef-slib-scripts)) ; we add the submenu `Script Library' to the oef-menu-bar.
(easy-menu-add-item oef-menu-bar '("Documents")["Files" nil]); create submenu `Files'  in `Documents'
(easy-menu-add-item oef-menu-bar '("Documents" "Files")["Entrance" oef-find-main :help "The Entrance block of the document is always named `main'"])
(easy-menu-add-item oef-menu-bar '("Documents" "Files")["Other" oef-find-block :help "Other block of the document"])
(easy-menu-add-item oef-menu-bar '("Documents")["Insert" nil]); create submenu `Insert' in submenu  `Documents'
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["Image" oef-insert-image :help "Insert an image.\n\nThe link `Other files' allows you to upload the image file to the wims server.\nCSS properties are defined online in properties of the main file."])
(easy-menu-add-item oef-menu-bar '("Documents" "Insert")["--" nil])
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


(add-hook 'menu-bar-update-hook 'update-oef-menu) ;

;;-----------MAJOR MODE----------------------------------------
;;;###autoload
(define-derived-mode oef-mode sgml-mode
  "oef-mode"
  "'Online Exercise Format' mode"

  (setq line-spacing oef-line-spacing)
  (if (string= (frame-parameter nil 'background-mode) "light") ; test if the background is light (or dark)
      (progn      ; if the background is light
        (set-face-attribute 'oef-font-h1text-face nil :inherit 'oef-font-h1text-lightbg-face)
        (set-face-attribute 'oef-font-h2text-face nil :inherit 'oef-font-h2text-lightbg-face))
    (progn    ; if the background is dark
      (set-face-attribute 'oef-font-h1text-face nil :inherit 'oef-font-h1text-darkbg-face)
      (set-face-attribute 'oef-font-h2text-face nil :inherit 'oef-font-h2text-darkbg-face)))

  ;; key binding
  (define-key oef-mode-map (kbd "C-x RET RET") 'oef-mode-indent-region) ; indent-region with sgml-mode-syntax-table because with oef-syntax-table there are problems with the indentation
  (define-key oef-mode-map (kbd "C-o") nil) ;
  (define-key oef-mode-map (kbd "C-o C-p") 'oef-select-parameter) ;
  (define-key oef-mode-map (kbd "C-o c") 'oef-comment-toggle) ;
  (define-key oef-mode-map (kbd "C-o ws") 'oef-get-wims-session) ;
  (define-key oef-mode-map (kbd "C-o C-o") 'oef-highlight-variable) ;
  (define-key oef-mode-map (kbd "C-o ee") 'oef-edit-exercise-in-browser) ;
  (define-key oef-mode-map (kbd "C-o ed") 'oef-edit-document-in-browser) ;
  (define-key oef-mode-map (kbd "<down-mouse-1>") ; toogle oef-variable highlighting on mouse click
    (lambda (event)
      (interactive "e")
;      (message "%s" event)
      (let ((posn (elt event 1)))		
        (with-selected-window (posn-window posn)
          (goto-char (posn-point posn))
      	(oef-highlight-variable)))))



  

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
     (,(regexp-opt oef-french-words-same-as-keywords 'words) . 'default)
;     ("^ *<\\(li\\)>.*?</\\(li\\)> *$"(1 'oef-font-litag-face)(2 'oef-font-litag-face)) ; <li> </li>
     ("<\\(li\\)[^>]*>"(1 'oef-font-litag-face)) ; <li>
     ("</\\(li\\)>"(1 'oef-font-litag-face)) ;  </li>
     ("wims\\s(\\(for\\) " 1 'oef-font-keyword-face) ; exception (for is a wims function not only a oef-doc-command)
     (,(regexp-opt oef-comparison-operators 'symbols) . 'oef-font-keyword-face)
     ("{[^}^{]*\\(>\\|<\\|!=\\)[^{]+}" 1 'oef-font-keyword-face) ;  "<" ">" "!=" comparison (must be after the precedent line)
     ;; There are text properties here: (face oef-font-keyword-face fontified t) see describe-char
     ("\\(real\\|complex\\|text\\|integer\\|rational\\|function\\|matrix\\){\\\\\\w* ?=" . 'oef-font-warning-face) ; warning '\varName=' instead of 'varName='
     (,(regexp-opt oef-slib-scripts 'words) . 'oef-font-keyword-face) ; slib scripts (some scripts stats with text/) 
     (,(regexp-opt oef-storage-types 'words) . 'oef-font-type-face) ; types : text, integer, real...
     ("^\\\\statement{" . 'oef-font-statement-command-face) ; command statement
     ("^\\\\answer{[^}]*}" . 'oef-font-answer-command-face) ; command answer
     ("^\\\\hint{[^}]*}" . 'oef-font-hint-command-face) ; command hint
     (,(regexp-opt oef-commands 'words) . 'oef-font-command-face) ; other oef-commands : embed...
     (,(regexp-opt oef-doc-commands 'words) . 'oef-font-command-face) ;  oef-doc-commands : def... (for is a oef-doc-command)
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
     ("\\(\\w*\\)\\(pari\\|maxima\\|yacas\\|wims\\|draw\\|slib\\|teximg)\\)(" 2 'oef-font-keyword-face) ; advanced functions
     ("\\(\\\\\\w+\\){" 1 'oef-font-warning-face) ; unknown '\command{'
     ("\\(\\\\\\){" 1 'oef-font-positivenumber-face) ; latex expression \{}
     ("\\\\\\w+\\([0-9]?_?\\w?\\)*" . 'oef-font-variable-name-face) ; '\variable'
     ("[^\\w]\\([0-9]+\\(\\.[0-9]+\\)?\\)" 1 'oef-font-positivenumber-face) ; a number
     ("=" . 'oef-font-equal-face) ; equal sign
     )
   )
  )

;;---- AUTO-ACTIVATION of Mode When Opening File -------------------------------

(add-to-list 'auto-mode-alist '("\\.oef?\\'" . oef-mode)) ;wims file

(provide 'oef-mode)
;;; oef-mode.el ends here
