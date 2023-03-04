;;; codemetrics-rules.el --- Define scoring rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Here we list of all scoring rules for all programming languages.
;;

;;; Code:

;;
;; (@* "Externals" )
;;

;; TODO(everyone): keep the forward declared alphabetically sorted

(declare-function codemetrics-weight-java-declaration "codemetrics.el")
(declare-function codemetrics-weight-java-outer-loop "codemetrics.el")
(declare-function codemetrics-weight-java-logical-operators "codemetrics.el")

;;
;; (@* "Rules" )
;;

;; TODO(everyone): keep the function alphabetically sorted

(defun codemetrics-rules-csharp ()
  "Return weight rules for C#."
  `((class_declaration  . 1)
    (method_declaration . 1)
    (if_statement       . 1)
    (switch_statement   . 1)
    (while_statement    . 1)
    (for_statement      . 1)
    (do_statement       . 1)))

(defun codemetrics-rules-java ()
  "Return weight rules for Java."
  `((class_declaration  . codemetrics-weight-java-declaration)
    (method_declaration . codemetrics-weight-java-declaration)
    (lambda_expression  . 0)  ; increase level, but don't score
    (if_statement       . 1)
    (switch_expression  . 1)
    (while_statement    . 1)
    (for_statement      . 1)
    (do_statement       . 1)
    (catch_clause       . 1)
    ("&&"               . codemetrics-weight-java-logical-operators)
    ("||"               . codemetrics-weight-java-logical-operators)
    (continue_statement . codemetrics-weight-java-outer-loop)
    (break_statement    . codemetrics-weight-java-outer-loop)))

(provide 'codemetrics-rules)
;;; codemetrics-rules.el ends here
