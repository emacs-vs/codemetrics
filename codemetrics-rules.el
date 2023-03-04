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

(declare-function codemetrics-rules-java-class-declaration "codemetrics.el")
(declare-function codemetrics-rules-java-method-declaration "codemetrics.el")
(declare-function codemetrics-rules-java-outer-loop "codemetrics.el")
(declare-function codemetrics-rules-java-logical-operators "codemetrics.el")

;;
;; (@* "Rules" )
;;

;; TODO(everyone): keep the function alphabetically sorted

(defun codemetrics-rules-c ()
  "Return rules for C."
  `((function_definition . codemetrics-rules-java-method-declaration)
    (lambda_expression   . (1 t))
    (if_statement        . (1 t))
    (switch_statement    . (1 t))
    (while_statement     . (1 t))
    (for_statement       . (1 t))
    (do_statement        . (1 t))
    ("&&"                . codemetrics-rules-java-logical-operators)
    ("||"                . codemetrics-rules-java-logical-operators)
    (goto_statement      . (1 t))))

(defun codemetrics-rules-c++ ()
  "Return rules for C++."
  (append
   (codemetrics-rules-c)
   `((class_declaration   . codemetrics-rules-java-class-declaration))))

(defun codemetrics-rules-csharp ()
  "Return rules for C#."
  `((class_declaration  . codemetrics-rules-java-class-declaration)
    (method_declaration . codemetrics-rules-java-method-declaration)
    (lambda_expression  . (0 t))  ; don't score, but increase nested level
    (if_statement       . (1 t))
    (switch_statement   . (1 t))
    (while_statement    . (1 t))
    (for_statement      . (1 t))
    (do_statement       . (1 t))
    ("&&"               . codemetrics-rules-java-logical-operators)
    ("||"               . codemetrics-rules-java-logical-operators)))

(defun codemetrics-rules-go ()
  "Return rules for Go."
  `())

(defun codemetrics-rules-java ()
  "Return rules for Java."
  `((class_declaration  . codemetrics-rules-java-class-declaration)
    (method_declaration . codemetrics-rules-java-method-declaration)
    (lambda_expression  . (0 t))  ; don't score, but increase nested level
    (if_statement       . (1 t))
    (switch_expression  . (1 t))
    (while_statement    . (1 t))
    (for_statement      . (1 t))
    (do_statement       . (1 t))
    (catch_clause       . (1 t))
    ("&&"               . codemetrics-rules-java-logical-operators)
    ("||"               . codemetrics-rules-java-logical-operators)
    (continue_statement . codemetrics-rules-java-outer-loop)
    (break_statement    . codemetrics-rules-java-outer-loop)))

(defun codemetrics-rules-lua ()
  "Return rules for Lua."
  `())

(provide 'codemetrics-rules)
;;; codemetrics-rules.el ends here
