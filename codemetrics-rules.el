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

(declare-function codemetrics-rules--class-declaration "codemetrics.el")
(declare-function codemetrics-rules--method-declaration "codemetrics.el")
(declare-function codemetrics-rules--logical-operators "codemetrics.el")
(declare-function codemetrics-rules--recursion "codemetrics.el")

(declare-function codemetrics-rules--elixir-call "codemetrics.el")
(declare-function codemetrics-rules--java-outer-loop "codemetrics.el")
(declare-function codemetrics-rules--julia-macro-expression "codemetrics.el")
(declare-function codemetrics-rules--lua-binary-expressions "codemetrics.el")
(declare-function codemetrics-rules--ruby-binary "codemetrics.el")
(declare-function codemetrics-rules--rust-outer-loop "codemetrics.el")

;;
;; (@* "Rules" )
;;

;; TODO(everyone): keep the function alphabetically sorted

(defun codemetrics-rules-bash ()
  "Return rules for Bash."
  `((function_definition . codemetrics-rules--method-declaration)
    (if_statement        . (1 t))
    (while_statement     . (1 t))
    (for_statement       . (1 t))
    (command             . codemetrics-rules--recursion)))

(defun codemetrics-rules-c ()
  "Return rules for C."
  `((function_definition . codemetrics-rules--method-declaration)
    (lambda_expression   . (1 t))
    (preproc_ifdef       . (1 t))  ; macro
    (if_statement        . (1 t))
    (switch_statement    . (1 t))
    (while_statement     . (1 t))
    (for_statement       . (1 t))
    (do_statement        . (1 t))
    ("&&"                . codemetrics-rules--logical-operators)
    ("||"                . codemetrics-rules--logical-operators)
    (goto_statement      . (1 t))
    (call_expression     . codemetrics-rules--recursion)))

(defun codemetrics-rules-c++ ()
  "Return rules for C++."
  (append
   (codemetrics-rules-c)
   `((class_declaration   . codemetrics-rules--class-declaration)
     (catch_clause        . (1 t)))))

(defun codemetrics-rules-csharp ()
  "Return rules for C#."
  `((class_declaration     . codemetrics-rules--class-declaration)
    (method_declaration    . codemetrics-rules--method-declaration)
    (lambda_expression     . (0 t))  ; don't score, but increase nested level
    (if_statement          . (1 t))
    (switch_statement      . (1 t))
    (while_statement       . (1 t))
    (for_statement         . (1 t))
    (do_statement          . (1 t))
    (catch_clause          . (1 t))
    (finally_clause        . (1 t))
    ("&&"                  . codemetrics-rules--logical-operators)
    ("||"                  . codemetrics-rules--logical-operators)
    (invocation_expression . codemetrics-rules--recursion)))

(defun codemetrics-rules-elixir ()
  "Return rules for Elixir."
  `((call . codemetrics-rules--elixir-call)))

(defun codemetrics-rules-go ()
  "Return rules for Go."
  `((function_declaration        . codemetrics-rules--method-declaration)
    (method_declaration          . codemetrics-rules--method-declaration)
    (func_literal                . (0 t))  ; don't score, but increase nested level
    (if_statement                . (1 t))
    (expression_switch_statement . (1 t))
    (for_statement               . (1 t))
    ("&&"                        . codemetrics-rules--logical-operators)
    ("||"                        . codemetrics-rules--logical-operators)
    (call_expression             . codemetrics-rules--recursion)))

(defun codemetrics-rules-java ()
  "Return rules for Java."
  `((class_declaration  . codemetrics-rules--class-declaration)
    (method_declaration . codemetrics-rules--method-declaration)
    (lambda_expression  . (0 t))  ; don't score, but increase nested level
    (if_statement       . (1 t))
    (switch_expression  . (1 t))
    (while_statement    . (1 t))
    (for_statement      . (1 t))
    (do_statement       . (1 t))
    (catch_clause       . (1 t))
    (finally_clause     . (1 t))
    ("&&"               . codemetrics-rules--logical-operators)
    ("||"               . codemetrics-rules--logical-operators)
    (continue_statement . codemetrics-rules--java-outer-loop)
    (break_statement    . codemetrics-rules--java-outer-loop)
    (method_invocation  . codemetrics-rules--recursion)))

(defun codemetrics-rules-javascript ()
  "Return rules for JavaScript."
  `((function_declaration . codemetrics-rules--method-declaration)
    (function             . (0 t))  ; traditional anonymous function
    (arrow_function       . (0 t))  ; don't score, but increase nested level
    (if_statement         . (1 t))
    (switch_statement     . (1 t))
    (while_statement      . (1 t))
    (for_statement        . (1 t))
    (do_statement         . (1 t))
    (catch_clause         . (1 t))
    (finally_clause       . (1 t))
    ("&&"                 . codemetrics-rules--logical-operators)
    ("||"                 . codemetrics-rules--logical-operators)
    (call_expression      . codemetrics-rules--recursion)))

(defun codemetrics-rules-julia ()
  "Return rules for Julia."
  `((function_definition . codemetrics-rules--method-declaration)
    (if_statement        . (1 t))
    (while_statement     . (1 t))
    (for_statement       . (1 t))
    (catch_clause        . (1 t))
    (finally_clause      . (1 t))
    ("&&"                . codemetrics-rules--logical-operators)
    ("||"                . codemetrics-rules--logical-operators)
    (macro_expression    . codemetrics-rules--julia-macro-expression)
    (call_expression     . codemetrics-rules--recursion)))

(defun codemetrics-rules-lua ()
  "Return rules for Lua."
  `((function_declaration . codemetrics-rules--method-declaration)
    (if_statement         . (1 t))
    (while_statement      . (1 t))
    (for_statement        . (1 t))
    (repeat_statement     . (1 t))
    (binary_expression    . codemetrics-rules--lua-binary-expressions)
    (goto_statement       . (1 nil))
    (function_call        . codemetrics-rules--recursion)))

(defun codemetrics-rules-php ()
  "Return rules for PHP."
  `())

(defun codemetrics-rules-python ()
  "Return rules for Python."
  `((class_definition    . codemetrics-rules--class-declaration)
    (function_definition . codemetrics-rules--method-declaration)
    (lambda              . (0 t))
    (if_statement        . (1 t))
    (while_statement     . (1 t))
    (for_statement       . (1 t))
    (boolean_operator    . codemetrics-rules--logical-operators)
    (raise_statement     . (lambda (&rest _)
                             (codemetrics-with-complexity
                               '(1 nil)
                               '(0 nil))))
    (call                . codemetrics-rules--recursion)))

(defun codemetrics-rules-ruby ()
  "Return rules for Ruby."
  `((class    . codemetrics-rules--class-declaration)
    (method   . codemetrics-rules--method-declaration)
    (lambda   . (0 t))  ; don't score, but increase nested level
    (if       . (1 t))
    (while    . (1 t))
    (for      . (1 t))
    (do_block . (1 t))
    (until    . (1 t))
    (binary   . codemetrics-rules--ruby-binary)
    (throw    . (1 nil))
    (call     . codemetrics-rules--recursion)))

(defun codemetrics-rules-rust ()
  "Return rules for Rust."
  `((function_item       . codemetrics-rules--method-declaration)
    (closure_expression  . (0 t))
    (if_expression       . (1 t))
    (while_expression    . (1 t))
    (for_expression      . (1 t))
    (loop_expression     . (1 t))
    ("&&"                . codemetrics-rules--logical-operators)
    ("||"                . codemetrics-rules--logical-operators)
    (break_expression    . codemetrics-rules--rust-outer-loop)
    (continue_expression . codemetrics-rules--rust-outer-loop)
    (macro_invocation    . codemetrics-rules--recursion)))

(defun codemetrics-rules-scala ()
  "Return rules for Scala."
  `())

(defun codemetrics-rules-swift ()
  "Return rules for Swift."
  `())

(defun codemetrics-rules-typescript ()
  "Return rules for TypeScript."
  (append
   (codemetrics-rules-javascript)
   `((class_declaration . codemetrics-rules--class-declaration)
     (method_definition . codemetrics-rules--method-declaration))))

(provide 'codemetrics-rules)
;;; codemetrics-rules.el ends here
