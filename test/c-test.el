;;; c-test.el --- C language tests for codemetrics.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Marie Katrine Ekeberg

;; Author: Marie Katrine Ekeberg <mke@themkat.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(require 'codemetrics)

(codemetrics-test c-simple
  "test/c/Simple.c"
  c-mode
  '(4
    (preproc_ifdef . 1)
    (function_definition . 0)
    (for_statement . 1)
    (while_statement . 1)
    (if_statement . 1)
    (call_expression . 0)))

(codemetrics-test c-recursion
  "test/c/Recursion.c"
  c-mode
  '(2
    (function_definition . 0)
    (if_statement . 1)
    (call_expression . 1)))

(codemetrics-test c-control-flow
  "test/c/ControlFlow.c"
  c-mode
  '(5
    (function_definition . 0)
    (switch_statement . 1)
    (call_expression . 0)
    (function_definition . 0)
    (do_statement . 1)
    (call_expression . 0)
    (function_definition . 0)
    (call_expression . 0)
    (if_statement . 1)
    (goto_statement . 2)))

(codemetrics-test c-logical-operators
  "test/c/LogicalOperators.c"
  c-mode
  '(6
    (function_definition . 0)
    (if_statement . 1)
    ("&&" . 0)
    (if_statement . 1)
    ("||" . 0)
    (if_statement . 1)
    ("&&" . 0)
    ("||" . 1)
    (if_statement . 1)
    ("||" . 1)
    ("&&" . 0)))

;;; c-test.el ends here
