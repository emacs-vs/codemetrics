;;; javascript-test.el --- JavaScript language tests for codemetrics.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Marie Katrine Ekeberg

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

(codemetrics-test javascript-simple
  "test/javascript/Simple.js"
  js-mode
  '(3
    (function_declaration . 0)
    (call_expression . 0)
    (call_expression . 0)
    (for_statement . 1)
    (if_statement . 2)))

(codemetrics-test javascript-recursion
  "test/javascript/Recursion.js"
  js-mode
  '(2
    (function_declaration . 0)
    (if_statement . 1)
    (call_expression . 1)))

(codemetrics-test javascript-nesting
  "test/javascript/Nesting.js"
  js-mode
  '(4
    (call_expression . 0)
    (arrow_function . 0)
    (call_expression . 0)
    (call_expression . 0)
    (function_expression . 0)
    (if_statement . 2)
    (arrow_function . 0)
    (if_statement . 2)
    (call_expression . 0)))

(codemetrics-test javascript-logical-operators
  "test/javascript/LogicalOperators.js"
  js-mode
  '(2
    ("&&" . 0)
    ("||" . 0)
    ("&&" . 0)
    ("||" . 1)
    ("||" . 1)
    ("&&" . 0)))

;;; javascript-test.el ends here
