;;; rust-test.el --- Rust language tests for codemetrics.el  -*- lexical-binding: t; -*-

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
(require 'rust-mode)

(codemetrics-test rust-simple
  "test/rust/Simple.rs"
  rust-mode
  '(2
    (function_item . 0)
    (function_item . 0)
    (call_expression . 0)
    (macro_invocation . 0)
    (if_expression . 1)
    (for_expression . 1)))

(codemetrics-test rust-recursion
  "test/rust/Recursion.rs"
  rust-mode
  '(2
    (function_item . 0)
    (if_expression . 1)
    (call_expression . 1)))

(codemetrics-test rust-nesting
  "test/rust/Nesting.rs"
  rust-mode
  '(5
    (function_item . 0)
    (macro_invocation . 0)
    (call_expression . 0)
    (call_expression . 0)
    (call_expression . 0)
    (closure_expression . 0)
    (closure_expression . 0)
    (match_expression . 2)
    (loop_expression . 1)
    (if_expression . 2)))

(codemetrics-test rust-logical-operators
  "test/rust/LogicalOperators.rs"
  rust-mode
  '(6
    (function_item . 0)
    (if_expression . 1)
    ("&&" . 0)
    (if_expression . 1)
    ("||" . 0)
    (if_expression . 1)
    ("&&" . 0)
    ("||" . 1)
    (if_expression . 1)
    ("||" . 1)
    ("&&" . 0)))

(codemetrics-test rust-control-flow
  "test/rust/ControlFlow.rs"
  rust-mode
  '(17
    (function_item . 0)
    (loop_expression . 1)
    (loop_expression . 2)
    (if_expression . 3)
    (break_expression . 1)
    (if_expression . 3)
    (continue_expression . 0)
    (macro_invocation . 0)
    (for_expression . 1)
    (loop_expression . 2)
    (if_expression . 3)
    (continue_expression . 1)
    (break_expression . 0)))

;;; rust-test.el ends here
