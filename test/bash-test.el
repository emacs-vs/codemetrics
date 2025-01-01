;;; bash-test.el --- Bash language tests for codemetrics.el  -*- lexical-binding: t; -*-

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

(codemetrics-test bash-simple
  "test/bash/Simple.sh"
  sh-mode
  '(3
    (function_definition . 0)
    (command . 0)
    (function_definition . 0)
    (for_statement . 1)
    (command . 0)
    (command . 0)
    (function_definition . 0)
    (while_statement . 1)
    (command . 0)
    (if_statement . 1)
    (command . 0)
    (command . 0)
    (command . 0)))

(codemetrics-test bash-recursion
  "test/bash/Recursion.sh"
  sh-mode
  '(2
    (function_definition . 0)
    (if_statement . 1)
    (command . 0)
    (command . 1)
    (command . 0)
    (command . 0)))

;;; bash-test.el ends here
