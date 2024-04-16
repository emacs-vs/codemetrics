;;; lua-test.el --- Lua language tests for codemetrics.el  -*- lexical-binding: t; -*-

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
(require 'lua-mode)

(codemetrics-test lua-simple
  "test/lua/Simple.lua"
  lua-mode
  '(3
    (function_declaration . 0)
    (function_call . 0)
    (function_call . 0)
    (for_statement . 1)
    (if_statement . 2)))

(codemetrics-test lua-recursion
  "test/lua/Recursion.lua"
  lua-mode
  '(2
    (function_declaration . 0)
    (if_statement . 1)
    (function_call . 1)))

;;; lua-test.el ends here
