;;; codemetrics.el --- Plugin shows complexity information  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/codemetrics
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.15.1"))
;; Keywords: convenience complexity

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
;; Plugin shows complexity information.
;;

;;; Code:

(require 'cl-lib)
(require 'pcase)

(require 'tree-sitter)

(require 'codemetrics-rules)

(defgroup codemetrics nil
  "Plugin shows complexity information."
  :prefix "codemetrics-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/codemetrics"))

(defcustom codemetrics-complexity 'cognitive
  "Type of the complexity computation."
  :type '(choice (const :tag "Cognitive Complexity" cognitive)
                 (const :tag "Cyclomatic Complexity" cyclomatic))
  :group 'codemetrics)

(defcustom codemetrics-rules
  `((c-mode      . ,(codemetrics-rules-c))
    (c++-mode    . ,(codemetrics-rules-c++))
    (csharp-mode . ,(codemetrics-rules-csharp))
    (java-mode   . ,(codemetrics-rules-java)))
  "An alist of (major-mode . (node-type . weight)).

WEIGHT is used to determine the final score."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type function))
  :group 'codemetrics)

;;
;; (@* "Logger" )
;;

(defvar codemetrics--debug-mode nil
  "Get more information from the program.")

(defun codemetrics--log (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when codemetrics--debug-mode
    (apply 'message fmt args)))

;;
;; (@* "Util" )
;;

(defmacro codemetrics--ensure-ts (&rest body)
  "Run BODY only if `tree-sitter-mode` is enabled."
  (declare (indent 0))
  `(if (bound-and-true-p tree-sitter-mode)
       (progn ,@body)
     (user-error "Tree-Sitter mode is not enabled for this buffer!")))

(defun codemetrics--s-count-matches (regexp str &optional start end)
  "Like function `s-count-matches' but accept list in REGEXP.

For arguments STR, START, and END, see function `s-count-matches' for details."
  (cond ((listp regexp)
         (let ((count 0))
           (dolist (regex regexp)
             (cl-incf count (s-count-matches regex str start end)))
           count))
        (t (s-count-matches regexp str start end))))

(defun codemetrics--make-even (number)
  "Ensure NUMBER is a even number."
  (if (zerop (% number 2))
      number
    (1+ number)))

;;
;; (@* "Core" )
;;

(defun codemetrics--rules (&optional mode)
  "Return rules from major (MODE)."
  (cdr (assoc (or mode major-mode) codemetrics-rules)))

(defun codemetrics--tsc-traverse-mapc (func tree-or-node &optional depth)
  "Like function `tsc-traverse-mapc' but pass with node.

For arguments FUNC and TREE-OR-NODE, see function `tsc-traverse-mapc' for
details.  Optional argument DEPTH is used for recursive depth calculation."
  (let ((depth (or depth 0))
        (node (if (tsc-node-p tree-or-node) tree-or-node
                (tsc-root-node tree-or-node))))
    (unless (tsc-node-p tree-or-node)
      (funcall func node depth))
    (tsc-mapc-children
     (lambda (node)
       (cl-incf depth)
       (funcall func node depth)
       (codemetrics--tsc-traverse-mapc func node depth)
       (cl-decf depth))
     node)))

;;;###autoload
(defun codemetrics-analyze (content &optional mode)
  "Analyze CONTENT in major (MODE), the code."
  (codemetrics--ensure-ts
    (let* ((mode (or mode major-mode))
           (rules (codemetrics--rules mode))
           (nested-level)  ; this is the "starting" nested level
           (nested 0)
           (score 0))
      (with-temp-buffer
        (insert content)
        (delay-mode-hooks (funcall mode))
        (tree-sitter-mode 1)
        (codemetrics--tsc-traverse-mapc
         (lambda (node depth)
           (when (and nested-level
                      (< depth nested-level))  ; decrement out
             (setq nested-level nil
                   nested 0))
           (when-let* ((type (tsc-node-type node))
                       (a-rule (assoc type rules))  ; cons
                       (rule (cdr a-rule)))
             (let* ((data (if (symbolp rule)
                              (funcall rule node depth nested)
                            rule))
                    (weight (nth 0 data))
                    (inc-nested (nth 1 data)))
               (when inc-nested
                 (if (null nested-level)
                     (setq nested-level depth
                           nested 0)
                   (cl-incf nested)))
               (codemetrics--log "depth: %s, nested-level: %s, nested: %s"
                                 depth nested-level nested)
               (let ((node-score (+ weight nested)))
                 (codemetrics--log "%s" (cons type node-score))
                 ;; The first value is plus, second is times.
                 (cl-incf score node-score)))))
         tree-sitter-tree))
      score)))

;;;###autoload
(defun codemetrics-region (&optional beg end)
  "Analyze the region, from BEG to END."
  (let ((beg (or beg (if (use-region-p) (region-beginning) (point-min))))
        (end (or end (if (use-region-p) (region-end)       (point-max)))))
    (codemetrics-analyze (buffer-substring beg end))))

;;;###autoload
(defun codemetrics-buffer ()
  "Analyze current buffer."
  (codemetrics-analyze (buffer-string)))

;;
;; (@* "Languages" )
;;

(defun codemetrics-rules-java-class-declaration (_node depth _nested)
  "Define weight for Java `class' declaration.

For argument DEPTH, see function `codemetrics-analyze' for more information."
  (cl-case codemetrics-complexity
    (`cognitive
     (if (< 1 depth)  ; if class inside class,
         '(1 nil)     ; we score 1, but don't increase nested level
       '(0 nil)))
    (`cyclomatic '(1 nil))))

(defun codemetrics-rules-java-method-declaration (_node depth nested)
  "Define weight for Java `method' declaration.

For argument NESTED, see function `codemetrics-analyze' for more information."
  (cl-case codemetrics-complexity
    (`cognitive (if (or (<= 5 depth ) (<= 3 nested))
                    '(1 nil)
                  '(0 nil)))
    (`cyclomatic '(1 nil))))

(defun codemetrics-rules-java-outer-loop (node &rest _)
  "Define weight for Java outer loop (jump), `break' and `continue' statements.

For argument NODE, see function `codemetrics-analyze' for more information."
  (cl-case codemetrics-complexity
    (`cognitive (list (if (<= (tsc-count-children node) 2) 0 1) nil))
    (`cyclomatic '(0 nil))))

(defun codemetrics-rules-java-logical-operators (node &rest _)
  "Define weight for Java logical operators.

For argument NODE, see function `codemetrics-analyze' for more information."
  (cl-case codemetrics-complexity
    (`cognitive
     (let ((parent (tsc-get-parent node))
           (sequence nil))
       (when (<= 2 (codemetrics--s-count-matches '("||" "&&") (tsc-node-text parent)))
         (setq sequence t))
       (list (if sequence 1 0) nil)))
    (`cyclomatic '(1 nil))))

(provide 'codemetrics)
;;; codemetrics.el ends here
