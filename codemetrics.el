;;; codemetrics.el --- Plugin shows complexity information  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/codemetrics
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.15.1") (s "1.12.0"))
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

(require 's)
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
    (go-mode     . ,(codemetrics-rules-go))
    (java-mode   . ,(codemetrics-rules-java))
    (lua-mode    . ,(codemetrics-rules-lua)))
  "An alist of (major-mode . (node-type . weight)).

WEIGHT is used to determine the final score."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type function))
  :group 'codemetrics)

(defcustom codemetrics-percent-score 8.0
  "The score represnet 100 percent."
  :type 'float
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

(defvar codemetrics--recursion-identifier nil
  "Record recursion identifier for increment.")

;;
;; (@* "Util" )
;;

(defmacro codemetrics--ensure-ts (&rest body)
  "Run BODY only if `tree-sitter-mode` is enabled."
  (declare (indent 0))
  `(if (bound-and-true-p tree-sitter-mode)
       (progn ,@body)
     (user-error "Tree-Sitter mode is not enabled for this buffer!")))

(defmacro codemetrics--with-current-buffer (buffer-or-name &rest body)
  "Safely execute BODY when BUFFER-OR-NAME is displayed."
  (declare (indent 1))
  `(when (get-buffer-window ,buffer-or-name)
     (with-current-buffer ,buffer-or-name ,@body)))

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

(defun codemetrics--tsc-compare-type (node type)
  "Compare NODE's type to TYPE."
  ;; tsc-node-type returns a symbol or a string and `string=' automatically
  ;; converts symbols to strings
  (string= (tsc-node-type node) type))

(defun codemetrics--get-children (node)
  "Get list of direct children of NODE."
  (let (children)
    (dotimes (index (tsc-count-children node))
      (push (tsc-get-nth-child node index) children))
    (reverse children)))

(defun codemetrics--tsc-find-children (node type)
  "Search through the children of NODE to find all with type equal to TYPE;
then return that list."
  (cl-remove-if-not (lambda (child) (codemetrics--tsc-compare-type child type))
                    (codemetrics--get-children node)))

;;
;; (@* "Core" )
;;

(defun codemetrics-percentage (score)
  "Calculate percentage from SCORE."
  (floor (* (/ score codemetrics-percent-score) 100.0)))

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
           (score 0)
           (data)
           ;; Global Records
           (codemetrics--recursion-identifier))
      (with-temp-buffer
        (insert content)
        (delay-mode-hooks (funcall mode))
        (tree-sitter-mode 1)
        (codemetrics--tsc-traverse-mapc
         (lambda (node depth)
           (when (and nested-level
                      (<= depth nested-level))  ; decrement out
             (setq nested-level nil
                   nested 0))
           (when-let* ((type (tsc-node-type node))
                       (a-rule (assoc type rules))  ; cons
                       (rule (cdr a-rule)))
             (let* ((rules-data (if (symbolp rule)
                                    (funcall rule node depth nested)
                                  rule))
                    (weight (nth 0 rules-data))
                    (inc-nested (nth 1 rules-data)))
               (when inc-nested
                 (if (null nested-level)
                     (setq nested-level depth
                           nested 0)
                   (cl-incf nested)))
               (codemetrics--log "depth: %s, nested-level: %s, nested: %s"
                                 depth nested-level nested)
               (let ((node-score (+ weight nested)))
                 (codemetrics--log "%s" (cons type node-score))
                 (push (list node node-score depth) data)
                 ;; The first value is plus, second is times.
                 (cl-incf score node-score)))))
         tree-sitter-tree))
      (cons score (reverse data)))))

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

(defun codemetrics-rules-recursion (node &rest _)
  "Handle recursion for most languages uses `identifier' as the keyword."
  (cl-case codemetrics-complexity
    (`cognitive
     (let ((identifier (car (codemetrics--tsc-find-children node "identifier"))))
       (if (equal (tsc-node-text identifier)
                  codemetrics--recursion-identifier)
           '(1 nil)
         '(0 nil))))
    ;; do nothing
    (`cyclomatic '(0 nil))))

(defun codemetrics-rules-java-class-declaration (_node depth _nested)
  "Define weight for Java `class' declaration.

For argument DEPTH, see function `codemetrics-analyze' for more information."
  (cl-case codemetrics-complexity
    (`cognitive
     (if (< 1 depth)  ; if class inside class,
         '(1 nil)     ; we score 1, but don't increase nested level
       '(0 nil)))
    (`cyclomatic '(1 nil))))

(defun codemetrics-rules-java-method-declaration (node depth nested)
  "Define weight for Java `method' declaration.

For arguments NODE, DEPTH, and NESTED, see function `codemetrics-analyze' for
more information."
  ;; XXX: Record the recursion method name (identifier)
  (setq codemetrics--recursion-identifier
        (tsc-node-text (car (codemetrics--tsc-find-children node "identifier"))))
  (cl-case codemetrics-complexity
    ;; These magic numbers are observed by TreeSitter AST.
    (`cognitive (if (or (<= 5 depth) (<= 3 nested))
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

;;
;; (@* "Debug Mode" )
;;

(defun codemetrics-debug--enable ()
  "Start `codemetrics-debug-mode'."
  )

(defun codemetrics-debug--disable ()
  "End `codemetrics-debug-mode'."
  )

;;;###autoload
(define-minor-mode codemetrics-debug-mode
  "Turn on/off debug mode for `codemetrics'."
  :group 'codemetrics
  :init-value nil
  :lighter "CodeMetrics Debug"
  ;;(if codemetrics-debug-mode (codemetrics-debug--enable) (codemetrics-debug--disable))
  (codemetrics--after-change))

;;
;; (@* "Display" )
;;

(defcustom codemetrics-display 'method
  "Choose the scope you want it to display."
  :type '(choice (const :tag "method" method)
                 (const :tag "class" class))
  :group 'codemetrics)

(defcustom codemetrics-delay 0.0
  "Delay time to display results in seconds."
  :type 'float
  :group 'codemetrics)

(defvar-local codemetrics--display-timer nil
  "Timer to render the result.")

(defvar-local codemetrics--ovs nil
  "List of overlays.")

(defun codemetrics--display-nodes (&optional scope)
  "Return a list of node types depends on the display scope variable
`codemetrics-display'."
  (setq scope (or scope codemetrics-display))
  (cl-case scope
    (`method '(method_declaration function_definition))
    (`class   (append '(class_declaration)
                      (codemetrics--display-nodes 'class)))
    (t (user-error "Unknow display scope"))))

(defun codemetrics--display-this-node-p (scope node)
  "Return non-nil when the NODE is inside the display SCOPE."
  (or codemetrics-debug-mode                ; scope is `all'
      (memq (tsc-node-type node) scope)))

(defun codemetrics--display-start (buffer)
  "Display result in BUFFER."
  (codemetrics--with-current-buffer buffer  ; make sure buffer still exists
    (let* ((result (codemetrics-buffer))
           (score (car result))             ; total score
           (data (cdr result))              ; list of `node' and `score'
           (scope (codemetrics--display-nodes)))
      (dolist (it data)
        (let ((node (nth 0 it))
              (node-score (nth 1 it))
              (depth (nth 2 it)))
          (when (codemetrics--display-this-node-p scope node)
            (jcs-print (tsc-node-start-position node))
            )
          ))
      )))

(defun codemetrics--after-change (&rest _)
  "Register to `after-change-functions' variable."
  (when (timerp codemetrics--display-timer)
    (cancel-timer codemetrics--display-timer))
  (setq codemetrics--display-timer
        (run-with-idle-timer codemetrics-delay nil
                             #'codemetrics--display-start (current-buffer))))

(defun codemetrics--enable ()
  "Start `codemetrics-mode'."
  (add-hook 'after-change-functions #'codemetrics--after-change nil t))

(defun codemetrics--disable ()
  "End `codemetrics-mode'."
  (remove-hook 'after-change-functions #'codemetrics--after-change t))

;;;###autoload
(define-minor-mode codemetrics-mode
  "Display codemetrics result in current buffer."
  :group 'codemetrics
  :init-value nil
  :lighter "CodeMetrics"
  (if codemetrics-mode (codemetrics--enable) (codemetrics--disable)))

(provide 'codemetrics)
;;; codemetrics.el ends here
