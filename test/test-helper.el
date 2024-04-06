(require 'dash)

(defmacro codemetrics-test (test-name file major-mode expected-result)
  "Create a codemetrics test named TEST-NAME using FILE in MAJOR-MODE.
Expecting result EXPECTED-RESULT."
  (declare (indent 1))
  `(ert-deftest ,test-name ()
     (with-current-buffer (find-file-noselect ,file)
       (,major-mode)
       (tree-sitter-mode)
       (let* ((codemetrics-results (codemetrics-buffer))
              (total-score (car codemetrics-results))
              (expressions-and-scores (codemetrics-test-utils-expression-scores codemetrics-results)))
         (should (equal ,expected-result
                        (cons total-score
                              expressions-and-scores)))))))

(defun codemetrics-test-utils-expression-scores (analyze-result)
  "Helper method to get a list of simple expression + score pairs to work with."
  (--map (cons (tsc-node-type (car it))
               (nth 2 it))
         (cdr analyze-result)))
