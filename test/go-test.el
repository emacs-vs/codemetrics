(require 'codemetrics)
(require 'go-mode)

(add-to-list 'load-path "test")
(require 'utils)

(codemetrics-test simple-go-code
                  "test/go/Simple.go"
                  go-mode
                  '(2
                    (function_declaration . 0)
                    (if_statement . 1)
                    (call_expression . 0)
                    (for_statement . 1)))
