(require 'codemetrics)
(require 'kotlin-mode)

(add-to-list 'load-path "test")
(require 'utils)

(codemetrics-test simple-kotlin-code
                  "test/kotlin/Simple.kt"
                  kotlin-mode
                  '(2
                    (function_declaration . 0)
                    (if_expression . 1)
                    (for_statement . 1)
                    (call_expression . 0)))
