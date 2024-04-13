(package "codemetrics"
         "0.1.0"
         "Plugin shows complexity information")

(website-url "https://github.com/emacs-vs/codemetrics")
(keywords "convenience" "complexity")

(package-file "codemetrics.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "tree-sitter")
(depends-on "s")
(depends-on "dash")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(development
 (depends-on "kotlin-mode")
 (depends-on "go-mode")
 (depends-on "rust-mode")
 (depends-on "typescript-mode")
 (depends-on "tree-sitter-langs"))
