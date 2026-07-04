;; -*- mode: eask; lexical-binding: t -*-

(package "copilot"
         "0.8.0"
         "An Emacs plugin for GitHub Copilot")

(website-url "https://github.com/copilot-emacs/copilot.el")
(keywords "convenience" "copilot")

(package-file "copilot.el")

(script "test" "eask exec buttercup -L .")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "27.2")
(depends-on "editorconfig")
(depends-on "jsonrpc")
(depends-on "compat")
(depends-on "track-changes" "1.4")

(development
 (depends-on "buttercup"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
