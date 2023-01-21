;;; A minimal config for using lsp-mode

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; LSP
(setq lsp-keymap-prefix "C-c l")

(require 'lsp-mode)
;; Start lsp-mode with desired languages
(add-hook 'python-mode-hook #'lsp)
(add-hook 'go-mode-hook     #'lsp)
;; Add more as needed

;; Drop-down auto completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
