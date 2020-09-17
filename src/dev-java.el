;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;              ----==| J A V A   D E V E L O P M E N T |==----               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; lsp-java IDE for Java development
;;
(use-package lsp-mode
  :init
  (setq
   lsp-java-server-install-dir  (expand-file-name "jdt" lambdamacs-save-dir)
   lsp-java-workspace-cache-dir (expand-file-name "cache" lambdamacs-save-dir)
   lsp-java-workspace-dir       (expand-file-name "work" lambdamacs-save-dir))
  :hook
  ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-completion-enable-additional-text-edit nil))
(use-package hydra)
(use-package lsp-ui)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package lsp-treemacs)
