;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| L O O K   &   F E E L |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; disable visible bell
(setq visible-bell nil)

;; set preferref font
(set-face-attribute 'default nil
        :font   lambdamacs/default-font
        :height lambdamacs/default-font-size)


;; modeline indicators
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;;
;; better modeline
;;
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


;;
;; Load theme package
;;
(use-package doom-themes
  :init (load-theme lambdamacs/default-theme t))


;;
;; Visual cursor
;;
(use-package beacon
  :ensure t
  :init (beacon-mode 1))


;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))


;;
;; Rainbow delimeters (parenthesis)
;;
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))


;; highlight matching parenthesis
(use-package paren
  :config
  (setq show-paren-delay 0)
  (set-face-background 'show-paren-match nil)
  (set-face-foreground 'show-paren-match "firebrick1")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (add-hook 'clojure-mode-hook #'sshow-paren-mode)
  (add-hook 'emacs-lisp-mode-hook #'show-paren-mode))



;;
;; adds automatically matching parentesis
;;
(use-package elec-pair
  :config
  (electric-pair-mode +1))


;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))



;; font-lock annotations like TODO in source code
(use-package hl-todo
  :init
  (global-hl-todo-mode 1))
