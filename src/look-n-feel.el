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


(setq frame-resize-pixelwise t)

;; remove gaps between windows & titlebar
(when (> emacs-major-version 28)
    ;; from emacs 29
    (setq default-frame-alist
          (append
           '((undecorated . t)
             (drag-internal-border . t)
             (internal-border-width . 15))
           default-frame-alist)))

(setq initial-frame-alist default-frame-alist)

;; expand to full screeen
(when (member initial-window-system '(x w32 ns))
  (toggle-frame-maximized)
  ;;(toggle-frame-fullscreen)
  )


;; disable visible bell
(setq visible-bell nil)

;; set preferref font
(set-face-attribute 'default nil
        :font   lambdamacs/default-font
        :weight lambdamacs/default-font-weight
        :height lambdamacs/default-font-size)


;;
;; run this in the *scratch* to see all available fonts
;;
;; (dolist (font (x-list-fonts "*"))
;;   (insert (format "%s\n" font)))

;;
;; Change font size utility function (globally, all windows)
;;
(defun lambdamacs/set-global-font-size (font-size)
  "Change font size utility function (globally, all windows)"
  (interactive
   (list (read-number "Font size: " (/ lambdamacs/default-font-size 10))))
  (set-face-attribute 'default nil :height (* font-size 10)))


;; modeline indicators
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)


;;
;; better modeline
;;
(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

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
  (set-face-attribute  'show-paren-match nil :weight 'extra-bold)
  (set-face-background 'show-paren-match "DeepSkyBlue3")
  (set-face-foreground 'show-paren-match "white")
  (show-paren-mode +1)
  ;;(add-hook 'clojure-mode-hook #'show-paren-mode)
  ;;(add-hook 'emacs-lisp-mode-hook #'show-paren-mode)
  )



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


;;
;; Function to switch between themes
;;
(defun lambdamacs/switch-theme-to-default ()
  "Switched to the selected default theme. check `lambdamacs/default-theme'."
  (interactive)
  (load-theme lambdamacs/default-theme t))


(defun lambdamacs/switch-theme-to-alternative ()
  "Switched to the selected alternative theme, check `lambdamacs/alternative-theme'"
  (interactive)
  (load-theme lambdamacs/alternative-theme t))



;;
;; Window and layout management
;;
(winner-mode 1)
(global-set-key (kbd "<s-right>") 'winner-redo)
(global-set-key (kbd "<s-left>") 'winner-undo)
