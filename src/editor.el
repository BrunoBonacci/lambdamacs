;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                        ----==| E D I T O R |==----                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; tabs - really?
;;
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq tab-width 2)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; Newline at end of file
(setq require-final-newline t)

;; Wrap lines at 70 characters
(setq-default fill-column 70)

;; delete the selection with a keypress
(delete-selection-mode t)


;;
;; encoding
;;
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


;;
;; Global revert
;;
;; reload buffers from disk
(global-auto-revert-mode t)


;;
;; Expand region selection
;;
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


;;
;; Removes these idious trailing whitespaces in your code
;;
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))


;;
;; Creates multiple cursors in the buffer and allows to edit content
;; in different places at the same time.
;;
(use-package multiple-cursors
  :bind
  (("C-M-s-. C-M-s-." . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))


;;
;; Allows to expand your marking selection by pressing the number of
;; items/sexp to mark after the current
;;
(use-package easy-kill
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

;;
;; When searching and replacing it shows the matches in the buffer
;;
(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))


;; hippie expand / completion
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)



;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)


;;
;; installing cua-mode for rectangle selection C-RET
;;
;; enable cua-mode for rectangular selections and regular copy/paste
;; support (like in the host platform)
;;
(require 'cua-base)
(require 'cua-gmrk)
(require 'cua-rect)
(cua-mode 1)
(setq cua-enable-cua-keys nil)


;;
;; browse the content of the kill-ring
;;
(use-package browse-kill-ring
  :init
  (browse-kill-ring-default-keybindings))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                     ----==| U N D O - T R E E |==----                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package undo-tree
  :diminish undo-tree-mode
  :init
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  :config
  (global-undo-tree-mode))
