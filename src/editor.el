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
(setq revert-without-query '(".*"))


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
;; to insert a new-line in multiple cursors use C-j C-i C-n
(use-package multiple-cursors
  :bind
  (("C-M-s-. C-M-s-." . mc/edit-lines)
   ("C->" .      mc/mark-next-like-this)
   ("C-<" .      mc/mark-previous-like-this)
   ("C-M->" .    mc/skip-to-next-like-this)
   ("C-M-<" .    mc/skip-to-previous-like-this)
   ("C-c C-<" .  mc/mark-all-like-this)
   ("C-M-@" .    mc/mark-all-dwim)))


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


;; enable paste with Cmd-v
(eval-after-load "term"
  '(define-key term-raw-map (kbd "s-v") 'term-paste))


;;
;; browse the content of the kill-ring
;;
(use-package browse-kill-ring
  :init
  (browse-kill-ring-default-keybindings))


;;
;; - Smart move-beginning-of-line which stops at the first non blank
;; character if you press C-a you go back to the actual beginning of
;; the line.
;; - Open a shell terminal
;; - Open recent file
(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-c t" . crux-visit-term-buffer)
         ("s-r"   . crux-recentf-find-file)))


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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| J U M P   A R O U N D |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(use-package avy
;;  :init
;;  (global-set-key (kbd "s-j") 'avy-goto-word-or-subword-1)
;;  (global-set-key (kbd "s-.") 'avy-goto-char)
;;  (global-set-key (kbd "s-w") 'ace-window))


;; unbing goto-line
(global-unset-key (kbd "s-l"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;        ----==| P E R S P E C T I V E   W O R K S P A C E S |==----         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package perspective
  :ensure t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-x C-p"))
  :init
  (persp-mode))

;;; editor.el ends here
