;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                       ----==| G E N E R A L |==----                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(setq lambdamacs-save-dir    (expand-file-name lambdamacs/save-place user-emacs-directory))

;; reduce the frequency of garbage collection by making it happen on
;; each 20MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 20000000)

;; warn when opening files bigger than 50MB
(setq large-file-warning-threshold 50000000)

;;
;; gpg password in minibuffer
;;
;; works with GnuPG 2.2.41
;; it doesn't seem to work with GnuPG 2.4.3
(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))


;;
;; Install password generator
;;
(use-package password-generator)



;;
;; Automatically save buffer when losing focus
;;
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))


;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;
;; super-save seems to have some problem recently
;; so implementing this hack
;;
(defun lambdamacs/save-all-modified-buffers ()
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'save-some-buffers))

;; replace the standard save-buffer with save all
(global-set-key (kbd "C-x C-s") 'lambdamacs/save-all-modified-buffers)


;;
;; Fixing PATH and env var issues
;;
(use-package exec-path-from-shell
  :init
  (when (memq system-type '(darwin))
    (exec-path-from-shell-initialize)))


;;
;; Set the executable permissions on scripts
;;
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                    ----==| C O M P L E T I O N |==----                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :ensure t
  :demand t
;;  :bind (:map vertico-map
;;              ("C-j" . vertico-next)
;;              ("C-k" . vertico-previous)
;;              ("C-f" . vertico-exit-input)
;;              :map minibuffer-local-map
;;              ("M-h" . vertico-directory-up))
  :custom
  (vertico-cycle t)

  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))

  :config
  (require 'vertico-directory)
  (vertico-mode))


(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)     ;; always quit at completion boundary
  (corfu-quit-no-match t)        ;; always quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))



(use-package orderless
  :ensure t
  :demand t
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        orderless-matching-styles '(orderless-literal orderless-regexp)
        completion-category-overrides
        '((file (styles partial-completion)))))



(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :config
  (marginalia-mode))



;;
;; Commands descriptions for M-x commands
;;
(use-package counsel
  :bind (("C-x j"   . counsel-imenu)))


;;
;; Dired config
;;
;; Enable Dired to copy between buffers in a split-screen
(setq dired-dwim-target t)
;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies 'always) ; “always” means no asking
(setq dired-recursive-deletes 'top) ; “top” means ask once
(require 'dired-x)
(global-set-key (kbd "C-x C-/")  'dired-jump) ;; jump to dired currentfile



;; use Shift+arrow_keys to move cursor around split panes
;; same but with [Cmd]+[alt]+[->]
(global-set-key [M-s-left]  'windmove-left)
(global-set-key [M-s-right] 'windmove-right)
(global-set-key [M-s-up]    'windmove-up)
(global-set-key [M-s-down]  'windmove-down)
(setq windmove-wrap-around t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;              ----==| B Y T E   C O M P I L A T I O N |==----               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; taken from:
;; https://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
;;
(defun byte-compile-all-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))


(defun er-remove-elc-on-save ()
  "If you're saving an Emacs Lisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'er-remove-elc-on-save)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| S A V E   P L A C E S |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file
        (expand-file-name "saveplace" lambdamacs-save-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file
        (expand-file-name "savehist" lambdamacs-save-dir))
  (savehist-mode +1))


(use-package recentf
  :config
  (setq recentf-save-file
        (expand-file-name "recentf" lambdamacs-save-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))




;;
;; display keys/commands
;;
(use-package command-log-mode
  :init
  (setq command-log-mode-key-binding-open-log nil))


;;
;; Pop-up with available options for Key prefix
;;
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))


;;
;; Copy the full path of the current file into the clipboard
;; credit: https://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
;;
(defun copy-file-path-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file path '%s' to the clipboard." filename))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                       ----==| P R O D I G Y |==----                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package prodigy
  :init
  (progn
    ;;
    ;; if a .prodigy exists load it
    ;;
    (if (file-exists-p "~/.prodigy.el")
        (load "~/.prodigy.el"))
    ;; set key binding
    (global-set-key (kbd "C-x p") 'prodigy)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                       ----==| P A R A D O X |==----                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; broken due to: https://github.com/Malabarba/paradox/issues/185
;;(use-package paradox
;;  :config
;;  (paradox-enable))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| W I N D O W   M G M T |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Allows to fix a window in place so that it is not closed/removed
;; until you explicitly ask for it.
;;
;; Bindings
;; C-x 9 - Mark as sticky
;; C-u C-x 0 - close sticky window
;;
(load "sticky-windows.el")


;;
;; Intuitive window resizing
;;
(use-package windsize
  :init
  (windsize-default-keybindings))


;;
;; Allows to move the windows transposing them
;;
;;(use-package transpose-frame
;;  :bind
;;  ("C-c s" . flop-frame))

;;
;; FIXME: weird behaviour on small screens
;;
;; this is min size before splitting, so the double of what you want
;; (setq split-height-threshold 70
;;       split-width-threshold  160)
;;
;; (defun split-window-sensibly-vertically (&optional window)
;;     "replacement `split-window-sensibly' function which prefers vertical splits"
;;     (interactive)
;;     (let ((window (or window (selected-window))))
;;         (or (and (window-splittable-p window t)
;;                  (with-selected-window window
;;                      (split-window-right)))
;;             (and (window-splittable-p window)
;;                  (with-selected-window window
;;                      (split-window-below))))))
;;
;; (setq split-window-preferred-function #'split-window-sensibly-vertically)

;;
;; Popups windows management
;;
;; TODO: check/setup shakle: https://depp.brause.cc/shackle/
;;
;;(use-package popper
;;  :ensure t
;;  :bind (("C-`"   . popper-toggle-latest)
;;         ("M-`"   . popper-cycle)
;;         ("C-M-`" . popper-toggle-type))
;;  :init
;;  (setq popper-reference-buffers
;;        '("\\*Messages\\*"
;;          "\\*helm-ag\\*"
;;          "Output\\*$"
;;          help-mode
;;          compilation-mode))
;;  (popper-mode +1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                       ----==| C O M P A N Y |==----                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (use-package company
;;   :ensure t
;;   :config
;;   (setq company-idle-delay 0.5)
;;   (setq company-show-numbers t)
;;   (setq company-tooltip-limit 10)
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-tooltip-align-annotations t)
;;   ;; invert the navigation direction if the the completion popup-isearch-match
;;   ;; is displayed on top (happens near the bottom of windows)
;;   (setq company-tooltip-flip-when-above t)
;;   (global-company-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                     ----==| B O O K M A R K S |==----                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package bm
  :init
  (setq
   bm-restore-repository-on-load t
   bm-repository-file (expand-file-name "bm-bookmarks" lambdamacs-save-dir)
   bm-buffer-persistence t
   bm-restore-repository-on-load t
   bm-cycle-all-buffers t
   bm-in-lifo-order t
   bm-persistent-face 'bm-face)

  :bind
  (("s-1" . 'bm-toggle)
   ("s-2" . 'bm-previous)
   ("s-3" . 'bm-next)
   ("s-5" . 'bm-bookmark-regexp)
   ("s-0" . 'bm-remove-all-current-buffer)
   ("s-)" . 'bm-remove-all-all-buffers))
  :config
  (add-hook 'after-save-hook   #'(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                  ----==| S T A R T   D A E M O N |==----                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; start daemon server
;;
;; Start server and set directory
(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(server-start)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                          ----==| D I F F |==----                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; ediff - don't start another frame
;;
(require 'ediff)
(setq
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally)


;;
;; ztree install (directory diff)
;;
(use-package ztree)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                        ----==| P R O C E D |==----                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)
(require 'proced)
(push '(simple  pid pcpu pmem rss state thcount comm args) proced-format-alist)
(push '(java-all (comm . "java")) proced-filter-alist)
(push '(java-no-lein (args . "java -classpath")) proced-filter-alist)
(setq proced-format 'simple)

;;; general.el ends here
