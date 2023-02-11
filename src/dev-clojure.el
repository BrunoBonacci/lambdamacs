;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                       ----==| C L O J U R E |==----                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (setq sp-base-key-bindings 'paredit)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol nil)
    (sp-use-paredit-bindings)
    (show-smartparens-global-mode 0)))



(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  :config
  (setq clojure-indent-style 'always-indent))



(defun cider-open-result-window ()
  (interactive)
  (let (old-buf (current-buffer))
    (when (get-buffer "*cider-result*")
      (pop-to-buffer "*cider-result*")
      (switch-to-buffer-other-window old-buf))))


(defun lambdamacs/cider-eval-last-sexp-in-repl ()
  (interactive)
  (save-excursion
    (save-window-excursion
      (setq current-prefix-arg '(4)) ; C-u
      (call-interactively 'cider-insert-last-sexp-in-repl))))


(use-package cider
  :ensure t
  :defer t
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :bind (:map cider-mode-map
              ("C-c r" . cider-open-result-window)
              ("C-c j" . counsel-imenu)
              ("M-;"   . cider-eval-last-sexp-to-repl)
              ("M-:"   . lambdamacs/cider-eval-last-sexp-in-repl))
  :config
  (setq nrepl-log-messages t
        nrepl-hide-special-buffers t
        cider-repl-display-in-current-window nil
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t
        cider-repl-use-content-types t
        cider-save-file-on-load t
        cider-prompt-for-symbol nil
        cider-font-lock-dynamically '(macro core function var)
        cider-repl-buffer-size-limit 100000
        cider-overlays-use-font-lock t
        cider-dynamic-indentation nil
        cider-result-use-clojure-font-lock t
        cider-overlays-use-font-lock nil
        ;; disable Omit stack traces
        cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow"
        ;;cider-format-code-options '(("indents" ((".*" (("inner" 0))))))
        )
  (cider-repl-toggle-pretty-printing))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| C L O J U R E   L S P |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  (setq                                 ; recommended
   gc-cons-threshold (* 1024 1024 1024)
   read-process-output-max (* 1024 1024))

  (setq                                 ; features
   lsp-lens-enable nil
   lsp-lens-place-position 'end-of-line
   lsp-enable-indentation nil ;; use cider one
   lsp-semantic-tokens-enable t
   lsp-headerline-breadcrumb-enable nil
   lsp-signature-auto-activate '(:on-trigger-char :after-completion :on-server-request)
   ;;lsp-completion-provider
   )

  (setq                                 ; conflicting
   cljr-add-ns-to-blank-clj-files nil
   cider-eldoc-display-for-symbol-at-point nil
   lsp-enable-indentation nil)

  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))



;; toggle individual features
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable   nil
        ;; lsp-ui-doc-delay    0.2
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-include-signature t

        lsp-ui-doc-show-with-cursor nil ;; not sure
        lsp-ui-doc-show-with-mouse  nil ;; not sure

        lsp-ui-peek-enable t

        lsp-ui-sideline-show-code-actions nil

        ;; Optimization for large files
        lsp-file-watch-threshold 10000
        lsp-log-io nil))

(require 'lsp-mode)
(define-key lsp-mode-map (kbd "s-l h s") '("Show DOC" . lsp-ui-doc-show)) ;;  NOT FULLY WORKING
(define-key lsp-mode-map (kbd "s-l h h") '("Hide DOC" . lsp-ui-doc-hide))
(define-key lsp-mode-map (kbd "s-l h p") '("Show DOC in buffer" . lsp-describe-thing-at-point))
(define-key lsp-mode-map (kbd "s-l h q") '("Show Signature" . lsp-signature-activate))



(use-package lsp-ivy      :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-error-list)

(use-package cider-eval-sexp-fu
  :defer t)

;;(use-package clj-refactor
;;  :defer t
;;  :ensure t
;;  :diminish clj-refactor-mode
;;  :config (cljr-add-keybindings-with-prefix "C-c C-r"))



(use-package paredit
  :ensure t  ;; if you need it
  :commands (enable-paredit-mode)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'common-lisp-mode-hook      #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'clojure-mode-hook          #'enable-paredit-mode)))


;; ------------------------------------------------------------
;; REPL history navigation with up and down
;; ------------------------------------------------------------

;; Add more natural <up> and <down> key bindings for nrepl mode
(defun my-nrepl-mode-keys ()
  "Modify keymaps used by repl."
  (local-set-key (kbd "<up>")   'nrepl-previous-input)
  (local-set-key (kbd "<down>") 'nrepl-next-input))

(add-hook 'nrepl-mode-hook 'my-nrepl-mode-keys)



;; Add more natural <up> and <down> key bindings for nrepl mode
(defun my-cider-mode-keys ()
  "Modify keymaps used by repl."
  (local-set-key (kbd "<up>")   'cider-repl-previous-input)
  (local-set-key (kbd "<down>") 'cider-repl-next-input))

(add-hook 'cider-repl-mode-hook 'my-cider-mode-keys)



;; ------------------------------------------------------------
;; Clojure ligatures
;; ------------------------------------------------------------

;;
;; Clojure mode font-locking for partial
;; To disable font locking at startup add this in your init.el
;;
;;    (setq lambdamacs/clojure-disable-font-locking t).
;;
;; NOTE: to disable font locking once is activated run this
;;
;;    (fset 'old-font-lock-add-keywords (symbol-function 'font-lock-add-keywords)) ;
;;    (defalias 'font-lock-add-keywords 'ignore) ;
;;
;; And then reopen the buffer.
;; To restore run:
;;
;;    (fset 'font-lock-add-keywords 'old-font-lock-add-keywords) ;
;;
;; And then reopen the buffer.
(unless (bound-and-true-p lambdamacs/clojure-disable-font-locking)

  (defface clojure-font-locking-ligatures-face
    '((t :inherit font-lock-keyword-face))
      ;;(t :weight normal :foreground "#ff79c6"))

    "Face for highlighting  ligatures.")

  (defvar clojure-font-locking-ligatures-face 'clojure-font-locking-ligatures-face)

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(fn\\)[\[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "λ")
                                 clojure-font-locking-ligatures-face))))))

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("\\(#\\)("
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "ƒ")
                                 clojure-font-locking-ligatures-face))))))

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("\\(#\\){"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "∈")
                                 clojure-font-locking-ligatures-face))))))

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(partial\\)[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "Ƥ")
                                 clojure-font-locking-ligatures-face))))))

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(comp\\)[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "⨌")
                                 clojure-font-locking-ligatures-face))))))

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(and\\)[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "∧")
                                 clojure-font-locking-ligatures-face))))))

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(or\\)[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "∨")
                                 clojure-font-locking-ligatures-face))))))

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(for\\)[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "∀")
                                 clojure-font-locking-ligatures-face))))))

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(reduce \\+\\)[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "∑")
                                 clojure-font-locking-ligatures-face))))))

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(reduce \\*\\)[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "∏")
                                 clojure-font-locking-ligatures-face))))))

  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(reduce\\)[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "ℝ")
                                 clojure-font-locking-ligatures-face))))))

  ;; -> LONG RIGHTWARDS ARROW
  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(->\\)[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "⟶")
                                 clojure-font-locking-ligatures-face))))))

  ;; ->> LONG RIGHTWARDS DOUBLE ARROW
  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("(\\(->>\\)[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "⟹")
                                 clojure-font-locking-ligatures-face))))))

  ;; => LONG RIGHTWARDS DOUBLE ARROW FROM BAR
  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("[[:blank:]\n]\\(=>\\)[[:blank:]\n]"
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "⟾")
                                 clojure-font-locking-ligatures-face))))))

  ;; u/log, mu/log, u/log*, mu/log*
  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `(("[([:blank:]]\\(m?u\\)/\\(log\\*\\|log\\|trace\\)[[:blank:]\n]"
                       (1 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "μ")
                                 clojure-font-locking-ligatures-face)))))))



;;
;; Test the regex here
;;
;; (re-search-forward "[([:blank:]]\\(m?u\\)/\\(log\\*\\|log\\|trace\\)[[:blank:]\n]") ;
;;
;; (partial foo) ;
;;



;; ------------------------------------------------------------
;; Cider send expression to REPL buffer
;; ------------------------------------------------------------
(require 'cider)



;;
;; This sends a sexp to the REPL buffer
;; credits: http://timothypratley.blogspot.co.uk/2015/07/seven-specialty-emacs-settings-with-big.html
;;
(defun cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (cider-defun-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (set-buffer (cider-current-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))



;; TODO: fix
;(define-key cider-mode-map
;  (kbd "C-M-;") 'cider-eval-expression-at-point-in-repl)



(defun cider-eval-last-expression-in-repl ()
  (interactive)
  (let ((form (cider-last-sexp)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
      (setq form (replace-match "" t t form)))
    (set-buffer (cider-current-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))



;; TODO: fix
;(define-key cider-mode-map
;  (kbd "C-;") 'cider-eval-last-expression-in-repl)



;; ------------------------------------------------------------
;; Cider eval in PopUp fix
;; ------------------------------------------------------------

;;
;; Hack for cider-popup eval inspired by
;; from: https://github.com/clojure-emacs/cider/issues/2580#issuecomment-606708789
;;
(defun cider-popup-eval-handler (&optional buffer)
  "Make a handler for printing evaluation results in popup BUFFER.
This is used by pretty-printing commands."
  (nrepl-make-response-handler
   (or buffer (current-buffer))
   (lambda (buffer value)
     (cider-emit-into-popup-buffer buffer (ansi-color-apply value) nil t))
   (lambda (buffer out)
     (cider-emit-into-popup-buffer buffer (ansi-color-apply out) nil t))
   (lambda (buffer err)
     (cider-emit-into-popup-buffer buffer (ansi-color-apply err) nil t))
   nil
   nil
   nil
   (lambda (buffer warning)
     (cider-emit-into-popup-buffer buffer warning 'font-lock-warning-face t))))



;; ------------------------------------------------------------
;; Clojure formatting
;; ------------------------------------------------------------

;;
;; Clean Clojure code
;;
(defun clean-clojure-indent ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max) nil)
  (save-buffer))



(defun clean-clojure ()
  (interactive)
  (save-restriction
    (clean-clojure-indent)
    (let* ((pos     (point))
           (content (replace-regexp-in-string
                     ")\\s-*\n+\\((def[^ ]*\\|(comment\\|(facts?\\|(repl-test\\|;\\)"
                     ")\n\n\n\n\\1"
                     (buffer-string))))
      (erase-buffer)
      (insert content)
      (goto-char pos))))



(define-key clojure-mode-map
  (kbd "C-c C-l") 'clean-clojure)

(define-key cider-mode-map
  (kbd "C-c C-l") 'clean-clojure)



;;
;; cljfmt on save
;;
;;
;; I don't like clojure-mode indentation idea.  to format correctly the
;; code it requires a running repl so that it can inspect the
;; formatting rules for macro and style/indent hints.
;;
;; Instead I prefer a much simple, but general formatting whereas an
;; indented form is always 2 spaces over indipendently of the form
;; name.
;;
(defun cljfmt ()
  (when (and lambdamacs/cljfmt-reformat-on-save
         (or (eq major-mode 'clojure-mode)
             (eq major-mode 'clojurescript-mode)))
    (shell-command-to-string (format "cljfmt fix %s --indents ~/.lein/cljfmt-indents.clj --no-remove-surrounding-whitespace --no-remove-consecutive-blank-lines" buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))

(add-hook 'after-save-hook #'cljfmt)



(defun cljfmt-toggle-reformat ()
  (interactive)
  ;; toggle the value
  (setq lambdamacs/cljfmt-reformat-on-save
        (not lambdamacs/cljfmt-reformat-on-save))
  (message "cljfmt is now %s." (if lambdamacs/cljfmt-reformat-on-save "enabled" "disabled")))



;; ------------------------------------------------------------
;; Code boxes
;; ------------------------------------------------------------
(defun -pad-center (str len char)
  (store-substring (make-string len char) (/ (- len (length str)) 2) str))



(defun -trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))



(defun comment-box (title)
  (let* ((size 80)
         (norm-title (upcase
                      (-trim-string
                       (replace-regexp-in-string "\\(.\\)" "\\1 " title))))
         (decor-title (concat "----==| " norm-title " |==----")))
    (cl-flet ((str-repeat (size char) (make-string size (string-to-char char))))
      (concat "\n"
              (str-repeat 80 ";") "\n"
              ";;" (str-repeat (- size 4) " ") ";;\n"
              ";;" (-pad-center decor-title (- size 4) ? ) ";;\n"
              ";;" (str-repeat (- size 4) " ") ";;\n"
              (str-repeat 80 ";") "\n"))))



(defun my-comment-box ()
  "Convert word at point (or selected region) to code box"
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'line)))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (comment-box text)))))

(global-set-key (kbd "s-b") 'my-comment-box)



;;
;; delete space but one like emacs-live
;;
(defun live-delete-whitespace-except-one ()
  (interactive)
  (just-one-space -1))
(global-set-key (kbd "M-SPC ") 'live-delete-whitespace-except-one)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| I N F - C L O J U R E |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package inf-clojure)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;         ----==| J S O N / E D N / T R A N S I T / Y A M L |==----          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package jet)
