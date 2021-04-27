;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                  ----==| P R O D U C T I V I T Y |==----                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                      ----==| O R G - M O D E |==----                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; .TODO.org
;;
;; automatically open .TODO.org scripts at startup
(setq default-message
      "
* org-mode is a great tool, use it for your own notes (press TAB on this line)
** org-mode key-bindings http://orgmode.org/orgcard.txt
** Documentation: http://orgmode.org/
** Good cheatsheet: http://orgmode.org/orgcard.pdf
** Video tutorials:
  - https://www.youtube.com/watch?v=VcgjTEa0kU4
  - https://www.youtube.com/watch?v=PNE-mgkZ6HM
  - https://www.youtube.com/watch?v=0g9BcZvQbXU
  - https://vimeo.com/15269391
  - https://www.youtube.com/watch?v=6W82EdwQhxU
  - https://www.youtube.com/watch?v=fgizHHd7nOo
  - https://www.youtube.com/watch?v=bzZ09dAbLEE
")

;; if not exists create one
(if (not (file-exists-p "~/.TODO.org"))
    (append-to-file default-message nil "~/.TODO.org"))
;; open all existing ones
(mapcar 'find-file  (directory-files "~/" t "^.TODO.*.org"))



;;
;; Autogenerate html on-save for org-mode
;;
(defun org-autogenerate-html-on-save ()
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (message "Enabled org html export on save for current buffer...")))


;;
;; Better bullets
;;
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;;
;; Allow image resize on inline preview
;; and other visual customisations
;;
(setq org-image-actual-width nil)
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-startup-folded 'showeverything) ;; showeverything
(setq org-startup-indented 't)    ;; indent
(setq org-hide-block-startup nil) ;; showblocks
(setq org-hide-leading-stars nil) ;; showstars
(setq org-ellipsis " ⤵")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| H T M L   E X P O R T |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Don't ask permissions to export as HTML file on save
;;
(add-to-list 'safe-local-variable-values
             '(after-save-hook . org-html-export-to-html))


;;
;; Export to HTML options
;;
;;
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)
(setq org-export-with-section-numbers nil)
(setq org-html-head "
<link rel=\"stylesheet\" type=\"text/css\" href=\"https://rawcdn.githack.com/BrunoBonacci/org-doc/master/assets/GTD.css\" />
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/styles/github.min.css\">

<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/highlight.min.js\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/languages/clojure.min.js\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/languages/clojure-repl.min.js\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/languages/java.min.js\"></script>
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/languages/bash.min.js\"></script>
<script>hljs.highlightAll();</script>")
(setq org-link-file-path-type "relative")


;; Hack to support syntax highlighing
;;
;; taken from: https://emacs.stackexchange.com/a/9838
(defun rasmus/org-html-wrap-blocks-in-code (src backend info)
  "Wrap a source block in <pre><code class=\"lang\">.</code></pre>"
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string
     "\\(</pre>\\)" "</code>\n\\1"
     (replace-regexp-in-string "<pre class=\"src src-\\([^\"]*?\\)\">"
                               "<pre class=\"src src-\\1\">\n<code class=\"\\1\">\n" src))))

(add-to-list 'org-export-filter-src-block-functions
             'rasmus/org-html-wrap-blocks-in-code)

(setq org-html-htmlize-output-type nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                    ----==| O R G - A G E N D A |==----                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (file-exists-p "~/org")
    (setq org-agenda-files (directory-files "~/org" t ".*\.org$")))

(setq org-todo-keywords
    '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")))


(advice-add 'org-deadline       :after (lambda (&rest _) (org-save-all-org-buffers)))
(advice-add 'org-schedule       :after (lambda (&rest _) (org-save-all-org-buffers)))
(advice-add 'org-store-log-note :after (lambda (&rest _) (org-save-all-org-buffers)))
(advice-add 'org-todo           :after (lambda (&rest _) (org-save-all-org-buffers)))

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)

(setq org-export-coding-system 'utf-8)
(setq org-default-notes-file "~/org/tasks.org")
(setq org-capture-templates
      '(("t" "Generic TODO item (scheduled)"
         entry
         (file org-default-notes-file)
         "* TODO (%^{SIZE[0-9]}) %? %^g\n  SCHEDULED: %^t\n  %i")

        ("d" "Deadline)"
         entry
         (file org-default-notes-file)
         "* TODO (%^{SIZE[0-9]}) %? %^g\n  DEADLINE: %^t\n  %i")

        ("l" "Linked TODO item"
         entry
         (file org-default-notes-file)
         "* TODO (%^{SIZE[0-9]}) %? %^g\n  SCHEDULED: %^t\n  %i\n  %a")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                     ----==| O R G - B A B E L |==----                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package ob-http)
(use-package ob-restclient)

;; which languages you can run
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (org . t)
    (shell . t)
    (clojure . t)
    (dot . t)
    (http . t)
    (restclient . t)
    (latex . t)))

;; don't ask for confirmation
(setq org-confirm-babel-evaluate nil)

(push '("properties" . conf-unix) org-src-lang-modes)
(push '("conf" . conf-unix) org-src-lang-modes)

;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-clojure.html
;; https://git.jeremydormitzer.com/jdormit/dotfiles/src/commit/5f9dbe53cea2b37fc89cc49f858f98387da99576/emacs/init.org
  (with-eval-after-load 'ob-clojure
    (defcustom org-babel-clojure-backend nil
      "Backend used to evaluate Clojure code blocks."
      :group 'org-babel
      :type '(choice
              (const :tag "inf-clojure" inf-clojure)
              (const :tag "cider" cider)
              (const :tag "slime" slime)
              (const :tag "bb" bb)
              (const :tag "Not configured yet" nil)))

    (defun elisp->clj (in)
      (cond
       ((listp in) (concat "[" (s-join " " (mapcar #'elisp->clj in)) "]"))
       (t (format "%s" in))))

    (defun ob-clojure-eval-with-bb (expanded params)
      "Evaluate EXPANDED code block with PARAMS using babashka."
      (unless (executable-find "bb")
        (user-error "Babashka not installed"))
      (let* ((stdin (let ((stdin (cdr (assq :stdin params))))
                      (when stdin
                        (elisp->clj
                         (org-babel-ref-resolve stdin)))))
             (input (cdr (assq :input params)))
             (file (make-temp-file "ob-clojure-bb" nil nil expanded))
             (command (concat (when stdin (format "echo %s | " (shell-quote-argument stdin)))
                              (format "bb %s -f %s"
                                      (cond
                                       ((equal input "edn") "")
                                       ((equal input "text") "-i")
                                       (t ""))
                                      (shell-quote-argument file))))
             (result (shell-command-to-string command)))
        (s-trim result)))

    (defun org-babel-execute:clojure (body params)
      "Execute a block of Clojure code with Babel."
      (unless org-babel-clojure-backend
        (user-error "You need to customize org-babel-clojure-backend"))
      (let* ((expanded (org-babel-expand-body:clojure body params))
             (result-params (cdr (assq :result-params params)))
             result)
        (setq result
              (cond
               ((eq org-babel-clojure-backend 'inf-clojure)
                (ob-clojure-eval-with-inf-clojure expanded params))
               ((eq org-babel-clojure-backend 'cider)
                (ob-clojure-eval-with-cider expanded params))
               ((eq org-babel-clojure-backend 'slime)
                (ob-clojure-eval-with-slime expanded params))
               ((eq org-babel-clojure-backend 'bb)
                (ob-clojure-eval-with-bb expanded params))))
        (org-babel-result-cond result-params
          result
          (condition-case nil (org-babel-script-escape result)
            (error result)))))

    (customize-set-variable 'org-babel-clojure-backend 'bb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| S P E L L - C H E C K |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Apparently this is part of Emacs default so no need to install it.
;;
(setq ispell-program-name "aspell"      ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(flyspell-mode +1)



(use-package flycheck
  :init
  (setq flycheck-temp-prefix (expand-file-name "flycheck" temporary-file-directory))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                      ----==| G R A P H V I Z |==----                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 2)
  (setq graphviz-dot-view-edit-command nil)
  (setq graphviz-dot-view-command "dot -Tpng %s")
  (setq graphviz-dot-save-before-view t))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                    ----==| C A R B O N - N O W |==----                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package carbon-now-sh
  :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| G O O G L E - T H I S |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package google-this
  :init
  (setq google-this-keybind (kbd "s-g"))
  (google-this-mode 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                    ----==| O R G - T R E L L O |==----                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-trello)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;          ----==| W E B - S E Q U E N C E - D I A G R A M |==----           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Installing WSD-mode
;;

(use-package wsd-mode
  :init
  (setq wsd-style "roundgreen")
  (setq wsd-style-altern "napkin")
  :bind (:map wsd-mode-map
              ("C-c C-a" . #'wsd-show-diagram-inline-alternative)))

(defun wsd-show-diagram-inline-alternative ()
  (interactive)
  (let*
      ((wsd-style-temp wsd-style))
    (setq wsd-style wsd-style-altern)
    (wsd-show-diagram-inline)
    (setq wsd-style wsd-style-temp)))



;;; productivity ends here
