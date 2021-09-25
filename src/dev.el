;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;     ----==| G E N E R A L   D E V E L O P M E N T   T O O L S |==----      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Git and Github packages
;;
(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)))
;; manage PRs
(use-package forge)

(use-package git-timemachine)

(use-package github-review)

;;
;; Open the current project in the browser (on Github)
;; Credit @dotemacs: https://gist.github.com/dotemacs/9a0433341e75e01461c9
;;
(defun parse-url (url)
  "convert a git remote location as a HTTP URL"
  (if (string-match "^http" url)
      url
    (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                              "https://\\2/\\3"
                              url)))


(defun magit-open-repo ()
  "open remote repo URL"
  (interactive)
  (let ((url (magit-get "remote" "origin" "url")))
    (progn
      (browse-url (parse-url url))
      (message "opening repo %s" url))))


(add-hook 'magit-mode-hook
          (lambda ()
            (local-set-key (kbd "C-o") 'magit-open-repo)))


;;
;; Browse selected file or lines on github.
;;
(use-package browse-at-remote
  :init
  (global-set-key (kbd "C-c C-w") 'browse-at-remote)
  (global-set-key (kbd "C-c C-S-w")   'browse-at-remote-kill))


;; for org knowledge base
(use-package git-auto-commit-mode)

;;
;; Don't ask permissions to export as HTML file on save
;;
(add-to-list 'safe-local-variable-values
             '(eval . git-auto-commit-mode))
(add-to-list 'safe-local-variable-values
             '(gac-automatically-push-p . t))
(add-to-list 'safe-local-variable-values
             '(gac-automatically-push-p . t))


;;
;; config projectile
;;
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  ;; -L allows to follow symlinks
  (setq projectile-generic-command "fd . -L -0 --type f --color=never")
  ;; seems the only one working with symlinks
  ;; (setq projectile-indexing-method 'native)
  )


;;
;; Faster searches with the_silver_searcher
;;
(use-package ag)


(use-package helm-ag
  :custom
  (helm-ag-base-command "ag --nocolor --nogroup --ignore-case --ignore-dir target")
  (helm-ag-command-option "") ;;--all-text not working in conjuction with --ignore-dir
  (helm-ag-insert-at-point 'symbol))

;;
;; The following snippet tells Projectile to use Helm-ag for project searches
;; (C-c p s s)
;; The `helm-projectile' version is better as it is fully interactive.
;;

;; taken from: https://github.com/bbatsov/helm-projectile/blob/master/helm-projectile.el
;; Thanks @bbastov
(defun helm-projectile-ag (&optional options)
  "Helm version of `projectile-ag'."
  (interactive (if current-prefix-arg (list (helm-read-string "option: " "" 'helm-ag--extra-options-history))))
  (if (require 'helm-ag nil t)
      (if (projectile-project-p)
          (let* ((grep-find-ignored-files (cl-union (projectile-ignored-files-rel) grep-find-ignored-files))
                 (grep-find-ignored-directories (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))
                 (ignored (mapconcat (lambda (i)
                                       (concat "--ignore " i))
                                     (append grep-find-ignored-files grep-find-ignored-directories (cadr (projectile-parse-dirconfig-file)))
                                     " "))
                 (helm-ag-base-command (concat helm-ag-base-command " " ignored " " options))
                 (current-prefix-arg nil))
            (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
        (error "You're not in a project"))
    (when (yes-or-no-p "`helm-ag' is not installed. Install? ")
      (condition-case nil
          (progn
            (package-install 'helm-ag)
            (helm-projectile-ag options))
        (error (error "`helm-ag' is not available.  Is MELPA in your `package-archives'?"))))))


(define-key projectile-mode-map [remap projectile-ag] #'helm-projectile-ag)
(def-projectile-commander-method ?A
    "Find ag on project."
    (call-interactively 'helm-projectile-ag))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| S W I T C H - J A V A |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; switch-java - select the jvm version
;; M-x switch-java
(load "switch-java.el")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                      ----==| M A R K D O W N |==----                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;
;; ability to generate Table of Contents
;;
(use-package markdown-toc)


;;
;; Live preview while editing
;;
(use-package flymd
  :config
  (defun my-flymd-browser-function (url)
    (let ((process-environment (browse-url-process-environment)))
      (apply 'start-process
             (concat "firefox " url)
             nil
             "/usr/bin/open"
             (list "-a" "firefox" url))))
  (setq flymd-browser-open-function 'my-flymd-browser-function))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                     ----==| Y A S N I P P E T |==----                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet                  ; Snippets
  :config
  (setq
   yas-verbosity 1                      ; No need to be so verbose
   yas-wrap-around-region t)

  (add-to-list 'yas-snippet-dirs 'yasnippet-snippets-dir t)
  (add-to-list 'yas-snippet-dirs (expand-file-name "yas" user-emacs-directory) t)

  (yas-reload-all)
  (yas-global-mode))

;;
;; Collection of snippets
;;
(use-package yasnippet-snippets)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                    ----==| R E S T C L I E N T |==----                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package restclient)

;;
;; restclient company--auto-completion
;;
(use-package company-restclient
  :config
  (add-to-list 'company-backends 'company-restclient)
  (setq auto-mode-alist
      (append '(("\\.rest\\'" . restclient-mode)) auto-mode-alist)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                     ----==| T E R R A F O R M |==----                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package terraform-mode
  :config
  (setq terraform-indent-level 2)
  :bind (:map terraform-mode-map
              ("C-c j" . helm-imenu)))
