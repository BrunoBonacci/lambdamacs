;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                  ----==| P R O D U C T I V I T Y |==----                   ;;
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
;; Allow image resize on inline preview
;;
(setq org-image-actual-width nil)
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)

;;
;; Don't ask permissions to export as HTML file on save
;;
(add-to-list 'safe-local-variable-values
             '(after-save-hook . org-html-export-to-html))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                   ----==| S P E L L - C H E C K |==----                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Apparently this is part of Emacs default so no need to install it.
;;
(setq ispell-program-name "aspell" ; use aspell instead of ispell
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
