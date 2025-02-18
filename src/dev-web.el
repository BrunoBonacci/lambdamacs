;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                  ----==| H T M L   A N D   C S S |==----                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consider skewer-mode
;; https://github.com/skeeto/skewer-mode



(use-package impatient-mode
  :ensure t
  :init
  (setq httpd-port 8888)
  (defun impatient-start (&optional arg)
    "Attach a browser to Emacs for a skewer JavaScript REPL. Uses
`browse-url' to launch a browser."
    (interactive "p")
    (httpd-start)
    (impatient-mode)
    (browse-url (format "http://127.0.0.1:%d/imp/" httpd-port)))
  (defun impatient-stop (&optional arg)
    "Attach a browser to Emacs for a skewer JavaScript REPL. Uses
`browse-url' to launch a browser."
    (interactive "p")
    (httpd-stop)))



(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  )
