;;
;; User preferences and personal configuration
;;

;; add your name and email address
;; (setq user-full-name    "Firstname Lastname"
;;       user-mail-address "your-email@email.org")

;; system-wide font size
(setq lambdamacs/default-font "Roboto Mono Thin for Powerline")
;;(setq lambdamacs/default-font "Victor Mono")
;;(setq lambdamacs/default-font "Fira Code")
(setq lambdamacs/default-font-size 150)

;; themes to use
;; suggestion: choose a dark and a light one, you will be switch between them
;; using `M-x switch-theme-to-default'
;; and `~M-x switch-theme-to-alternative'
(setq lambdamacs/default-theme 'doom-moonlight)
(setq lambdamacs/alternative-theme 'doom-one-light)

;; directory where to save temp stuff
(setq lambdamacs/save-place ".save")

;; Clojure ligatures are enabled by default,
;; if you find them distracting you can disable them here.
;;(setq lambdamacs/clojure-disable-font-locking t)


;; The way Cider formats the code isn't particularly nice.  To format
;; the code properly you need to have a repl running and the code
;; needs to be loaded. Because of this the formatting changes
;; depending on which state your IDE is. I think it is a bad idea.  I
;; rather have a less appealing formatting, but which can be done
;; without having to load/compile the code.  (λmacs) performs a
;; reformatting of the code on save with a definition which doesn't
;; take into account which particular form you are in.  However there
;; are times this is not convenient, like when working on someone else
;; code. To disable the code-reformat set the following variable to
;; `nil' You can toggle the value with `M-x cljfmt-toggle-reformat`.
(setq lambdamacs/cljfmt-reformat-on-save nil)


;; base directory where all the JDK versions are installed
;; use `M-x switch-java' to select the JVM to use
(setq JAVA_BASE "/Library/Java/JavaVirtualMachines")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;             ----==| W I N D O W   M A N A G E M E N T |==----              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)


(defun as-regexp (string-list)
  (->> string-list
       (-map (lambda (s) (replace-regexp-in-string "\\*" "\\\\*" s)))
       (-map (lambda (s) (replace-regexp-in-string "\\?" "\\\\?" s)))
       (-map (lambda (s) (replace-regexp-in-string "\\[" "\\\\[" s)))
       (-map (lambda (s) (replace-regexp-in-string "\\]" "\\\\]" s)))
       ((lambda (it) (mapconcat 'identity it "\\|")))
       ((lambda (it) (concat "\\(" it "\\)")))))



(defun lambdamacs/win-reset ()
  (interactive)
  (setq display-buffer-alist nil))



;; test with
;;; (string-match (as-regexp '("magit-diff:")) "magit-diff: foo.clj")


(defun lambdamacs/win-dev-mode-large ()
  (interactive)

  (setq display-buffer-alist
        `((,(as-regexp '("cider-clojuredocs" "cider-doc" "cider-error"
                          "Warnings" "Help" "help" "*scratch*" "Backtrace"
                          "magit:" "ansi-term" "info" "Kill Ring" "sesman CIDER"))
            (display-buffer-in-side-window)
            (window-width . 0.20)
            (preserve-size . (t . nil))
            (side . right)
            (slot . -1))

          (,(as-regexp '("magit-diff:"))
            (display-buffer-in-side-window)
            (window-width . 0.20)
            (preserve-size . (t . nil))
            (side . right)
            (slot . 1))


          (,(as-regexp '("cider-result"))
            (display-buffer-in-side-window)
            (window-width . 0.20)
            (preserve-size . (t . nil))
            (side . left)
            (slot . -1))

          (,(as-regexp '("cider-macroexpansion"))
            (display-buffer-in-side-window)
            (window-width . 0.20)
            (preserve-size . (t . nil))
            (side . left)
            (slot . 0))

          ( ,(as-regexp '("cider-repl"))
            (display-buffer-in-side-window)
            (window-width . 0.20)
            (preserve-size . (t . nil))
            (side . left)
            (slot . 1)))))



(defun lambdamacs/win-dev-mode-large2 ()
  (interactive)

  (setq display-buffer-alist
        `(;; cider repl, results and macroexpand
          (,(as-regexp '("cider-result"))
            (display-buffer-in-side-window)
            (window-width . 0.20)
            (preserve-size . (t . nil))
            (side . left)
            (slot . -1))

          (,(as-regexp '("cider-macroexpansion"))
            (display-buffer-in-side-window)
            (window-width . 0.20)
            (preserve-size . (t . nil))
            (side . left)
            (slot . 0))

          (,(as-regexp '("cider-repl"))
            (display-buffer-in-side-window)
            (window-width . 0.20)
            (preserve-size . (t . nil))
            (side . left)
            (slot . 1))

          ;; dev modes as central windows
          ((mode . (cider-mode
                    clojure-mode
                    lisp-mode
                    emacs-lisp-mode
                    java-mode
                    javascript-mode
                    c-mode
                    prog-mode))
           (display-buffer-reuse-window)
           (window-width . 0.20)
           (preserve-size . (t . nil))
           (side . left)
           (slot . -1))

          ;; if this the previous doesn't work,
          ;;; try this
          (,(as-regexp '("\\.clj"  "\\.cljc" "\\.cljs" "\\.edn"
                         "\\.java" "\\.js"   "\\.json" "\\.org"
                         "\\.xml"  "\\.el"))
           (display-buffer-reuse-window)
           (window-width . 0.20)
           (preserve-size . (t . nil))
           (side . left)
           (slot . -1))

          ;; help & doc in side windows
          ;; without switching
          ((mode . (helpful-mode help-mode))
           (display-buffer-in-side-window)
           (inhibit-switch-frame . t)
           (window-width . 0.20)
           (preserve-size . (t . nil))
           (side . right)
           (slot . -1))

          (,(as-regexp '("cider-clojuredocs" "cider-doc" "cider-error"))
           (display-buffer-in-side-window)
           (inhibit-switch-frame . t)
           (window-width . 0.20)
           (preserve-size . (t . nil))
           (side . right)
           (slot . -1))

          ;; so that it won't overlap with the commit msg
          (,(as-regexp '("magit-diff:"))
            (display-buffer-in-side-window)
            (window-width . 0.20)
            (preserve-size . (t . nil))
            (side . right)
            (slot . 1))

          ;; everything else on the right side
          ;; out of my way
          (".*"
           (display-buffer-in-side-window)
           (window-width . 0.20)
           (preserve-size . (t . nil))
           (side . right)
           (slot . -1))
          )))




;;; end user-config.el
