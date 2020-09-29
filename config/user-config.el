;;
;; User preferences and personal configuration
;;

;; add your name and email address
;; (setq user-full-name    "Firstname Lastname"
;;       user-mail-address "your-email@email.org")

;; system-wide font size
(setq lambdamacs/default-font "Roboto Mono Thin for Powerline") ;;"Fira Code Retina"
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
(setq lambdamacs/cljfmt-reformat-on-save t)


;; base directory where all the JDK versions are installed
;; use `M-x switch-java' to select the JVM to use
(setq JAVA_BASE "/Library/Java/JavaVirtualMachines")
