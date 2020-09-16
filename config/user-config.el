;;
;; User preferences and personal configuration
;;


(setq user-full-name "Firstname Lastname"
      user-mail-address "your-email@email.org")



;; system-wide font size
(setq lambdamacs/default-font-size 150)
(setq lambdamacs/default-font "Roboto Mono Thin for Powerline") ;;"Fira Code Retina"
(setq lambdamacs/default-theme 'doom-moonlight)
(setq lambdamacs/alternative-theme 'doom-moonlight)

;; directory where to save temp stuff
(setq lambdamacs/save-place ".save")


;; disable clojure ligatures
;;(setq lambdamacs/clojure-disable-font-locking t)

;; base directory where all the JDK versions are installed
(setq JAVA_BASE "/Library/Java/JavaVirtualMachines")
