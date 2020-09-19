;;
;;
;;
;;      ***                                                              ***
;;      **    #####                                                        **
;;     **        ###                                                       **.
;;     **        ####      ////// /////    //////     //////    //////     ***
;;     **       ######     ///  ///  //*  ///   //   ///  ///  ///  ///    ***
;;     ***     ###  ##     //   ///  ///    ///////  //   //    ////       ***
;;      **    ###   ####   //   //*  /// ///    ///  //    /// //   ////   **
;;      ***  ###      ##   //        ///   ,///////    ////      /////   .**
;;
;;
;;
;;; Code:

;; load logo.
(find-file-read-only
 (expand-file-name "assets/lambdamacs.txt" user-emacs-directory))

;;
;; Initialize package sources
(require 'package)
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(unless (file-exists-p package-user-dir)
    (make-directory package-user-dir))

;; Always load newest byte code
(setq load-prefer-newer t)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;;
;;  Initialize use-package on non-Linux platforms
;;
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; add use pacakge and set gloabl `:ensure t`
(require 'use-package)
(setq use-package-always-ensure t)


(setq lambdamacs-dir         (expand-file-name "src" user-emacs-directory))
(setq lambdamacs-conf-dir    (expand-file-name "config" user-emacs-directory))
(setq lambdamacs-modules-dir (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path lambdamacs-dir)
(add-to-list 'load-path lambdamacs-conf-dir)
(add-to-list 'load-path lambdamacs-modules-dir)

(setq custom-file (expand-file-name "emacs-custom.el" lambdamacs-conf-dir))

(defun loadx (file)
  "load a configuration file if it exists."
  (if (file-exists-p (expand-file-name file lambdamacs-conf-dir))
      (load-file (expand-file-name file lambdamacs-conf-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;           ----==| L O A D   C O N F I G U R A T I O N S |==----            ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load  "user-config.el")    ;; Load user preferences
(loadx "custom-config.el")  ;; load custom configuration if present
(load  "look-n-feel.el")    ;; visual aspects
(load  "editor.el")         ;; editor settings
(load  "general.el")        ;; general settings
(load  "dev.el")            ;; common dev tools
(load  "dev-clojure.el")    ;; clojure settings
(load  "dev-java.el")       ;; settings for java development
(load  "productivity.el")   ;; org-mode and productivity tools
(loadx "post-init.el")      ;; load custom post configuration if present
(loadx "emacs-custom.el")   ;; load emacs customizations


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                     ----==| R E C O M P I L E |==----                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; recompile all if not compiled
;;
;;(if (not (file-exists-p (concat buffer-file-name "c")))
;;    (byte-compile-all-init-dir))

;;; init.el ends here
