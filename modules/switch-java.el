;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                ----==| S W I T C H - J A V A . E L |==----                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; switch java
;;; switch the JVM used when running java/jvm tools
;;; http://blog.brunobonacci.com/2020/07/02/switching-between-multiple-jdk-in-emacs/
;;;

;; requires:
;;(setq JAVA_BASE "/Library/Java/JavaVirtualMachines")

;;
;; This function returns the list of installed
;;
(defun switch-java--versions ()
  "Return the list of installed JDK."
  (seq-remove
   (lambda (a) (or (equal a ".") (equal a "..")))
   (directory-files JAVA_BASE)))


(defun switch-java--save-env ()
  "Store original PATH and JAVA_HOME."
  (when (not (boundp 'SW_JAVA_PATH))
    (setq SW_JAVA_PATH (getenv "PATH")))
  (when (not (boundp 'SW_JAVA_HOME))
    (setq SW_JAVA_HOME (getenv "JAVA_HOME"))))


(defun switch-java ()
  "List the installed JDKs and enable to switch the JDK in use."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (switch-java--save-env)

  (let ((ver (completing-read
              "Which Java: "
              (seq-map-indexed
               (lambda (e i) (list e i)) (switch-java--versions))
              nil t "")))
    ;; switch java version
    (setenv "JAVA_HOME" (concat JAVA_BASE "/" ver "/Contents/Home"))
    (setenv "PATH" (concat (concat (getenv "JAVA_HOME") "/bin/java")
                           ":" SW_JAVA_PATH)))
  ;; show version
  (switch-java-which-version?))


(defun switch-java-default ()
  "Restore the default Java version."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (switch-java--save-env)

  ;; switch java version
  (setenv "JAVA_HOME" SW_JAVA_HOME)
  (setenv "PATH" SW_JAVA_PATH)
  ;; show version
  (switch-java-which-version?))


(defun switch-java-which-version? ()
  "Display the current version selected Java version."
  (interactive)
  ;; displays the current java version
  (message (concat "Java HOME: " (getenv "JAVA_HOME"))))


;;
;; Version used in (my) perf measurement
;;
;; TODO: remove this
(defun switch-java8-perf ()
  (interactive)
  ;; store original PATH and JAVA_HOME
  (switch-java--save-env)

  (setenv "JAVA_HOME" (concat JAVA_BASE "/graalvm-ce-java08-19.3.0.2/Contents/Home"))
  (setenv "PATH" (concat (concat (getenv "JAVA_HOME") "/bin/java") ":" SW_JAVA_PATH))
  ;; show version
  (switch-java-which-version?))


;;; switch-java.el ends here
