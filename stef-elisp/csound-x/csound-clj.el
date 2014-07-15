;;; -*- auto-recompile: t -*-

;;; csound-clj.el --- csound API with Clojure

;; Keywords: csound

;; This file is not part of GNU Emacs.
;; 
;; csound-clj.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; csound-clj.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Author: Stéphane Rollandin <hepta@zogotounga.net>
;;---------

(require 'clojure-mode)
(require 'nrepl)

(defvar clojure-home (surmulot-directory "clojure")) 

(defvar clojure-environment-set-p nil)

(defvar clojure-connected-p nil)

(defun surmulot-setup-clojure-environment ()
  (unless clojure-environment-set-p
    (setenv "PATH" (concat (surmulot-directory "lein") ";" (getenv "PATH")))
    (setenv "CLASSPATH" 
	    (concat (mapconcat 'identity
			       csclj-surmulot-jars ";")
		    ";" (getenv "CLASSPATH")))
    (setenv "LEIN_JAR" 
	  (expand-file-name "leiningen-2.0.0-standalone.jar" 
			    (surmulot-directory "lein")))
    (unless (eq system-type 'gnu/linux)
      (setenv "PATH" (concat csclj-csound-path ";" (getenv "PATH")))))
  (setq clojure-environment-set-p t))

(add-hook 'nrepl-connected-hook 'surmulot-clojure-load-init-file)
(add-hook 'nrepl-connected-hook 'surmulot-clojure-note-connected)
(add-hook 'nrepl-disconnected-hook 'surmulot-clojure-note-disconnected)

(defun surmulot-clojure-note-connected ()
  (setq clojure-connected-p t))

(defun surmulot-clojure-note-disconnected ()
  (setq clojure-connected-p nil))

(add-to-list 'special-display-buffer-names "*nrepl*")

(defun surmulot-start-clojure (&optional form)
  (interactive)
  (unless clojure-connected-p
    (when form
      (add-hook 'nrepl-connected-hook 
		`(lambda ()
		   (surmulot-clojure-eval ',form t)
		   (setq nrepl-connected-hook (butlast nrepl-connected-hook 1)))
		t))
    (surmulot-setup-clojure-environment)
    (nrepl-jack-in)))

(defun surmulot-clojure-load-init-file ()
  (surmulot-clojure-eval `(load-file ,csclj-init-file)))

(defun surmulot-clojure-eval (form &optional no-test)
  (if clojure-connected-p
      (nrepl-send-string-sync (prin1-to-string form))
    (unless no-test
      (surmulot-start-clojure form))))

(defun surmulot-slider-demo ()
  (interactive)
  (surmulot-clojure-eval '(def gui (slider-demo))))

; 2)
;(surmulot-clojure-eval2 '(load-file (surmulot-file ["clojure" "draft.clj"])))

; (surmulot-clojure-eval2 '(csound-play (surmulot-file ["csound" "library" "examples" "Study for Computer (John Endicott).csd"])))

;(surmulot-clojure-eval2 '(emacs-do "(new-frame)"))

;; NOTE:
;; there is a CLASSPATH=%CLASSPATH% in lein.bat  Needed !



;; ------- settings to be improved 

(defvar csclj-csound-path 
  (if (eq system-type 'gnu/linux) 
      ""
    (surmulot-csound-directory "bin")))

(defvar csclj-csnd-jar 
 (if (eq system-type 'gnu/linux) 
      "/usr/share/java/csnd-5.08.0.jar"  ;;; make this dynamic or customizable
   (expand-file-name "csnd.jar" csclj-csound-path)))

(defvar csclj-csnd-library-path 
  (if (eq system-type 'gnu/linux) 
      "/usr/lib/jni/")) ;"/usr/lib/jni/lib_jcsound.so"))

(defvar csclj-surmulot-jars
  (list  
   (expand-file-name "leiningen-2.0.0-standalone.jar" 
		     (surmulot-directory "lein"))
   (expand-file-name "clojure-1.5.1.jar" clojure-home)
   csclj-csnd-jar
   (surmulot-directory "jmusic/jmusic.jar")
   (surmulot-directory "jfugue/jfugue.jar")))

;; --------


(defvar csclj-init-file 
  (surmulot-directory "clojure/surmulot.clj"))

;; (add-to-list 'special-display-buffer-names "*slime-repl clojure*")

;; (defun surmulot-start-clojure (&optional init-form)
;;   (interactive)
;;   (require 'swank-clojure-autoload)
;;   ;;
;;   ;; ugly, make this dynamic and versatile (how ?)
;;   (unless (eq system-type 'gnu/linux)
;;     (setenv "PATH" (concat csclj-csound-path ";" (getenv "PATH"))))
;;   ;;
;;   (swank-clojure-config
;;    (slime-setup '(slime-repl))
;;    (setq swank-clojure-jar-path (expand-file-name "clojure.jar" clojure-home))
;;    (when csclj-csnd-library-path
;;      (add-to-list 'swank-clojure-library-paths csclj-csnd-library-path))
;;    (setq swank-clojure-extra-classpaths 
;; 	 (append csclj-surmulot-jars (list csclj-csnd-jar))))
;;   (require 'slime)
;;   (setq slime-repl-banner-function 
;; 	(lambda () (insert ";--- Clojure interaction ---")))
;;   (save-excursion 
;;     (save-window-excursion (slime)))
;;   (while (not (slime-connected-p))
;;     (sit-for 1))
;;   (surmulot-clojure-eval `(load-file ,csclj-init-file)))

;;; --------------------------- experimental  -----------------------------

;; (defun surmulot-slider-demo ()
;;   (interactive)
;;   (unless (and (featurep 'slime) 
;; 	       (slime-connected-p))
;; 	(surmulot-start-clojure))
;;   (surmulot-clojure-eval '(def gui (slider-demo))))


;(surmulot-clojure-eval '(load-file "draft.clj"))
; (csound-play "../csound/library/examples/study.csd")
; (csound-play "d:/devel/surmulot/csound/library/examples/study.csd")

;(surmulot-clojure-eval '(emacs-do "(new-frame)"))

;; ================== this is it.
(provide 'csound-clj)

;; csound-clj.el ends here
