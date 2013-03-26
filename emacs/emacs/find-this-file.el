;; Copyright (C) 2002
;; Patrick Percot
;; ppercot at free.fr
;; Gnupg key: F64F1F8F
;; You can load my public key at "pgp.mit.edu"

;; Contributions from Matthieu Moy <Matthieu.Moy@imag.fr>
;;
;;

;; This software can be redistributed as specified in GNU General Public License

;; ChangeLog:
;; Version 1.7:
;;  - First version maintained by Matthieu MOY
;;
;; Version 1.6 and previous: No changelog available :-(

;; Copy this file in your .emacs file, or just put the file in your
;; `load-path' and add (require 'find-this-file) in your .emacs.

;; Two key bindings are loaded :
;; C-x a f: ltf-find-file
;; C-x a v: ltf-find-file-at-point
;; C-u C-x a v : find file at point, in another window.
;; If anyone finds smartest key binding for these functions, let me know
;; i will add it in this file


;; The purpose of these functions is to allow you to read quickly
;; a file located in some known directories, without specifying its full path
;; Two interactive functions are provided :
;; ltf-find-file : gives you the choice to give a relative file name
;; ltf-find-file-at-point : reads the file name at point

;; these two functions load the file(s) with this name if they appear
;; to exist in one of the absolute pathnames defined in the variable
;; ltf-find-this-file-list


;; ltf-find-only-first is no more used
;; if ltf-find-only-first is set to NOT NIL, the above functions only
;; load the first file they found.
;; To stop loading when the first file is found
;; (setq ltf-find-only-first t)
;; To load all the matching files in ltf-find-this-file-list
;; (setq ltf-find-only-first nil)



;; These portions of code are here to explain and test

;; To clear ltf-find-this-file-list
;; (makunbound 'ltf-find-this-file-list)

;; to add a path to the ltf-find-this-file-list
;; (setq ltf-find-this-file-list (cons ".." ltf-find-this-file-list ))


;; Test the program by putting the cursor between the quotes "" and <>
;; #include <simple.el>
;; #include <find-this-file_1-5.el>

;; #include "simple.el"
;; #include "find-this-file_1-5.el"

;; ./find-this-file_1-4.el:52:

;		euros.el

;; If everything works as you expected, M-x insert-file this file in your
;; .emacs file


(global-set-key "\C-xaf" 'ltf-find-file)
(global-set-key "\C-xav" 'ltf-find-file-at-point-interactive)

(defvar ltf-open-in-other-window t
  "Whether or not to open the file in another window (equivalent of giving a prefix arg).")

(defvar ltf-find-this-file-list
  (list "." "/usr/local/share/emacs/21.1/lisp")
  "List of directories to search when using load-this-file.")

;;(defvar ltf-find-only-first t
;;  "Set to t to stop loading of files as soon as one has been found.")

;;;###autoload
(defun ltf-find-file (&optional arg)
  "Loads the file whose relative path is given."
  (interactive "MFile to find (give a relative path): ")
  (ltf-find-file-at-point arg))

;;;###autoload
(defun ltf-find-file-at-point-interactive (&optional prefix arg)
  "Just calls `ltf-find-file-at-point' but accepts prefix argument to open the file in another buffer."
  (interactive "P")
  (ltf-find-file-at-point arg prefix))

(defun ltf-find-file-at-point (&optional arg prefix)
  "Loads a file wich relative path is given, searching in ltf-find-this-file-list for paths.
If ARG is nil, loads the relative pathname located at point.
Three kinds of paths are recognized: path/file:line-number: or \"path/file\" or <path/file>.
The first one is useful for grep-like files. You can format your log files this way."
  (interactive)
  (if arg
	  (ltf-find-file-in-path arg ltf-find-this-file-list)
	(save-excursion
	  (let ((line-end (save-excursion
						(progn
						  (end-of-line)
						  (point)))))
;		(if (re-search-backward "\\(^[\"<]?\\|[\"< ]\\)" 0 t 1)
		(if (re-search-backward "\\(^\\|[\"<\t( ]\\)" 0 t 1)
		    (progn
		      (let* ((kind-of-sep (match-string 1))
			     (re
			      (cond
			       ((equal kind-of-sep "\"")
				(progn (forward-char 1)
				       "\\([^\":]+\\)\\(:\\([0-9]+\\):?\\)?\""))
			       ((equal kind-of-sep "<")
				(progn (forward-char 1)
				       "\\([^>:]+\\)\\(:\\([0-9]+\\):?\\)?>"))
			       ((equal kind-of-sep "(")
				(progn (forward-char 1)
				       "\\([^):]+\\)\\(:\\([0-9]+\\):?\\)?)"))
			       ((or (equal kind-of-sep " ")
				    (equal kind-of-sep "")
				    (equal kind-of-sep "\t"))
				(progn (re-search-forward "[^\t ]")
				       (backward-char 1)
				       "\\([^:\t ]+\\)\\(:\\([0-9]+\\):?\\)?"))
			       (t "\\([^:]+\\)\\(:\\([0-9]+\\):?\\)?"))))
			(if (re-search-forward re line-end t 1)
			    (ltf-find-file-in-path (match-string 1) ltf-find-this-file-list (match-string 3))
			  (message "No matching character for %s with %s" kind-of-sep re))))
		  (message "No beg of line, nor character \" or < found backward."))))))


;; The list of paths is sent to this function to enable in the future
;; the use of other lists of paths
;; This version considers the path is relative or absolute (beginning
;; by /, ./, or ~

(defun ltf-find-file-relative-in-path
  (this-file path-list &optional line-to-reach)
  (if (null path-list)
      nil
    (let ((the-file (expand-file-name (concat (car path-list)
					      "/"
					      this-file))))
      (if (file-exists-p the-file)
	  the-file
	(ltf-find-file-relative-in-path this-file
					(cdr path-list)
					line-to-reach)))))


(defun ltf-find-file-in-path (this-file path-list &optional line-to-reach)
  (let ((absolute-file
	 (if (and (file-name-absolute-p this-file)
		  (file-exists-p this-file))
	     this-file
	   (ltf-find-file-relative-in-path
	    this-file path-list line-to-reach))))
    (let ((buffer
	   (if (null absolute-file)
	       (let ((existing-buffer
		      (get-buffer
		       (file-name-nondirectory
			this-file))))
		 (if (buffer-live-p existing-buffer)
		     existing-buffer))
	     (find-file-noselect absolute-file))))
      (if (null buffer)
	  (error "Can't find \"%s\" in path %s"
		 this-file path-list)
	(if (or prefix ltf-open-in-other-window)
	    (switch-to-buffer-other-window buffer)
	  (switch-to-buffer buffer))
	(if line-to-reach
	    (goto-line (string-to-number line-to-reach)))))))

(provide 'find-this-file)

;; arch-tag: 5d20c647-c392-4f18-b8b7-ab6e566188e1
