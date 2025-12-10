;;; org-utils.el --- Useful tools for working with orgmode documents -*- lexical-binding: t; -*-
;;
;; Author: Diacus Magnuz <diacus.magnuz@gmail.com>
;; URL: https://github.com/diacus/org-utils
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (org "9.5") (cl-lib "0.7"))
;; Keywords: orgmode
;;; Commentary:
;;
;; Org-utils is a simple package with useful tools for working with orgmode documents
;;
;;; Code:
(require 'org)

(defun org-utils/copy-property-value (property-name)
  "Copies the value of PROPERTY-NAME property of the current Org item."
  (interactive "MPROPERTY: ")
  (let ((value (org-entry-get nil property-name)))
    (if value
	(progn
	  (kill-new value)
	  (message "Copied value: %s" value))
      (message "Property '%s' not found" property-name))))


(defun org-utils/browse-property-url (property-name)
  "Spawns the browser to the url at PROPERTY-NAME.
Reads the value of PROPERTY-NAME property of the current Org item, if
it is a valid URL opens it using the default web browser."
  (interactive "MPROPERTY: ")
  (let ((url (org-entry-get nil property-name)))
    (if url
	(browse-url url)
      (message "Property '%s' not found" property-name))))


(defun org-utils/set-org-agenda-files ()
  "Update `org-agenda-files' variable.
Presents multiple ptions of files .org/.org.gpg excluding backups.
Uses ':' as separator like `org-set-tags-command' does.
Supports wildcard patterns (e.g. ca*.org) that can be expanded."
  (interactive)
  (require 'crm)
  (require 'cl-lib)
  (let* ((orgdir (if (and (boundp 'org-directory) org-directory)
			 (expand-file-name org-directory)
		       (read-directory-name "org-directory: " "~")))
	     (org-file-re "\\`[^#].*\\.org\\(\\.gpg\\)?\\'")
	     (all-org-files (directory-files-recursively orgdir org-file-re))
	     (rel-files (mapcar (lambda (f) (file-relative-name f orgdir))
				all-org-files))
	     (dirs (cl-remove-if-not #'file-directory-p
				    (directory-files orgdir t "^[^.].*")))
	     (rel-dirs (mapcar (lambda (d) (file-relative-name d orgdir)) dirs))
	     (candidates (append rel-files rel-dirs))
	     (crm-separator ":")
	     ;; Get entries from user, might be wildcards
	     (raw (completing-read-multiple
		   (format "Agenda files (in %s) — use ':' as separator: " orgdir)
		   candidates nil nil))
	     (choices (cl-remove-if #'string-empty-p (mapcar #'string-trim raw)))
	     (result '()))
	(dolist (choice choices)
	  (let ((abs-choice (expand-file-name choice orgdir)))
	    (cond
	     ;; If entry is a directory
	     ((and (file-exists-p abs-choice) (file-directory-p abs-choice))
	      (setq result (append (directory-files-recursively abs-choice org-file-re)
				   result)))
	     ;; If entry is a valid file
	     ((and (file-exists-p abs-choice)
		   (string-match org-file-re abs-choice))
	      (push abs-choice result))
	     ;; If entry contains wildcards → expand matches
	     ((string-match-p "[[*?]" choice)
	      (let* ((pattern (expand-file-name choice orgdir))
		     (matches (file-expand-wildcards pattern t)))
		(setq matches (cl-remove-if-not
			       (lambda (f) (string-match org-file-re f))
			       matches))
		(if matches
		    (setq result (append matches result))
		  (message "Warning: the pattern %s didn't math any valid file"
			   choice))))
	     (t
	      (message "Warning: %s isn't a valid .org(.gpg) file (ignored)" abs-choice)))))
	;; Normalize results
	(setq result (cl-delete-duplicates (nreverse result) :test #'string=))
	(setq org-agenda-files result)
	(message "org-agenda-files updated (%d): %s"
		 (length result)
		 (mapconcat #'file-name-nondirectory result ", "))))


(defun org-utils/restore-org-agenda-files ()
  "Restore `org-agenda-files` to the value set in `custom-file`.
This is equivalent to revert any temporal change made by the user
in the current session."
  (interactive)
  (if (get 'org-agenda-files 'customized-value)
	  (progn
	    (custom-reevaluate-setting 'org-agenda-files)
	    (message "org-agenda-files value restored from custom.el"))
	(message "org-agenda-files isn't customized in custom.el")))
  
(provide 'org-utils)
;;; org-utils.el ends here
