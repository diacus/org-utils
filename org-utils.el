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
(require 'calendar)


(defun org-utils/copy-property-value ()
  "Copies the value of PROPERTY-NAME property of the current Org item."
  (interactive)
  (let* ((props (org-entry-properties nil))
         (prop-names (mapcar #'car props))
         (property-name
          (completing-read
           "Property: "
           prop-names nil t))
         (value (org-entry-get nil property-name)))
    (if value
        (progn
          (kill-new value)
          (message "Copied value: %s" value))
      (message "Property '%s' not found" property-name))))


(defun org-utils/browse-property-url ()
  "Spawns the browser to the url at PROPERTY-NAME.
Reads the value of PROPERTY-NAME property of the current Org item, if
it is a valid URL opens it using the default web browser."
  (interactive)
  (let* ((props (org-entry-properties nil))
         (prop-names (mapcar #'car props))
         (property-name
          (completing-read
           "Property: "
           prop-names nil t))
         (url (org-entry-get nil property-name)))
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


(defun org-utils/get-entry-scheduled ()
  "Prses the SCHEDULED value of the current org element and return its value."
  (let ((scheduled (org-entry-get nil "SCHEDULED")))
    (org-time-string-to-time scheduled)))


(defun org-utils/add-months (time n)
  "Add N months to TIME and return the resulting time value.
TIME must be a time value.  Day is clamped to last day of target month."
  (let* ((dt (decode-time time))
	 (sec         (nth 0 dt))
	 (min         (nth 1 dt))
	 (hour        (nth 2 dt))
	 (day         (nth 3 dt))
	 (month       (nth 4 dt))
	 (year        (nth 5 dt))
	 (new-month   (+ month n))
	 (new-year    (+ year (/ (1- new-month) 12)))
	 (final-month (1+ (mod (1- new-month) 12)))
	 (last-day    (calendar-last-day-of-month final-month new-year))
	 (adjusted-day (min day last-day)))
    (encode-time sec min hour adjusted-day final-month new-year)))


(defun org-utils/make-timedelta (&rest plist)
  "Return seconds from PLIST with keys :days :hours :minutes :seconds.
Note: :months is not supported here because months have variable lengths.
Use `org-utils/add-timedelta' for month arithmetic."
  (let ((days    (or (plist-get plist :days) 0))
        (hours   (or (plist-get plist :hours) 0))
        (minutes (or (plist-get plist :minutes) 0))
        (seconds (or (plist-get plist :seconds) 0)))
    (+ seconds
       (* minutes 60)
       (* hours 3600)
       (* days 86400))))


(defun org-utils/add-timedelta (time &rest plist)
  "Add timedelta from PLIST to TIME and return result.
PLIST keys: :months :days :hours :minutes :seconds.
Months are added first using `org-utils/add-months',
then days/hours/minutes/seconds."
  (let ((months (or (plist-get plist :months) 0))
        (delta-seconds (org-utils/make-timedelta
                        :days (or (plist-get plist :days) 0)
                        :hours (or (plist-get plist :hours) 0)
                        :minutes (or (plist-get plist :minutes) 0)
                        :seconds (or (plist-get plist :seconds) 0))))
    (time-add (org-utils/add-months time months) delta-seconds)))


(defun org-utils/format-active-timestamp (time)
  "Return active timestamp string from TIME.
TIME must be a time value as returned by `encode-time' or
`org-time-string-to-time'.  Format is <YYYY-MM-DD DDD> or
<YYYY-MM-DD DDD HH:MM> if time is non-zero."
  (let ((decoded (decode-time time)))
    (if (or (/= (decoded-time-hour decoded) 0)
            (/= (decoded-time-minute decoded) 0)
            (/= (decoded-time-second decoded) 0))
        (format-time-string "<%Y-%m-%d %a %H:%M>" time)
      (format-time-string "<%Y-%m-%d %a>" time))))


(defun org-utils/format-inactive-timestamp (time)
  "Return inactive timestamp string from TIME.
TIME must be a time value as returned by `encode-time' or
`org-time-string-to-time'.  Format is [YYYY-MM-DD DDD] or
[YYYY-MM-DD DDD HH:MM] if time is non-zero."
  (let ((decoded (decode-time time)))
    (if (or (/= (decoded-time-hour decoded) 0)
            (/= (decoded-time-minute decoded) 0)
            (/= (decoded-time-second decoded) 0))
        (format-time-string "[%Y-%m-%d %a %H:%M]" time)
      (format-time-string "[%Y-%m-%d %a]" time))))


(defun org-utils/scheduled-monthly-dates (n)
  "Return N monthly dates starting from current heading's SCHEDULED."
  (let* ((scheduled (org-entry-get nil "SCHEDULED"))
         (time      (org-time-string-to-time scheduled))
         (decoded   (decode-time time))
         (day       (decoded-time-day decoded))
         (month     (decoded-time-month decoded))
         (year      (decoded-time-year decoded)))
    (cl-loop for i from 0 below n
             for new-month = (+ month i)
             for adjusted-year = (+ year (/ (1- new-month) 12))
             for adjusted-month = (1+ (mod (1- new-month) 12))
             for last-day = (calendar-last-day-of-month adjusted-month adjusted-year)
             for adjusted-day = (min day last-day)
             for new-time = (encode-time 0 0 0 adjusted-day adjusted-month adjusted-year)
             collect (format-time-string "%Y-%m-%d" new-time))))
  
(provide 'org-utils)
;;; org-utils.el ends here
