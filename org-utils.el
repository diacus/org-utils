;;; org-utils.el --- Useful tools for working with orgmode documents
;;
;; Author: Diacus Magnuz <diacus.magnuz@gmail.com>
;; URL: https://github.com/diacus/org-utils
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (org "9.7.11"))
;; Keywords: orgmode
;;; Commentary:
;;
;; Org-utils is a simple package with useful tools for working with orgmode documents
;;
;;; Code:

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
  "Reads the value of PROPERTY-NAME property of the current Org item, if it is a valid URL opens it using the default web browser."
  (interactive "MPROPERTY: ")
  (let ((url (org-entry-get nil property-name)))
    (if url
	(browse-url url)
      (message "Property '%s' not found" property-name))))
  
(provide org-utils)
;;; org-utils.el ends here
