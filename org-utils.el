;;; org-utils.el
;; Author: Diacus Magnuz <diacus.magnuz@gmail.com>
;; Version: 0.0.1
;;; Commentary: A simple package with useful tools for working with orgmode documents
;;; Code:

(defun org-utils/copy-property-value (property-name)
  "Copies the value of PROPERTY-NAME property of the current Org item."
  (interactive "MPROPERTY: ")
  (let ((value (org-get-entry nil property-name)))
    (if value
	(progn
	  (kill-new value)
	  (message "Copied value: %s" value))
      (message "Property '%s' not found" property-name))))


(defun org-utils/browse-property-url (property-name)
  "Reads the value of PROPERTY-NAME property of the current Org item, if it is a valid URL opens it using the default web browser."
  (interactive "MPROPERTY: ")
  (let ((url (org-get-entry nil property-name)))
    (if url
	(browse-url url)
      (message "Property '%s' not found" property-name))))
  
(provide org-utils)
;;; org-utils.el ends here
