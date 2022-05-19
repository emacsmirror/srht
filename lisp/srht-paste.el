;;; srht-paste.el --- Sourcehut paste                -*- lexical-binding: t; -*-

;; Copyright © 2022  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;  https://man.sr.ht/paste.sr.ht/api.md#paste-resource
;;

;;; Code:

(require 'srht)

(defvar srht-paste-all-pastes nil
  "Stores pastes plist of the form (:domain pastes ...).")

(defun srht-paste--make-crud (domain path &optional query body)
  "Make crud for paste service for the DOMAIN of the Sourcehut instance.
PATH is the path for the URI.  BODY is the body sent to the URI.
QUERY is the query for the URI."
  (srht-generic-crud domain 'paste path query body))

(cl-defun srht-paste-make (&key (visibility "unlisted") (filename 'null) contents)
  "Make paste parameters.
VISIBILITY must be one of \"public\", \"private\", or \"unlisted\".
FILENAME string or null by default.
CONTENTS must be a UTF-8 encoded string; binary files are not allowed."
  (cl-assert (member visibility '("unlisted" "public" "private")))
  `((visibility . ,visibility)
    (files . [((filename . ,filename)
               (contents . ,contents))])))

(defun srht-pastes (domain &optional query)
  "Get all pastes owned by the authenticated user and instance with DOMAIN.
QUERY is the query for the URI."
  (srht-paste--make-crud domain "/api/pastes" query))

(defun srht-paste-blob (domain sha)
  "Retrieve a blob resource with the hash SHA from the DOMAIN instance."
  (srht-paste--make-crud domain (format "/api/blobs/%s" sha)))

(defun srht-paste--domain-results-get (domain pastes)
  "Extract the value for the :results property.
For the existing PASTES for the DOMAIN domain name."
  (declare (indent 1))
  (plist-get (plist-get pastes (intern domain)) :results))

(defun srht-paste--candidates (domain)
  "Return completion candidates for DOMAIN."
  (seq-map (pcase-lambda ((map (:created c)
                               (:visibility v)
                               (:sha sha)
                               (:files (seq (map (:filename fn))))))
             (list fn c v sha))
           (srht-results-get domain
             (or srht-paste-all-pastes
                 (srht-put srht-paste-all-pastes
                   domain (srht-retrive (srht-pastes domain)))))))

(defun srht-paste--annot (domain str)
  "Function to add annotations in the completions buffer for STR and DOMAIN."
  (srht-annotation (seq _f created visibility)
    (assoc str (srht-paste--candidates domain)) str))

(defun srht-paste--sha (domain)
  "Read a FILENAME in the minibuffer, with completion and return SHA.
DOMAIN is the domain name of the Sourcehut instance."
  (let ((cand (srht-paste--candidates domain)))
    (car (last (assoc
                (srht-read-with-annotaion
                    "Select paste: " cand
                    (lambda (str) (srht-paste--annot domain str))
                    'sourcehut-paste)
                cand)))))

(defun srht-paste (domain &optional sha &rest details)
  "Create, retrieve or delete a paste from DOMAIN.

When retrieving or deleting a paste SHA must the the hash
corresponding to the paste.

When creating a new paste, SHA must be nil and one has to
specify the DETAILS (see `srht-paste-make') of the paste."
  (cond
   ((stringp sha)
    (srht-paste--make-crud domain (format "/api/pastes/%s" sha)))
   ((stringp (plist-get details :contents))
    (srht-paste--make-crud domain "/api/pastes" nil (apply #'srht-paste-make details)))))

(defun srht-paste--get-content ()
  "Extract the content we want to paste.
Either the active region or, if no region is active (i.e. text selected)
the whole buffer."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-string)))

(defun srht-paste--else (domain plz-error)
  "An optional callback function.
Called when the request fails with one argument, a ‘plz-error’ struct PLZ-ERROR
and domain name DOMAIN."
  (pcase-let* (((cl-struct plz-error response) plz-error)
               ((cl-struct plz-response status body) response))
    (pcase status
      (201 (srht-with-json-read-from-string body
             (map (:sha sha)
                  (:user (map (:canonical_name name))))
             (srht-kill-link domain 'paste name sha)
             (srht-retrive (srht-pastes domain)
                           :then (lambda (resp)
                                   (srht-put srht-paste-all-pastes domain resp)))))
      (204 (srht-retrive (srht-pastes domain)
                         :then (lambda (resp)
                                 (srht-put srht-paste-all-pastes domain resp)
                                 (message "Deleted!"))))
      (_ (error "Unkown error with status %s: %S" status plz-error)))))

;;;###autoload
(defun srht-paste-region (domain visibility filename)
  "Paste region or buffer to Sourcehut instance with DOMAIN.
Set FILENAME and VISIBILITY."
  (interactive
   (list (srht-read-domain "Instance: ")
         (srht-read-visibility "Visibility: ")
	 (read-string (format "Filename (default: %s): " (buffer-name))
		      nil nil (buffer-name))))
  (let ((content (srht-paste--get-content)))
    (srht-create
     (srht-paste domain nil
                 :visibility visibility
                 :filename filename
                 :contents content)
     :then (lambda (_r))
     :else (lambda (err) (srht-paste--else domain err)))))

;;;###autoload
(defun srht-paste-delete (domain sha)
  "Detete paste with SHA from the DOMAIN instance."
  (interactive
   (let ((instance (srht-read-domain "Instance: ")))
     (list instance (srht-paste--sha instance))))
  (srht-delete (srht-paste domain sha)
               :then (lambda (_r))
               :else (lambda (err) (srht-paste--else domain err))))

;;;###autoload
(defun srht-paste-link (domain)
  "Kill the link of the selected paste owned by the USER from the DOMAIN instance."
  (interactive (list (srht-read-domain "Instance: ")))
  (when (string-empty-p srht-username)
    (error "`srht-username' must be set"))
  (srht-kill-link domain 'paste (concat "~" (string-trim-left srht-username "~"))
                  (srht-paste--sha domain)))

(provide 'srht-paste)
;;; srht-paste.el ends here
