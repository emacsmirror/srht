;;; srht-paste.el --- Sourcehut paste                -*- lexical-binding: t; -*-

;; Copyright © 2022-2023  Free Software Foundation, Inc.

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
  "Stores pastes plist of the form (:instance pastes ...).")

(defun srht-paste--make-crud (instance path &optional query body)
  "Make crud for paste service for the INSTANCE of the Sourcehut instance.
PATH is the path for the URI.  BODY is the body sent to the URI.
QUERY is the query for the URI."
  (srht-generic-crud instance 'paste path query body))

(cl-defun srht-paste-make (&key (visibility "unlisted") (filename 'null) contents)
  "Make paste parameters.
VISIBILITY must be one of \"public\", \"private\", or \"unlisted\".
FILENAME string or null by default.
CONTENTS must be a UTF-8 encoded string; binary files are not allowed."
  (cl-assert (member visibility '("unlisted" "public" "private")))
  `((visibility . ,visibility)
    (files . [((filename . ,filename)
               (contents . ,contents))])))

(defun srht-pastes (instance &optional query)
  "Get all pastes owned by the authenticated user and instance with INSTANCE.
QUERY is the query for the URI."
  (srht-paste--make-crud instance "/api/pastes" query))

(defun srht-paste-blob (instance sha)
  "Retrieve a blob resource with the hash SHA from the INSTANCE instance."
  (srht-paste--make-crud instance (format "/api/blobs/%s" sha)))

(defun srht-paste--instance-results-get (instance pastes)
  "Extract the value for the :results property.
For the existing PASTES for the INSTANCE instance name."
  (declare (indent 1))
  (plist-get (plist-get pastes (intern instance)) :results))

(defun srht-paste--candidates (instance)
  "Return completion candidates for INSTANCE."
  (seq-map (pcase-lambda ((map (:created c)
                               (:visibility v)
                               (:sha sha)
                               (:files (seq (map (:filename fn))))))
             (list fn c v sha))
           (srht-results-get instance
             (or srht-paste-all-pastes
                 (srht-put srht-paste-all-pastes
                   instance (srht-retrive (srht-pastes instance)))))))

(defun srht-paste--annot (instance str)
  "Function to add annotations in the completions buffer for STR and INSTANCE."
  (pcase-let (((seq _f created visibility)
               (assoc str (srht-paste--candidates instance))))
    (srht-annotation str visibility created)))

(defun srht-paste--sha (instance)
  "Read a FILENAME in the minibuffer, with completion and return SHA.
INSTANCE is the instance name of the Sourcehut instance."
  (let ((cand (srht-paste--candidates instance)))
    (car (last (assoc
                (srht-read-with-annotaion
                    "Select paste: " cand
                    (lambda (str) (srht-paste--annot instance str))
                    'sourcehut-paste)
                cand)))))

(defun srht-paste (instance &optional sha &rest details)
  "Create, retrieve or delete a paste from INSTANCE.

When retrieving or deleting a paste SHA must the the hash
corresponding to the paste.

When creating a new paste, SHA must be nil and one has to
specify the DETAILS (see `srht-paste-make') of the paste."
  (cond
   ((stringp sha)
    (srht-paste--make-crud instance (format "/api/pastes/%s" sha)))
   ((stringp (plist-get details :contents))
    (srht-paste--make-crud instance "/api/pastes" nil (apply #'srht-paste-make details)))))

(defun srht-paste--get-content ()
  "Extract the content we want to paste.
Either the active region or, if no region is active (i.e. text selected)
the whole buffer."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-string)))

;;;###autoload
(defun srht-paste-region (instance visibility filename)
  "Paste region or buffer to Sourcehut instance with INSTANCE.
Set FILENAME and VISIBILITY."
  (interactive
   (list (srht-read-instance "Instance: ")
         (srht-read-visibility "Visibility: ")
	 (read-string (format "Filename (default: %s): " (buffer-name))
		      nil nil (buffer-name))))
  (let ((content (srht-paste--get-content)))
    (srht-create
     (srht-paste instance nil
                 :visibility visibility
                 :filename filename
                 :contents content)
     :then
     (lambda (results)
       (pcase-let* (((map (:sha sha)
                          (:user (map (:canonical_name username))))
                     results)
                    (url (srht--make-uri
                          instance 'paste (format "/%s/%s" username sha) nil)))
         (srht-copy-url url)
         (srht-browse-url url)
         (srht-retrive (srht-pastes instance)
                       :then (lambda (resp)
                               (srht-put srht-paste-all-pastes instance resp))))))))

;;;###autoload
(defun srht-paste-delete (instance sha)
  "Detete paste with SHA from the INSTANCE instance."
  (interactive
   (let ((instance (srht-read-instance "Instance: ")))
     (list instance (srht-paste--sha instance))))
  (srht-delete
   (srht-paste instance sha)
   :as 'string
   :then (lambda (_r)
           (srht-retrive
            (srht-pastes instance)
            :then (lambda (resp)
                    (srht-put srht-paste-all-pastes instance resp)
                    (message "Deleted!"))))))

;;;###autoload
(defun srht-paste-copy-url (instance)
  "Copy selected paste URL to clipboard.
INSTANCE (see `srht-read-instance')."
  (interactive (list (srht-read-instance "Instance: ")))
  (when (string-empty-p srht-username)
    (error "`srht-username' must be set"))
  (let* ((sha (srht-paste--sha instance))
         (username (concat "~" (string-trim-left srht-username "~")))
         (url (srht--make-uri
               instance 'paste (format "/%s/%s" username sha) nil)))
    (srht-copy-url url)
    (srht-browse-url url)))


(provide 'srht-paste)
;;; srht-paste.el ends here
