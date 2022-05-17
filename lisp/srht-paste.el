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
  "Stores pastes info.")

(defun srht-paste--make-crud (path &optional query body)
  "Make crud for paste service.
PATH is the path for the URI.  BODY is the body sent to the URI.
QUERY is the query for the URI."
  (srht-generic-crud 'paste path query body))

(cl-defun srht-paste-make (&key (visibility "unlisted") (filename 'null) contents)
  "Make paste parameters.
VISIBILITY must be one of \"public\", \"private\", or \"unlisted\".
FILENAME string or null by default.
CONTENTS must be a UTF-8 encoded string; binary files are not allowed."
  (cl-assert (member visibility '("unlisted" "public" "private")))
  `((visibility . ,visibility)
    (files . [((filename . ,filename)
               (contents . ,contents))])))

(defun srht-pastes (&optional query)
  "Retrieve all the pastes that belong to the user.
QUERY is the query for the URI."
  (srht-paste--make-crud "/api/pastes" query))

(defun srht-paste-blob (sha)
  "Retrieve a blob resource with the hash SHA."
  (srht-paste--make-crud (format "/api/blobs/%s" sha)))

(defun srht-paste--candidates ()
  "Return completion candidates."
  (seq-map (pcase-lambda ((map (:created c)
                               (:visibility v)
                               (:sha sha)
                               (:files (seq (map (:filename fn))))))
             (list fn c v sha))
           (plist-get (or srht-paste-all-pastes
                          (setq srht-paste-all-pastes
                                (srht-retrive (srht-pastes))))
                      :results)))

(defun srht-paste--annot (str)
  "Function to add annotations in the completions buffer for STR."
  (pcase-let* (((seq _f c v _s) (assoc str (srht-paste--candidates)))
               (l (- 40 (length (substring-no-properties str))))
               (bb (make-string l (string-to-char " ")))
               (sb (if (string= v "public") "      " "    ")))
    (concat bb (format "%s%s%s" v sb c))))

(defun srht-paste--sha ()
  "Read a FILENAME in the minibuffer, with completion and return SHA."
  (let ((cand (srht-paste--candidates)))
    (car (last (assoc
                (srht-read-with-annotaion
                    "Select paste: " cand #'srht-paste--annot 'sourcehut-paste)
                cand)))))

(defun srht-paste (&optional sha &rest details)
  "Create, retrieve or delete a paste.

When retrieving or deleting a paste SHA must the the hash
corresponding to the paste.

When creating a new paste, SHA must be nil and one has to
specify the DETAILS (see `srht-paste-make') of the paste."
  (cond
   ((stringp sha)
    (srht-paste--make-crud (format "/api/pastes/%s" sha)))
   ((stringp (plist-get details :contents))
    (srht-paste--make-crud "/api/pastes" nil (apply #'srht-paste-make details)))))

(defun srht-paste--get-content ()
  "Extract the content we want to paste.
Either the active region or, if no region is active (i.e. text selected)
the whole buffer."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-string)))

(defun srht-paste--else (plz-error)
  "An optional callback function.
Called when the request fails with one argument, a ‘plz-error’ struct PLZ-ERROR."
  (pcase-let* (((cl-struct plz-error response) plz-error)
               ((cl-struct plz-response status body) response))
    (pcase status
      (201 (srht-with-json-read-from-string body
             (map (:sha sha)
                  (:user (map (:canonical_name name))))
             (srht-kill-link 'paste name sha)
             (srht-retrive (srht-pastes)
                           :then (lambda (resp)
                                   (setq srht-paste-all-pastes resp)))))
      (204 (srht-retrive (srht-pastes)
                         :then (lambda (resp)
                                 (setq srht-paste-all-pastes resp)
                                 (message "Deleted!"))))
      (_ (error "Unkown error with status %s: %S" status plz-error)))))

;;;###autoload
(defun srht-paste-region (visibility filename)
  "Paste region or buffer to sourcehut under FILENAME with VISIBILITY."
  (interactive
   (list (completing-read "Visibility: "
			  '("private" "public" "unlisted") nil t)
	 (read-string (format "Filename (default: %s): " (buffer-name))
		      nil nil (buffer-name))))
  (let ((content (srht-paste--get-content)))
    (srht-create
     (srht-paste nil :visibility visibility :filename filename :contents content)
     :then (lambda (_r))
     :else #'srht-paste--else)))

;;;###autoload
(defun srht-paste-delete (sha)
  "Detete paste with SHA."
  (interactive
   (list (srht-paste--sha)))
  (srht-delete (srht-paste sha)
               :then (lambda (_r))
               :else #'srht-paste--else))

;;;###autoload
(defun srht-paste-link ()
  "Kill the link of the selected paste owned by the USER."
  (interactive)
  (when (string-empty-p srht-username)
    (error "`srht-username' must be set"))
  (srht-kill-link 'paste (concat "~" srht-username) (srht-paste--sha)))

(provide 'srht-paste)
;;; srht-paste.el ends here
