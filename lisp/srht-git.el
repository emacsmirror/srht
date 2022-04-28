;;; srht-git.el --- Sourcehut git                    -*- lexical-binding: t; -*-

;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>

;; Created: <2022-04-26 Tue>

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
;; https://man.sr.ht/git.sr.ht/api.md
;;

;;; Code:

(require 'srht)

(defun srht-git--make-crud (path &optional body form)
  "Make crud for git service.
PATH is the path for the URI.  BODY is the body sent to the URI.
FORM is a content type."
  (srht-generic-crud 'git path body form))

(defun srht-git-user (&optional username)
  "Retrieves a user resource.
If USERNAME is nil, the authenticated user is assumed."
  ;; TODO: tilda in username
  (let ((path (if username
                  (concat "/api/user/" username)
                "/api/user")))
    (srht-git--make-crud path)))

;; (srht-retrive (srht-git-user "~akagi"))
;; (srht-retrive (srht-git-user "~sircmpwn"))

(defun srht-git-repos (&optional username)
  "Retrive list of repository resources owned by this USERNAME.
If USERNAME is nil the authenticated user is assumed."
  (let ((path (if username
                  (format "/api/%s/repos" username)
                "/api/repos")))
    (srht-git--make-crud path)))

;; (setq akagi-repos-test (srht-retrive (srht-git-repos)))

(cl-defun srht-git-make (&key (visibility "unlisted") description name)
  "Make paste parameters.
VISIBILITY must be one of \"public\", \"private\", or \"unlisted\".
DESCRIPTION is repository description, markdown is allowed.
NAME is repository name."
  (cl-assert (or (member visibility '("unlisted" "public" "private"))
                 (not (null name))))
  `((name . ,name)
    (description . ,description)
    (visibility . ,visibility)))

;; (srht-git-make :visibility "ulnlisted" :name "test-repo" :description "hi")
;; (srht-git-make :visibility "ulnlisted" :description "hi")
;; (json-encode (srht-git-make :visibility "unlisted" :name "test-repo" :description "hi"))

(defun srht-git-repo (repo-name &optional username &rest details)
  "Create, retrieve, delete or update a git repository.

When retrieving, deleting or updating a repository, REPO-NAME must be
the name of an existing repository.

When retrieving if USERNAME is nil the authenticated user is assumed.

When updating DETAILS, you must specify DETAILS (see `srht-git-make').
;; NOTE: Updating the name will create a redirect.

When creating repository omit REPO-NAME and specify DETAILS
\(see `srht-git-make'\)."
  (cond
   ((and (stringp repo-name) (stringp username))
    (srht-git--make-crud (format "/api/%s/repos/%s" username repo-name)))
   ((stringp repo-name) (srht-git--make-crud (format "/api/repos/%s" repo-name)))
   (t (srht-git--make-crud "/api/repos" (apply #'srht-git-make details)))))

(defmacro srht-git--endpoints (endpoint name username &optional body form)
  "Generate crud for ENDPOINT and repository NAME.
If USERNAME is nil the authenticated user is assumed.
BODY is the body sent to the URI.
FORM is a content type."
  (let ((path (gensym "path")))
    `(let ((,path (if ,username
                      (format "/api/%s/repos/%s/%s" ,username ,name ,endpoint)
                    (format "/api/repos/%s/%s" ,name ,endpoint))))
       (srht-git--make-crud ,path ,body ,form))))

(defun srht-git--endpoint-widen (func name end &optional username body)
  "TODO: doc."
  (let* ((plist (if body
                    (funcall func name username body)
                  (funcall func name username)))
         (path (plist-get plist :path)))
    (setf (plist-get plist :path)
          (concat path "/" end))
    plist))

(defun srht-git--artifact (name username body)
  "TODO: doc."
  (srht-git--endpoints "artifacts" name username body "multipart/form-data"))

(defun srht-git-repo-readme (name &optional username body form)
  "Retrieve, update or delete README override for repository NAME.

If USERNAME is nil the authenticated user is assumed.
BODY is the body sent to the URI.  FORM is a content type."
  (srht-git--endpoints "readme" name username body form))

(defun srht-git-repo-refs (name &optional username)
  "Endpoints for fetching git data from repository NAME.
If USERNAME is nil the authenticated user is assumed."
  (srht-git--endpoints "refs" name username))

(defun srht-git-repo-log (name &optional username)
  "List of the latest commit log for repository NAME.
If USERNAME is nil the authenticated user is assumed."
  (srht-git--endpoints "log" name username))

(defun srht-git-repo-artifact (name ref body &optional username)
  "Attaches a file artifact to the specified REF and repository NAME.
Note: this endpoint does not accept JSON.  Submit your request
as `multipart/form-data', with a single field: file in BODY."
  (srht-git--endpoint-widen #'srht-git--artifact name ref username body))

(defun srht-git-repo-log-ref (name ref &optional username)
  "List of the latest commit resources starting from the given REF.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed."
  (srht-git--endpoint-widen #'srht-git-repo-log name ref username))

(defun srht-git-repo-tree-ref (name ref &optional username)
  "Return the tree resource for the given REF.
Following the parent trees until the requested tree is found.
In other words, this lists the contents of a subdirectory by path.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed."
  (srht-git--endpoint-widen #'srht-git-repo-tree name ref username))

(defun srht-git-repo-tree-id (name id &optional username)
  "Return the tree resource with the given ID.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed."
  (srht-git--endpoint-widen #'srht-git-repo-tree name id username))

(defun srht-git-repo-tree (name &optional username)
  "Return the tree resource for the latest commit to the default branch.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed."
  (srht-git--endpoints "tree" name username))

(provide 'srht-git)
;;; srht-git.el ends here
