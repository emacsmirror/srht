;;; srht-git.el --- Sourcehut git                    -*- lexical-binding: t; -*-

;; Copyright © 2022  Free Software Foundation, Inc.

;; Created: <2022-04-26 Tue>

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
;; https://man.sr.ht/git.sr.ht/api.md
;;

;;; Code:

(require 'srht)

(defvar srht-git-repos nil
  "Authenticated user repos plist of the form (:domain repos ...).")

(defun srht-git--make-crud (domain path &optional query body form)
  "Make a crud for the git service for the DOMAIN of the Sourcehut instance.
PATH is the path for the URI.  BODY is the body sent to the URI.
FORM is a content type.  QUERY is the query for the URI."
  (declare (indent 1))
  (srht-generic-crud domain 'git path query body form))

(defun srht-git-user (domain &optional username)
  "Retrieves a user resource from DOMAIN.
If USERNAME is nil, the authenticated user is assumed."
  (let ((path (if username
                  (concat "/api/user/~" (string-trim-left username "~"))
                "/api/user")))
    (srht-git--make-crud domain path)))

(defun srht-git-repos (domain &optional username query)
  "Retrive list of repository resources owned by this USERNAME from DOMAIN.
If USERNAME is nil the authenticated user is assumed.
QUERY is the query for the URI.  To retrieve the next page of results,
add start=:id to your QUERY, using the :id given by \"next\"."
  (let ((path (if username
                  (format "/api/~%s/repos" (string-trim-left username "~"))
                "/api/repos")))
    (srht-git--make-crud domain path query)))

(cl-defun srht-git-make (&key visibility description name)
  "Make paste parameters.
VISIBILITY must be one of \"public\", \"private\", or \"unlisted\".
DESCRIPTION is repository description, markdown is allowed.
NAME is repository name."
  (cl-assert (and (member visibility '("unlisted" "public" "private"))
                  (not (null name))))
  `((name . ,name)
    (description . ,description)
    (visibility . ,visibility)))

(defun srht-git-repo (domain repo-name &optional username &rest details)
  "Create, retrieve, delete or update a git repository from DOMAIN.

When retrieving, deleting or updating a repository, REPO-NAME must be
the name of an existing repository.

When retrieving if USERNAME is nil the authenticated user is assumed.

When updating, you must specify DETAILS (see `srht-git-make').
;; NOTE: Updating the name will create a redirect.

When creating repository omit REPO-NAME and specify DETAILS
\(see `srht-git-make'\)."
  (cond
   ((and (stringp repo-name) (stringp username))
    (srht-git--make-crud domain
      (format "/api/~%s/repos/%s" (string-trim-left username "~") repo-name)))
   ((and (stringp repo-name) details)
    (srht-git--make-crud domain
      (format "/api/repos/%s" repo-name) nil (apply #'srht-git-make details)))
   ((stringp repo-name)
    (srht-git--make-crud domain (format "/api/repos/%s" repo-name)))
   (t (srht-git--make-crud domain "/api/repos" nil (apply #'srht-git-make details)))))

(defmacro srht-git--endpoint (domain endpoint name username &optional query body form)
  "Generate crud for ENDPOINT and repository NAME for DOMAIN.
If USERNAME is nil the authenticated user is assumed.
QUERY is the query for the URI.  To retrieve the next page of results,
add start=:id to your QUERY, using the :id given by \"next\".
BODY is the body sent to the URI.
FORM is a content type."
  (let ((path (gensym "path")))
    `(let ((,path (if ,username
                      (format "/api/~%s/repos/%s/%s"
                              (string-trim-left ,username "~") ,name ,endpoint)
                    (format "/api/repos/%s/%s" ,name ,endpoint))))
       (srht-git--make-crud ,domain ,path ,query ,body ,form))))

(defun srht-git--endpoint-widen (endpoint domain name end &optional username body-or-query)
  "Extend the ENDPOINT for the repository NAME from DOMAIN to include END.
If USERNAME is nil the authenticated user is assumed.
BODY-OR-QUERY is the body or query sent to the URI."
  (let* ((plist (if body-or-query
                    (funcall domain endpoint name username body-or-query)
                  (funcall domain endpoint name username)))
         (path (plist-get plist :path)))
    (setf (plist-get plist :path)
          (concat path "/" end))
    plist))

(defun srht-git--artifact (domain name username body)
  "Helper function for `srht-git-repo-artifact'."
  (srht-git--endpoint domain "artifacts" name username body "multipart/form-data"))

(defun srht-git-repo-readme (domain name &optional username body form)
  "Retrieve, update or delete README override for repository NAME from DOMAIN.

If USERNAME is nil the authenticated user is assumed.
BODY is the body sent to the URI.  FORM is a content type."
  (srht-git--endpoint domain "readme" name username body form))

(defun srht-git-repo-refs (domain name &optional username query)
  "Endpoints for fetching git data from repository NAME from DOMAIN.
If USERNAME is nil the authenticated user is assumed.
QUERY is the query for the URI."
  (srht-git--endpoint domain "refs" name username query))

(defun srht-git-repo-log (domain name &optional username query)
  "List of the latest commit log for repository NAME from DOMAIN.
If USERNAME is nil the authenticated user is assumed.
QUERY is the query for the URI."
  (srht-git--endpoint domain "log" name username query))

(defun srht-git-repo-artifact (domain name ref body &optional username)
  "Attaches a file artifact to the specified REF and repository NAME from DOMAIN.
Note: this endpoint does not accept JSON.  Submit your request
as `multipart/form-data', with a single field: file in BODY."
  (srht-git--endpoint-widen #'srht-git--artifact domain name ref username body))

(defun srht-git-repo-log-ref (domain name ref &optional username query)
  "List of the latest commit resources starting from the given REF and DOMAIN.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed.  QUERY is the query for the URI."
  (srht-git--endpoint-widen #'srht-git-repo-log domain name ref username query))

(defun srht-git-repo-tree-ref (domain name ref &optional username query)
  "Return the tree resource for the given REF from DOMAIN.
Following the parent trees until the requested tree is found.
In other words, this lists the contents of a subdirectory by path.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed.  QUERY is the query for the URI."
  (srht-git--endpoint-widen #'srht-git-repo-tree domain name ref username query))

(defun srht-git-repo-tree-id (domain name id &optional username query)
  "Return the tree resource with the given ID from DOMAIN.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed.  QUERY is the query for the URI."
  (srht-git--endpoint-widen #'srht-git-repo-tree domain name id username query))

(defun srht-git-repo-tree (domain name &optional username)
  "Return the tree resource for the latest commit to the default branch.
DOMAIN is the domain name of the Sourcehut instance.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed."
  (srht-git--endpoint domain "tree" name username))

(defun srht-git--candidates (domain)
  "Return completion candidates for DOMAIN."
  (seq-map (pcase-lambda ((map (:created c)
                               (:visibility v)
                               (:name n)))
             (list n c v))
           (srht-results-get domain
             (or srht-git-repos
                 (srht-put srht-git-repos
                   domain (srht-retrive (srht-git-repos domain)))))))

(defun srht-git--annot (domain str)
  "Function to add annotations in the completions buffer for STR and DOMAIN."
  (srht-annotation (seq _n created visibility)
    (srht-git--candidates domain) str))

(defun srht-git--repo-name-read (domain)
  "Read a repository name in the minibuffer, with completion.
DOMAIN is the domain name of the Sourcehut instance."
  (srht-read-with-annotaion "Select repository: "
    (srht-git--candidates domain)
    (lambda (str) (srht-git--annot domain str))
    'sourcehut-git-repository))

(defvar srht-git-repo-name-history nil
  "History variable.")

(defun srht-git--else (domain plz-error)
  "An optional callback function.
Called when the request fails with two arguments, a ‘plz-error’ struct PLZ-ERROR
and domain name DOMAIN."
  (pcase-let* (((cl-struct plz-error response) plz-error)
               ((cl-struct plz-response status body) response))
    (pcase status
      (201 (srht-with-json-read-from-string body
             (map (:name repo-name)
                  (:owner (map (:canonical_name username))))
             (srht-kill-link domain 'git username repo-name)
             (srht-retrive (srht-git-repos domain)
                           :then (lambda (resp)
                                   (srht-put srht-git-repos domain resp)))))
      (204 (srht-retrive (srht-git-repos domain)
                         :then (lambda (resp)
                                 (srht-put srht-git-repos domain resp)
                                 (message "Deleted!"))))
      (_ (error "Unkown error with status %s: %S" status plz-error)))))

;;;###autoload
(defun srht-git-repo-create (domain visibility name description)
  "Create the NAME repository on an instance with the domain name DOMAIN.
Set VISIBILITY and DESCRIPTION."
  (interactive
   (list (srht-read-domain "Instance: ")
         (srht-read-visibility "Visibility: ")
	 (read-string "New git repository name: " nil
                      'srht-git-repo-name-history)
         (read-string "Repository description (markdown): ")))
  (srht-create (srht-git-repo domain nil nil
                              :visibility visibility
                              :name name
                              :description description)
               :then (lambda (_r))
               :else (lambda (err) (srht-git--else domain err))))

(defun srht-git--find-info (domain repo-name)
  "Find repository information by REPO-NAME from the DOMAIN instance."
  (catch 'found
    (seq-doseq (repo (plist-get (plist-get srht-git-repos domain) :results))
      (when (equal (cl-getf repo :name) repo-name)
        (throw 'found repo)))))

;;;###autoload
(defun srht-git-repo-update (domain repo-name visibility new-name description)
  "Update the REPO-NAME repository from the DOMAIN instance.
Set VISIBILITY, NEW-NAME and DESCRIPTION."
  (interactive
   (pcase-let* ((instance (srht-read-domain "Instance: "))
                (name (srht-git--repo-name-read instance))
                ((map (:visibility v)
                      (:description d))
                 (srht-git--find-info instance name)))
     (list instance
           name
           (srht-read-visibility "Visibility: " v)
           (read-string "Repository name: " nil
                        'srht-git-repo-name-history)
           (read-string "Repository description (markdown): " d))))
  (when (yes-or-no-p (format "Update %s repository?" repo-name))
    (srht-update (srht-git-repo domain repo-name nil
                                :visibility visibility
                                :name new-name
                                :description description)
                 :else (lambda (err) (srht-git--else domain err))
                 :then (lambda (_resp)
                         ;; NOTE: resp examle
                         ;; (:id 110277
                         ;;  :created 2022-04-29T14:05:29.662497Z
                         ;;  :updated 2022-04-29T14:43:53.155504Z
                         ;;  :name test-from-srht-6.el
                         ;;  :owner (:canonical_name ~akagi :name akagi)
                         ;;  :description nil
                         ;;  :visibility unlisted)
                         (message "Updated!")
                         (srht-retrive (srht-git-repos domain)
                                       :then (lambda (resp)
                                               (srht-put srht-git-repos domain resp)
                                               ))))))

;;;###autoload
(defun srht-git-repo-delete (domain repo-name)
  "Delete the REPO-NAME repository from the DOMAIN instance."
  (interactive
   (let ((instance (srht-read-domain "Instance: ")))
     (list instance (srht-git--repo-name-read instance))))
  (when (yes-or-no-p
         (format "This action cannot be undone.\n Delete %s repository?" repo-name))
    (srht-delete (srht-git-repo domain repo-name)
                 :then (lambda (_r))
                 :else (lambda (err) (srht-git--else domain err)))))

(provide 'srht-git)
;;; srht-git.el ends here
