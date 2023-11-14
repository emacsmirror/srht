;;; srht-git.el --- Sourcehut git                    -*- lexical-binding: t; -*-

;; Copyright © 2022-2023  Free Software Foundation, Inc.

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
(require 'srht-gql)

(defvar srht-git-repositories nil
  "Authenticated user repos plist of the form (:instance repos ...).")

(defvar srht-git-cursor nil
  "Query result is paginated, so it has a cursor.
If you pass this value into repositories(cursor:\"...\") in a
subsequent request, you'll get the next page.")

(defconst srht-git-gql-base-query
  '(:query me
    :fields
    (canonicalName
     (:type repositories
      :fields
      (cursor
       (:type results
        :fields(id name created updated visibility)))))))

(defun srht-git--gql-next-query (cursor)
  "Created next query from CURSOR."
  (pcase-let* ((plist (copy-sequence srht-git-gql-base-query))
               ((map (:fields (seq n lst))) plist))
    (plist-put
     plist
     :fields `(,n ,(plist-put lst :arguments `(:cursor ,cursor))))))

(defun srht-git-repos (instance)
  "Retrive list of repositories from INSTANCE.
CALLBACK is called when the object has been completely retrieved.
Or CALLBACK may be `sync' to make a synchronous request."
  (declare (indent 1))
  (named-let loop ((query (srht-gql-query
                           (srht-git--gql-next-query nil)))
                   (cursor "") (ac nil))
    (if cursor
        (pcase-let (((map (:data
                           (map (:me
                                 (map (:repositories
                                       (map (:cursor pointer)
                                            (:results results))))))))
                     (srht--gql-api-request
                      :instance instance
                      :service 'git
                      :token-host "git.sr.ht"
                      :query query)))
          (loop (srht-gql-query (srht-git--gql-next-query pointer))
                pointer (append results ac)))
      ac)))

(defun srht-git--make-crud (instance path &optional query body form)
  "Make a crud for the git service for the INSTANCE of the Sourcehut instance.
PATH is the path for the URI.  BODY is the body sent to the URI.
FORM is a content type.  QUERY is the query for the URI."
  (declare (indent 1))
  (srht-generic-crud instance 'git path query body form))

(defun srht-git-user (instance &optional username)
  "Retrieves a user resource from INSTANCE.
If USERNAME is nil, the authenticated user is assumed."
  (let ((path (if username
                  (concat "/api/user/~" (string-trim-left username "~"))
                "/api/user")))
    (srht-git--make-crud instance path)))

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

(defun srht-git-repo (instance repo-name &optional username &rest details)
  "Create, retrieve, delete or update a git repository from INSTANCE.

When retrieving, deleting or updating a repository, REPO-NAME must be
the name of an existing repository.

When retrieving if USERNAME is nil the authenticated user is assumed.

When updating, you must specify DETAILS (see `srht-git-make').
;; NOTE: Updating the name will create a redirect.

When creating repository omit REPO-NAME and specify DETAILS
\(see `srht-git-make'\)."
  (cond
   ((and (stringp repo-name) (stringp username))
    (srht-git--make-crud instance
      (format "/api/~%s/repos/%s" (string-trim-left username "~") repo-name)))
   ((and (stringp repo-name) details)
    (srht-git--make-crud instance
      (format "/api/repos/%s" repo-name) nil (apply #'srht-git-make details)))
   ((stringp repo-name)
    (srht-git--make-crud instance (format "/api/repos/%s" repo-name)))
   (t (srht-git--make-crud instance "/api/repos" nil (apply #'srht-git-make details)))))

(defmacro srht-git--endpoint (instance endpoint name username &optional query body form)
  "Generate crud for ENDPOINT and repository NAME for INSTANCE.
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
       (srht-git--make-crud ,instance ,path ,query ,body ,form))))

(defun srht-git--endpoint-widen (endpoint instance name end &optional username body-or-query)
  "Extend the ENDPOINT for the repository NAME from INSTANCE to include END.
If USERNAME is nil the authenticated user is assumed.
BODY-OR-QUERY is the body or query sent to the URI."
  (let* ((plist (if body-or-query
                    (funcall instance endpoint name username body-or-query)
                  (funcall instance endpoint name username)))
         (path (plist-get plist :path)))
    (setf (plist-get plist :path)
          (concat path "/" end))
    plist))

(defun srht-git--artifact (instance name username body)
  "Helper function for `srht-git-repo-artifact'."
  (srht-git--endpoint instance "artifacts" name username body "multipart/form-data"))

(defun srht-git-repo-readme (instance name &optional username body form)
  "Retrieve, update or delete README override for repository NAME from INSTANCE.

If USERNAME is nil the authenticated user is assumed.
BODY is the body sent to the URI.  FORM is a content type."
  (srht-git--endpoint instance "readme" name username body form))

(defun srht-git-repo-refs (instance name &optional username query)
  "Endpoints for fetching git data from repository NAME from INSTANCE.
If USERNAME is nil the authenticated user is assumed.
QUERY is the query for the URI."
  (srht-git--endpoint instance "refs" name username query))

(defun srht-git-repo-log (instance name &optional username query)
  "List of the latest commit log for repository NAME from INSTANCE.
If USERNAME is nil the authenticated user is assumed.
QUERY is the query for the URI."
  (srht-git--endpoint instance "log" name username query))

(defun srht-git-repo-artifact (instance name ref body &optional username)
  "Attaches a file artifact to the specified REF and repository NAME from INSTANCE.
Note: this endpoint does not accept JSON.  Submit your request
as `multipart/form-data', with a single field: file in BODY."
  (srht-git--endpoint-widen #'srht-git--artifact instance name ref username body))

(defun srht-git-repo-log-ref (instance name ref &optional username query)
  "List of the latest commit resources starting from the given REF and INSTANCE.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed.  QUERY is the query for the URI."
  (srht-git--endpoint-widen #'srht-git-repo-log instance name ref username query))

(defun srht-git-repo-tree-ref (instance name ref &optional username query)
  "Return the tree resource for the given REF from INSTANCE.
Following the parent trees until the requested tree is found.
In other words, this lists the contents of a subdirectory by path.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed.  QUERY is the query for the URI."
  (srht-git--endpoint-widen #'srht-git-repo-tree instance name ref username query))

(defun srht-git-repo-tree-id (instance name id &optional username query)
  "Return the tree resource with the given ID from INSTANCE.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed.  QUERY is the query for the URI."
  (srht-git--endpoint-widen #'srht-git-repo-tree instance name id username query))

(defun srht-git-repo-tree (instance name &optional username)
  "Return the tree resource for the latest commit to the default branch.
INSTANCE is the instance name of the Sourcehut instance.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed."
  (srht-git--endpoint instance "tree" name username))

(defun srht-git--candidates (instance)
  "Return completion candidates for INSTANCE."
  (seq-map (pcase-lambda ((map (:created c)
                               (:visibility v)
                               (:name n)))
             (list n c v))
           (plist-get
            (or srht-git-repositories
                (srht-put srht-git-repositories
                  instance (srht-git-repos instance)))
            (intern instance))))

(defun srht-git--annot (instance str)
  "Function to add annotations in the completions buffer for STR and INSTANCE."
  (pcase-let (((seq _n created visibility)
               (assoc str (srht-git--candidates instance))))
    (srht-annotation str visibility created)))

(defun srht-git--repo-name-read (instance)
  "Read a repository name in the minibuffer, with completion.
INSTANCE is the instance name of the Sourcehut instance."
  (srht-read-with-annotaion "Select repository: "
    (srht-git--candidates instance)
    (lambda (str) (srht-git--annot instance str))
    'sourcehut-git-repository))

(defvar srht-git-repo-name-history nil
  "History variable.")

;;;###autoload
(defun srht-git-repo-create (instance visibility name description)
  "Create the NAME repository on an instance with the instance name INSTANCE.
Set VISIBILITY and DESCRIPTION."
  (interactive
   (list (srht-read-instance "Instance: ")
         (srht-read-visibility "Visibility: ")
	 (read-string "New git repository name: " nil
                      'srht-git-repo-name-history)
         (read-string "Repository description (markdown): ")))
  (srht-create (srht-git-repo instance nil nil
                              :visibility visibility
                              :name name
                              :description description)
               :then (lambda (results)
                       (pcase-let* (((map (:name repo-name)
                                          (:owner (map (:canonical_name username))))
                                     results)
                                    (url (srht--make-uri
                                          instance 'git
                                          (format "/%s/%s" username repo-name) nil)))
                         (srht-copy-url url)
                         (srht-browse-url url)
                         (srht-put srht-git-repositories
                           instance (srht-git-repos instance))
                         ))))

(defun srht-git--find-info (instance repo-name)
  "Find repository information by REPO-NAME from the INSTANCE instance."
  (catch 'found
    (seq-doseq (repo (plist-get srht-git-repositories instance))
      (when (equal (cl-getf repo :name) repo-name)
        (throw 'found repo)))))

;;;###autoload
(defun srht-git-repo-update (instance repo-name visibility new-name description)
  "Update the REPO-NAME repository from the INSTANCE instance.
Set VISIBILITY, NEW-NAME and DESCRIPTION."
  (interactive
   (pcase-let* ((instance (srht-read-instance "Instance: "))
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
    (srht-update (srht-git-repo instance repo-name nil
                                :visibility visibility
                                :name new-name
                                :description description)
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
                         (srht-put srht-git-repositories
                           instance (srht-git-repos instance))
                         ))))

;;;###autoload
(defun srht-git-repo-delete (instance repo-name)
  "Delete the REPO-NAME repository from the INSTANCE instance."
  (interactive
   (let ((instance (srht-read-instance "Instance: ")))
     (list instance (srht-git--repo-name-read instance))))
  (when (yes-or-no-p
         (format "This action cannot be undone.\n Delete %s repository?" repo-name))
    (srht-delete
     (srht-git-repo instance repo-name)
     :as 'string
     :then (lambda (_r)
             (message
              (format "Sourcehut %s git repository deleted!" repo-name))
             (srht-put srht-git-repositories
               instance (srht-git-repos instance))
             ))))

;;;###autoload
(defun srht-git-repos-list (instance)
  "Display a list of Sourcehut INSTANCE git repositories."
  (interactive
   (list (srht-read-instance "Instance: ")))
  (unless (fboundp 'make-vtable)
    (error "Vtable required"))
  (srht--view instance srht-git-repositories
    `("d" (lambda (obj)
            (srht-git-repo-delete ,instance (plist-get obj :name))))))

(provide 'srht-git)
;;; srht-git.el ends here
