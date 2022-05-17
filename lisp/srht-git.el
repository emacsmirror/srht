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
  "Authenticated user repos.")

(defun srht-git--make-crud (path &optional query body form)
  "Make crud for git service.
PATH is the path for the URI.  BODY is the body sent to the URI.
FORM is a content type.  QUERY is the query for the URI."
  (srht-generic-crud 'git path query body form))

(defun srht-git-user (&optional username)
  "Retrieves a user resource.
If USERNAME is nil, the authenticated user is assumed."
  (let ((path (if username
                  (concat "/api/user/~" (string-trim-left username "~"))
                "/api/user")))
    (srht-git--make-crud path)))

(defun srht-git-repos (&optional username query)
  "Retrive list of repository resources owned by this USERNAME.
If USERNAME is nil the authenticated user is assumed.
QUERY is the query for the URI.  To retrieve the next page of results,
add start=:id to your QUERY, using the :id given by \"next\"."
  (let ((path (if username
                  (format "/api/~%s/repos" (string-trim-left username "~"))
                "/api/repos")))
    (srht-git--make-crud path query)))

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

(defun srht-git-repo (repo-name &optional username &rest details)
  "Create, retrieve, delete or update a git repository.

When retrieving, deleting or updating a repository, REPO-NAME must be
the name of an existing repository.

When retrieving if USERNAME is nil the authenticated user is assumed.

When updating, you must specify DETAILS (see `srht-git-make').
;; NOTE: Updating the name will create a redirect.

When creating repository omit REPO-NAME and specify DETAILS
\(see `srht-git-make'\)."
  (cond
   ((and (stringp repo-name) (stringp username))
    (srht-git--make-crud
     (format "/api/~%s/repos/%s" (string-trim-left username "~") repo-name)))
   ((and (stringp repo-name) details)
    (srht-git--make-crud
     (format "/api/repos/%s" repo-name) nil (apply #'srht-git-make details)))
   ((stringp repo-name) (srht-git--make-crud (format "/api/repos/%s" repo-name)))
   (t (srht-git--make-crud "/api/repos" nil (apply #'srht-git-make details)))))

(defmacro srht-git--endpoint (endpoint name username &optional query body form)
  "Generate crud for ENDPOINT and repository NAME.
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
       (srht-git--make-crud ,path ,query ,body ,form))))

(defun srht-git--endpoint-widen (endpoint name end &optional username body-or-query)
  "Extends the ENDPOINT for the repository NAME to include END.
If USERNAME is nil the authenticated user is assumed.
BODY-OR-QUERY is the body or query sent to the URI."
  (let* ((plist (if body-or-query
                    (funcall endpoint name username body-or-query)
                  (funcall endpoint name username)))
         (path (plist-get plist :path)))
    (setf (plist-get plist :path)
          (concat path "/" end))
    plist))

(defun srht-git--artifact (name username body)
  "Helper function for `srht-git-repo-artifact'."
  (srht-git--endpoint "artifacts" name username body "multipart/form-data"))

(defun srht-git-repo-readme (name &optional username body form)
  "Retrieve, update or delete README override for repository NAME.

If USERNAME is nil the authenticated user is assumed.
BODY is the body sent to the URI.  FORM is a content type."
  (srht-git--endpoint "readme" name username body form))

(defun srht-git-repo-refs (name &optional username query)
  "Endpoints for fetching git data from repository NAME.
If USERNAME is nil the authenticated user is assumed.
QUERY is the query for the URI."
  (srht-git--endpoint "refs" name username query))

(defun srht-git-repo-log (name &optional username query)
  "List of the latest commit log for repository NAME.
If USERNAME is nil the authenticated user is assumed.
QUERY is the query for the URI."
  (srht-git--endpoint "log" name username query))

(defun srht-git-repo-artifact (name ref body &optional username)
  "Attaches a file artifact to the specified REF and repository NAME.
Note: this endpoint does not accept JSON.  Submit your request
as `multipart/form-data', with a single field: file in BODY."
  (srht-git--endpoint-widen #'srht-git--artifact name ref username body))

(defun srht-git-repo-log-ref (name ref &optional username query)
  "List of the latest commit resources starting from the given REF.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed.  QUERY is the query for the URI."
  (srht-git--endpoint-widen #'srht-git-repo-log name ref username query))

(defun srht-git-repo-tree-ref (name ref &optional username query)
  "Return the tree resource for the given REF.
Following the parent trees until the requested tree is found.
In other words, this lists the contents of a subdirectory by path.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed.  QUERY is the query for the URI."
  (srht-git--endpoint-widen #'srht-git-repo-tree name ref username query))

(defun srht-git-repo-tree-id (name id &optional username query)
  "Return the tree resource with the given ID.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed.  QUERY is the query for the URI."
  (srht-git--endpoint-widen #'srht-git-repo-tree name id username query))

(defun srht-git-repo-tree (name &optional username)
  "Return the tree resource for the latest commit to the default branch.
NAME is a repository name.  If USERNAME is nil the authenticated user
is assumed."
  (srht-git--endpoint "tree" name username))

(defun srht-git--candidates ()
  "Return completion candidates."
  (seq-map (pcase-lambda ((map (:created c)
                               (:visibility v)
                               (:name n)))
             (list n c v))
           (plist-get (or srht-git-repos
                          (setq srht-git-repos
                                (srht-retrive (srht-git-repos))))
                      :results)))

(defun srht-git--annot (str)
  "Function to add annotations in the completions buffer for STR."
  (pcase-let* (((seq _n c v) (assoc str (srht-git--candidates)))
               (l (- 40 (length (substring-no-properties str))))
               (bb (make-string l (string-to-char " ")))
               (sb (cond
                    ((string= v "public") "      ")
                    ((string= v "private") "     ")
                    ((string= v "unlisted") "    "))))
    (concat bb (format "%s%s%s" v sb c))))

(defun srht-git--repo-name-read ()
  "Read a repository name in the minibuffer, with completion."
  (srht-read-with-annotaion "Select repository: "
    (srht-git--candidates) #'srht-git--annot 'sourcehut-git-repository))

(defvar srht-git-repo-name-history nil
  "History variable.")

(defun srht-git--else (plz-error)
  "An optional callback function.
Called when the request fails with one argument, a ‘plz-error’ struct PLZ-ERROR."
  (pcase-let* (((cl-struct plz-error response) plz-error)
               ((cl-struct plz-response status body) response))
    (pcase status
      (201 (srht-with-json-read-from-string body
             (map (:name repo-name)
                  (:owner (map (:canonical_name username))))
             (srht-kill-link 'git username repo-name)
             (srht-retrive (srht-git-repos)
                           :then (lambda (resp)
                                   (setq srht-git-repos resp)))))
      (204 (srht-retrive (srht-git-repos)
                         :then (lambda (resp)
                                 (setq srht-git-repos resp)
                                 (message "Deleted!"))))
      (_ (error "Unkown error with status %s: %S" status plz-error)))))

;;;###autoload
(defun srht-git-repo-create (visibility name description)
  "Create repository NAME with selected VISIBILITY  and DESCRIPTION."
  (interactive
   (list (completing-read "Visibility: "
			  '("private" "public" "unlisted") nil t)
	 (read-string "New git repository name: " nil
                      'srht-git-repo-name-history)
         (read-string "Repository description (markdown): ")))
  (srht-create (srht-git-repo nil nil
                              :visibility visibility
                              :name name
                              :description description)
               :then (lambda (_r))
               :else #'srht-git--else))

(defun srht-git--find-info (repo-name)
  "Find repository information by REPO-NAME."
  (catch 'found
    (seq-doseq (repo (plist-get srht-git-repos :results))
      (when (equal (cl-getf repo :name) repo-name)
        (throw 'found repo)))))

;;;###autoload
(defun srht-git-repo-update (repo-name visibility new-name description)
  "Update repository REPO-NAME.
Set VISIBILITY, NEW-NAME and DESCRIPTION."
  (interactive
   (pcase-let* ((name (srht-git--repo-name-read))
                ((map (:visibility v)
                      (:description d))
                 (srht-git--find-info name)))
     (list name
           (completing-read "Visibility: "
			    '("private" "public" "unlisted") nil t v)
           (read-string "Repository name: " nil
                        'srht-git-repo-name-history)
           (read-string "Repository description (markdown): " d))))
  (when (yes-or-no-p (format "Update %s repository?" repo-name))
    (srht-update (srht-git-repo repo-name nil
                                :visibility visibility
                                :name new-name
                                :description description)
                 :else #'srht-git--else
                 :then (lambda (_resp)
                         ;; NOTE: resp examle
                         ;; (:id 110277
                         ;;  :created 2022-04-29T14:05:29.662497Z
                         ;;  :updated 2022-04-29T14:43:53.155504Z
                         ;;  :name test-from-srht-6.el
                         ;;  :owner (:canonical_name ~akagi :name akagi)
                         ;;  :description nil
                         ;;  :visibility unlisted)
                         (srht-retrive (srht-git-repos)
                                       :then (lambda (resp)
                                               (setq srht-git-repos resp)))))))

;;;###autoload
(defun srht-git-repo-delete (name)
  "Delete NAME repository."
  (interactive (list (srht-git--repo-name-read)))
  (when (yes-or-no-p
         (format "This action cannot be undone.\n Delete %s repository?" name))
    (srht-delete (srht-git-repo name)
                 :then (lambda (_r))
                 :else #'srht-git--else)))

(provide 'srht-git)
;;; srht-git.el ends here
