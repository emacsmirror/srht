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
(require 'transient)

(defvar srht-git-repositories nil
  "Authenticated user repos plist of the form (:instance repos ...).")

(defvar srht-git-cursor nil
  "Query result is paginated, so it has a cursor.
If you pass this value into repositories(cursor:\"...\") in a
subsequent request, you'll get the next page.")

(defun srht-git-repositories-next (cursor)
  "Next query from CURSOR."
  (let ((repositories `(:type repositories
                        :arguments (:filter (:count 30) :cursor ,cursor)
                        :fields
                        (cursor
                         (:type results
                          :fields (id
                                   name
                                   description
                                   created
                                   updated
                                   visibility))))))
    `(:query me
      :fields
      (canonicalName ,repositories))))

(cl-defun srht-git-request (instance query &optional (then 'sync))
  "Request git.INSTANCE.
QUERY is GraphQL.  THEN (see `plz')."
  (declare (indent 1))
  (srht--gql-api-request
   :instance instance
   :service 'git
   :token-host "git.sr.ht"
   :then then
   :query query))

(defun srht-git-repos (instance)
  "Retrive list of repositories from INSTANCE.
CALLBACK is called when the object has been completely retrieved.
Or CALLBACK may be `sync' to make a synchronous request."
  (declare (indent 1))
  (named-let loop ((query (srht-gql-query
                           (srht-git-repositories-next nil)))
                   (cursor "") (ac nil))
    (if cursor
        (let* ((resp (srht-git-request instance query))
               (results (srht-plist-get resp
                          :data :me :repositories :results))
               (pointer (srht-plist-get resp
                          :data :me :repositories :cursor)))
          (loop (srht-gql-query (srht-git-repositories-next pointer))
                pointer (append results ac)))
      ac)))

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

(defun srht-git--select-repo (instance)
  "Read a repository name in the minibuffer, with completion.
INSTANCE is the instance name of the Sourcehut instance."
  (srht-read-with-annotaion "Select repository: "
    (srht-git--candidates instance)
    (lambda (str) (srht-git--annot instance str))
    'sourcehut-git-repository))

(defun srht-git--message (instance &rest args)
  "Display a message at the bottom of the screen.
Update repositories from INSTANCE."
  (declare (indent 1))
  (apply #'message args)
  (srht-put srht-git-repositories
    instance (srht-git-repos instance)))

(defvar srht-git-repo-name-history nil
  "History variable.")

(defun srht-git--read-non-empty (prompt initial-input history)
  "Read non-empty string from the minibuffer, prompting with string PROMPT.
INITIAL-INPUT, HISTORY (see `read-from-minibuffer')."
  (save-match-data
    (cl-block nil
      (while t
        (let ((str (read-from-minibuffer prompt initial-input nil nil history)))
          (unless (string-empty-p str)
            (cl-return str)))
        (message "Please enter non-empty!")
        (sit-for 1)))))

(defun srht-git--read-url (prompt initial-input history)
  "Read URL string from the minibuffer, prompting with string PROMPT.
INITIAL-INPUT, HISTORY (see `read-from-minibuffer')."
  (save-match-data
    (cl-block nil
      (while t
        (let ((str (read-from-minibuffer prompt initial-input nil nil history))
              (regex (rx (group "http"
                                (zero-or-one "s")
                                "://")
                         (group (* alphanumeric)))))
          (when (eq 0 (string-match regex str))
            (cl-return str)))
        (message "Please enter url with https://")
        (sit-for 1)))))

;;;###autoload (autoload 'srht-git-repo-create "srht-git" nil t)
(transient-define-prefix srht-git-repo-create ()
  "Prefix that just shows off many typical infix types."
  ["Repo input"
   ("i" "instance" "instance="
    :always-read t
    :init-value (lambda (obj) (oset obj value (seq-first srht-instances)))
    :prompt "Instance: "
    :reader (lambda (prompt _initial-input _history)
              (srht-read-instance prompt)))
   ("n" "name" "name="
    :always-read t
    :prompt "Git repository name: "
    :reader srht-git--read-non-empty)
   ("v" "visibility" "visibility="
    :always-read t
    :allow-empty nil
    :prompt "Visibility: "
    :reader (lambda (prompt _initial-input _history)
              (srht-read-visibility prompt)))
   ("d" "description" "description="
    :always-read t
    :prompt "Repository description (markdown): ")
   ("u" "clone URL" "cloneUrl="
    :always-read t
    :prompt "Repository will be cloned from the given URL: "
    :reader srht-git--read-url)
   ]
  ["New repository"
   ("c" "create repository" srht-git-repo-create0)])

(defun srht-git--transient-value (arg)
  "Return the value of ARG."
  (transient-arg-value arg (transient-args 'srht-git-repo-create)))

(transient-define-suffix srht-git-repo-create0 ()
  "Create the NAME repository on an instance with the instance name INSTANCE.
Set VISIBILITY and DESCRIPTION."
  (interactive nil nil)
  (let ((instance (srht-git--transient-value "instance="))
        (name (let ((val (srht-git--transient-value "name=")))
                (if (or (null val) (string-empty-p val))
                    (error "Repository name required")
                  val)))
        (visibility (let ((val (srht-git--transient-value "visibility=")))
                      (if (or (null val) (string-empty-p val))
                          (error "Visibility required")
                        (intern (upcase val)))))
        (description (srht-git--transient-value "description="))
        (cloneurl (srht-git--transient-value "cloneUrl=")))
    (srht-git-request instance
      (srht-gql-mutation
       `(:query createRepository
         :arguments (:name ,name
                     :visibility ,visibility
                     :description ,description
                     :cloneUrl ,cloneurl)
         :fields (id)))
      (lambda (_r)
        (let* ((username (string-trim-left srht-username "~"))
               (url (srht--make-uri
                     instance 'git
                     (format "/~%s/%s" username name) nil)))
          (srht-copy-url url)
          (srht-browse-url url)
          (srht-put srht-git-repositories
            instance (srht-git-repos instance)))
        (srht-git--message instance
          "Sourcehut %s git repository created" name)))))

(defun srht-git--find-repo (instance repo-name)
  "Find repository information by REPO-NAME from the INSTANCE instance."
  (seq-find
   (lambda (repo)
     (equal (plist-get repo :name) repo-name))
   (plist-get srht-git-repositories (intern instance))))

(defun srht-git--repoinput (repo-name new-name visibility description)
  "Create a list from REPO-NAME, NEW-NAME, VISIBILITY, DESCRIPTION.
It will contains the data that is passed as the value of
the :input argument when making changes to the repository."
  (declare (indent defun))
  (let ((name-plist (unless (and (string-empty-p new-name)
                                 (equal repo-name new-name))
                      (list :name new-name))))
    `(:visibility ,(intern (upcase visibility))
      :description ,description
      ,@name-plist)))

(defun srht-git--repoinput-read ()
  "Read a strings from the minibuffer."
  (list (read-string "Repository new name (omit to leave them unchanged): "
                     nil 'srht-git-repo-name-history)
        (srht-read-visibility "Visibility: ")
        (read-string "Repository description (markdown): ")))

;;;###autoload
(defun srht-git-repo-update (instance repo-name)
  "Update the REPO-NAME repository from the INSTANCE instance.
Set VISIBILITY, NEW-NAME and DESCRIPTION."
  (interactive
   (let ((inst (srht-read-instance "Instance: ")))
     (list inst (srht-git--select-repo inst))))
  (pcase-let* ((repo (srht-git--find-repo instance repo-name))
               (id (plist-get repo :id))
               ((seq new-name visibility description)
                (srht-git--repoinput-read))
               (repoinput (srht-git--repoinput
                            repo-name new-name visibility description)))
    (when (yes-or-no-p (format "Update %s repository?" repo-name))
      (srht-git-request instance
        (srht-gql-mutation
         `(:query updateRepository
           :arguments (:id ,id :input ,repoinput)
           :fields (id)))
        (lambda (_r)
          (srht-git--message instance
            "Sourcehut %s git repository updated!" new-name))))))

;;;###autoload
(defun srht-git-repo-delete (instance repo-name)
  "Delete the REPO-NAME repository from the INSTANCE instance."
  (interactive
   (let ((instance (srht-read-instance "Instance: ")))
     (list instance (srht-git--select-repo instance))))
  (when (yes-or-no-p
         (format "This action cannot be undone.\n Delete %s repository?"
                 repo-name))
    (let ((id (plist-get
               (srht-git--find-repo instance repo-name) :id)))
      (srht-git-request instance
        (srht-gql-mutation
         `(:query deleteRepository
           :arguments (:id ,id)
           :fields (id)))
        (lambda (_r)
          (srht-git--message instance
            "Sourcehut %s git repository deleted!" repo-name))))))

(defun srht-git-repository-log (instance repo-name &optional cursor)
  "Sourcehut INSTANCE repository REPO-NAME log.
If you pass value of CURSOR into repositories(cursor:\"...\") in a
subsequent request, you'll get the next page."
  (let* ((log `(:type log
                :arguments (:cursor ,cursor)
                :fields (cursor
                         (:type results
                          :fields (shortId
                                   message
                                   (:type author
                                    :fields (name email)))))))
         (repository `(:type repository
                       :arguments (:name ,repo-name)
                       :fields (id name ,log)
                       )))
    (srht-git-request instance
      (srht-gql-query
       `(:query me
         :fields (,repository))))))

;;;###autoload
(defun srht-git-log (instance repo-name)
  "Display log of Sourcehut INSTANCE git repositories REPO-NAME."
  (interactive nil nil)
  (if-let ((resp (srht-git-repository-log instance repo-name))
           (log (srht-plist-get resp :data :me :repository :log :results)))
      (srht--view-log log)
    (user-error "No log")))

;;;###autoload
(defun srht-git-repos-list (instance)
  "Display a list of Sourcehut INSTANCE git repositories."
  (interactive
   (list (srht-read-instance "Instance: ")))
  (unless (fboundp 'make-vtable)
    (error "Vtable required"))
  (srht--view instance srht-git-repositories
    `("d" (lambda (obj)
            (srht-git-repo-delete ,instance (plist-get obj :name)))
      "u" (lambda (obj)
            (srht-git-repo-update ,instance (plist-get obj :name)))
      "c" (lambda (_obj) (srht-git-repo-create))
      "l" (lambda (obj)
            (srht-git-log ,instance (plist-get obj :name))))))

;;;;;;;;;;;;;;;;;;;LEGACY API;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(provide 'srht-git)
;;; srht-git.el ends here
