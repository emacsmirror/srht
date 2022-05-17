;;; srht.el --- Sourcehut               -*- lexical-binding: t; -*-

;; Copyright © 2022  Free Software Foundation, Inc.

;; Author: Aleksandr Vityazev <avityazev@posteo.org>
;; Maintainer: Aleksandr Vityazev <avityazev@posteo.org>
;; Keywords: comm vc
;; Package-Version: 0.1
;; Homepage: https://sr.ht/~akagi/srht.el/
;; Keywords: comm
;; Package-Requires: ((emacs "27.1") (plz "0.1"))

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
;; https://man.sr.ht/api-conventions.md
;;

;;; Code:
(require 'cl-lib)
(require 'plz)
(require 'rx)
(require 'auth-source)

(defgroup srht nil
  "Customize options."
  :prefix "srht"
  :group 'comm)

(defcustom srht-domain "sr.ht"
  "Sourcehut domain."
  :type 'string
  :group 'srht)

(defcustom srht-token
  (if-let ((f (plist-get (car (auth-source-search :host "sr.ht"))
                         :secret)))
      (funcall f) "")
  "Personal access token for Sourcehut instance."
  :type 'string
  :group 'srht)

(defcustom srht-username ""
  "Sourcehut username."
  :type 'string
  :group 'srht)

(cl-defun srht--build-uri-string (scheme &key host path query)
  "Construct a URI string.
SCHEME should be a symbol.  HOST should be strings or nil
PATH should be strings or nil.  QUERY should be strings or nil."
  (concat
   (if scheme (concat (symbol-name scheme) ":") "")
   (if host
       (concat "//"
               (if (string-match-p ":" host)
                   (format "[%s]" host)
                 host))
     "")
   (pcase path
     ((or (pred null) (pred string-empty-p)) "")
     ((rx bol "/" (zero-or-more alnum)) path)
     (_ (error "Expected absolute path starting with \"/\" or empty string: %s" path)))
   (if query (concat "?" query) "")))

(defun srht--make-uri (service path query)
  "Construct a URI for making a request to Sourcehut.
SERVICE is name of the service, PATH is the path for the URI, and
QUERY is the query for the URI."
  (let ((host (format "%s.%s" service srht-domain)))
    (srht--build-uri-string
     'https :host host :path path :query query)))

(defun srht--else (plz-error)
  "An optional callback function.
Called when the request fails with one argument, a ‘plz-error’ struct PLZ-ERROR."
  (pcase-let* (((cl-struct plz-error response) plz-error)
               ((cl-struct plz-response status) response))
    (pcase status
      (201 (message "Created. Successful with status %s." status))
      (204 (message "No Content. Successful with status %s" status))
      (_ (error "Unkown error with status %s: %S" status plz-error)))))

(defun srht--as ()
  "Parse and return the JSON object following point.
A function, which is called in the response buffer with it
narrowed to the response body."
  (let ((json-object-type 'plist)
        (json-key-type 'keyword)
        (json-array-type 'list))
    (json-read)))

;; TODO add body-type to use with `multipart/from-data'
(cl-defun srht--api-request (method &key service path query
                                    body (else #'srht--else)
                                    form (then 'sync) (as #'srht--as)
                                    &allow-other-keys)
  "Request METHOD from SERVICE.
Return the curl process object or, for a synchronous request, the
selected result.

HEADERS may be an alist of extra headers to send with the
request.

PATH is the path for the URI and QUERY is the query for the URI.

If FORM is nil, the content type used will be `application/json'.

BODY is the body sent to the URI.

AS selects the kind of result to pass to the callback function
THEN (see `plz').
THEN is a callback function, which is called in the response data.
ELSE is an optional callback function called when the request
fails with one argument, a `plz-error' struct."
  (when (or (string-empty-p srht-token) (not srht-token))
    (error "Need a token"))
  (let ((uri (srht--make-uri service path query))
        (content-type (or form "application/json")))
    (plz method uri
      :headers `(,(cons "Content-Type" content-type)
                 ,(cons "Authorization" (concat "token " srht-token)))
      :body body
      :then then
      :else else
      :as as)))

(defun srht-generic-crud (service path &optional query body form)
  "Return a list of arguments to pass to `srht--make-crud-request'.
SERVICE is the service to used, and PATH is the path for the URI.
BODY is optional, if it is an empty list, the resulting list will not
contain the body at all.  FORM is optional.  QUERY is the query for the
URI."
  (let ((crud `(:service ,service :path ,path :query , query :form ,form)))
    (if body
        (append crud `(:body ,(if form body (json-encode body))))
      crud)))

(defun srht--make-crud-request (method args)
  "Make API request with METHOD and ARGS."
  (apply #'srht--api-request method (append (car args) (cdr args))))

(defun srht-create (&rest args)
  "Create an API request with ARGS using the POST method."
  (srht--make-crud-request 'post args))

(defun srht-retrive (&rest args)
  "Create an API request with ARGS using the GET method."
  (srht--make-crud-request 'get args))

(defun srht-update (&rest args)
  "Create an API request with ARGS using the PUT method."
  (srht--make-crud-request 'put args))

(defun srht-delete (&rest args)
  "Create an API request with ARGS using the DELETE method."
  (srht--make-crud-request 'delete args))

(defun srht-read-with-annotaion (prompt collection annot-function category)
  "Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
COLLECTION can be a list of strings, an alist or a hash table.
ANNOT-FUNCTION value should be a function for “annotating” completions.
The function should take one argument, STRING, which is a possible completion.
CATEGORY value should be a symbol describing what kind of text the
completion function is trying to complete."
  (declare (indent 1))
  (let ((table
         (lambda (string pred action)
           (if (eq action 'metadata)
               `(metadata
                 (category . ,category)
                 (annotation-function . ,annot-function)
                 (cycle-sort-function . identity)
                 (display-sort-function . identity))
             (complete-with-action action collection string pred)))))
    (completing-read prompt table)))

(defun srht-kill-link (service name resource)
  "Make URL the latest kill in the kill ring.
Constructed from SERVICE, NAME and RESOURCE."
  (kill-new (srht--make-uri service (format "/%s/%s" name resource) nil))
  (message "URL in kill-ring"))

(defmacro srht-with-json-read-from-string (string pattern &rest body)
  "Read the JSON object contained in STRING.
Bind it with the ‘pcase’ PATTERN and do BODY."
  (declare (indent 1))
  `(pcase-let* ((json-object-type 'plist)
                (json-key-type 'keyword)
                (json-array-type 'list)
                (,pattern (json-read-from-string ,string)))
     ,@body))

(provide 'srht)
;;; srht.el ends here
