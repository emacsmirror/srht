;;; srht.el --- Sourcehut               -*- lexical-binding: t; -*-

;; Copyright © 2022-2023  Free Software Foundation, Inc.

;; Author: Aleksandr Vityazev <avityazev@posteo.org>
;; Maintainer: Aleksandr Vityazev <avityazev@posteo.org>
;; Keywords: comm vc
;; Package-Version: 0.3
;; Homepage: https://sr.ht/~akagi/srht.el/
;; Keywords: comm
;; Package-Requires: ((emacs "27.1") (plz "0.7"))

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

(defcustom srht-domains '("sr.ht")
  "Sourcehut instance domain names."
  :type '(list (repeat :tag "Domain"
		       :inline t
		       (string :format "%v")))
  :group 'srht)

(defcustom srht-token nil
  "Personal access token for Sourcehut instance.
It is necessary to use Oauth personal token not Oauth2."
  :type 'string
  :group 'srht)

(defcustom srht-username ""
  "Sourcehut username.  May contain ~ or not, your choice."
  :type 'string
  :group 'srht)

(defcustom srht-browse-url-after-kill nil
  "When t open the url copied to `kill-ring' url `browse-url'."
  :type 'boolean
  :group 'srht)

(defun srht-token ()
  "Lookup variable `srht-token' if needed and return it.
If variable `srht-token' is nil or cannot be looked up or is empty, signal
an error."
  (if srht-token
      srht-token
    (let ((token (when-let ((f (plist-get
                                (car (auth-source-search :host "sr.ht"))
                                :secret)))
                   (funcall f))))
      (unless token
        (error "No srht-token"))
      (when (string-empty-p token)
        (error "srht-token must not be empty"))
      token)))

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

(defun srht--make-uri (domain service path query)
  "Construct a URI for making a request to Sourcehut DOMAIN.
SERVICE is name of the service, PATH is the path for the URI, and
QUERY is the query for the URI."
  (cl-assert (and (not (string-empty-p domain)) domain)
             nil "Require domain")
  (let ((host (format "%s.%s" service domain)))
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
(cl-defun srht--api-request (method &key domain service path query
                                    body (else #'srht--else)
                                    form (then 'sync) (as #'srht--as)
                                    &allow-other-keys)
  "Request METHOD from SERVICE.
Return the curl process object or, for a synchronous request, the
selected result.

DOMAIN is the domain name of the Sourcehut instance.

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
  (let ((uri (srht--make-uri domain service path query))
        (content-type (or form "application/json")))
    (plz method uri
      :headers `(,(cons "Content-Type" content-type)
                 ,(cons "Authorization" (concat "token " (srht-token))))
      :body body
      :then then
      :else else
      :as as)))

(defun srht-generic-crud (domain service path &optional query body form)
  "Return a list of arguments to pass to `srht--make-crud-request'.
DOMAIN is the domain name of the Sourcehut instance.
SERVICE is the service to used, and PATH is the path for the URI.
BODY is optional, if it is an empty list, the resulting list will not
contain the body at all.  FORM is optional.  QUERY is the query for the
URI."
  (let ((crud `(:domain ,domain :service ,service
                :path ,path :query , query :form ,form)))
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
    (completing-read prompt table nil t)))

(defun srht-copy-url (url)
  "Make URL the latest kill in the kill ring."
  (kill-new url)
  (message "Copied \"%s\" into clipboard" url))

(defun srht-browse-url (url)
  "Browse URL."
  (when srht-browse-url-after-kill
    (browse-url url)))

(defmacro srht-with-json-read-from-string (string pattern &rest body)
  "Read the JSON object contained in STRING.
Bind it with the ‘pcase’ PATTERN and do BODY."
  (declare (indent 1))
  `(pcase-let* ((json-object-type 'plist)
                (json-key-type 'keyword)
                (json-array-type 'list)
                (,pattern (json-read-from-string ,string)))
     ,@body))

(defun srht-read-domain (prompt)
  "Read domain name of the Sourcehut instance from `srht-domains' collection.
If the collection contains only one name, return it without completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space."
  (if (eq (length srht-domains) 1)
      (car srht-domains)
    (completing-read prompt srht-domains nil t)))

(defun srht-read-visibility (prompt &optional initial-input)
  "Select a visibility through `completing-read'.
PROMPT, INITIAL-INPUT see `completing-read' doc."
  (completing-read prompt '("private" "public" "unlisted") nil t initial-input))

(defun srht-results-get (domain plist)
  "Extract the value for the :results property.
For the existing PLIST for the DOMAIN domain name."
  (declare (indent 1))
  (plist-get (plist-get plist (intern domain)) :results))

(defmacro srht-put (plist domain val)
  "Change value in PLIST of DOMAIN to VAL if is not nil."
  (declare (indent 1))
  `(when ,val (setq ,plist (plist-put ,plist (intern ,domain) ,val))))

(defun srht-annotation (str visibility created)
  "Return an annotation for STR using VISIBILITY and CREATED."
  (declare (indent 1))
  (let* ((ws-char (string-to-char " "))
         (len (- 40 (length (substring-no-properties str))))
         (blank (make-string (if (> len 1) len 1) ws-char)))
    (concat blank visibility (make-string (- 12 (length visibility)) ws-char)
            created)))

(provide 'srht)
;;; srht.el ends here
