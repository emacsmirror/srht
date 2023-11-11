;;; srht-gql.el --- Sexp to GraphQL  -*- lexical-binding: t; -*-

;; Copyright © 2022-2023  Free Software Foundation, Inc.

;; Created: <2023-11-10 Fri>

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

;;

;;; Code:

(declare-function json-encode "json" (object))
(declare-function map-apply "map")

(defun srht-gql--string (val)
  (pcase val
    ((pred symbolp)
     (symbol-name val))
    ((pred integerp)
     (number-to-string val))
    ((pred stringp)
     (json-encode val))))

(defun srht-gql--string-join (plist)
  (let ((lst (map-apply (lambda (kw val)
                          (if val
                              (concat (substring (symbol-name kw) 1) ": "
                                      (srht-gql--string val))
                            ""))
                        plist)))
    (mapconcat #'identity lst ", ")))

(defun srht-gql--serialize-args (args)
  "Serialize type arguments ARGS."
  (when args
    (format "(%s)" (srht-gql--string-join args))))

(defun srht-gql--serialize-fields (lst)
  "Serialize all type fields from the LST."
  (let* ((type (plist-get lst :type))
         (args (plist-get lst :arguments))
         (fields (if type (plist-get lst :fields) lst)))
    (concat (when type (symbol-name type))
            (when args (srht-gql--serialize-args args))
            "{"
            (seq-reduce (lambda (acc field)
                          (concat acc " "
                                  (if (symbolp field)
                                      (symbol-name field)
                                    (srht-gql--serialize-fields field))))
                        fields "")
            "}")))

(defun srht-gql-serialize (query)
  "Serialize GraphQL QUERY."
  (let ((q (symbol-name (plist-get query :query)))
        (args (srht-gql--serialize-args (plist-get query :arguments)))
        (fields (srht-gql--serialize-fields (plist-get query :fields))))
    (concat "{" q args fields"}")))

(defun srht-gql-query (base-query)
  (json-encode `(("query" . ,(srht-gql-serialize base-query)))))

(defun srht-gql-mutation (base-query)
  (json-encode `(("mutation" . ,(srht-gql-serialize base-query)))))

(provide 'srht-gql)
;;; srht-gql.el ends here
