;;; test.el --- Tests                                -*- lexical-binding: t; -*-

;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>

;; Author: Aleksandr Vityazev <avityazev@posteo.org>
;; Keywords: tests

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

(require 'ert)
(require 'srht)
(require 'srht-paste)
(require 'srht-git)

(ert-deftest srht-paste ()
  (should (equal (srht-paste "489fa091d5c6d5751769375a6f0e00447347adff")
                 '(:service paste
                   :path "/api/pastes/489fa091d5c6d5751769375a6f0e00447347adff"
                   :form nil))))

(ert-deftest srht-git-repo-retrive ()
  (pcase-let (((map (:path name)) (srht-git-repo "srht.el")))
    (should (equal "/api/repos/srht.el" name))))

;; (srht-retrive (srht-git-repo "srht.el"))
;; (srht-retrive (srht-git-repo "rrr" "~akagi"))
;; (srht-git-repo nil "~akagi" :visibility "ulnlisted" :name "test-repo" :description "hi")
;; (srht-git-repo nil nil :visibility "ulnlisted" :name "test-repo" :description "hi")

;; (srht-retrive (srht-git-repo-readme "rrr")) 404 error
;; (srht-retrive (srht-git-repo-readme "srht.el" "~akagi")) 404 error
;; (srht-git-repo-readme "srht.el" "~akagi" "hello" "text/html")

;; (srht-retrive (srht-git-repo-refs "rrr"))

;; (srht-git-repo-artifact "rrr" "refs/heads/master" "hello" "~akagi")

;; (setq test-log-1 (srht-retrive `(:next 2 ,@(srht-git-repo-log "rrr" "~akagi"))))

;; (srht-retrive (srht-git-repo-log-ref "rrr" "refs/heads/master" "~akagi"))

;; (srht-retrive (srht-git-repo-tree "rrr"))

(provide 'test)
;;; test.el ends here
