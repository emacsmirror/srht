;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Comment: guix build -f guix.scm

(use-modules (guix gexp)
             (guix packages)
             (guix git-download)
             (guix build-system emacs)
             ((guix licenses) #:prefix license:)
             (gnu packages emacs)
             (ice-9 receive)
             (ice-9 popen)
             (ice-9 rdelim))

(define-public emacs-plz
  (let ((commit "1d3efc036c9fdb7242499575e4d6bdcc928b0539")
        (revision "2")
        (version "0.1-pre"))
    (package
      (name "emacs-plz")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alphapapa/plz.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1vfa4igsvgspfx6qqzgdxb86hgbkcdr8hf63hr98yqfh7dngqjnz"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/alphapapa/plz.el")
      (synopsis "GNU Emacs HTTP library")
      (description #f)
      (license license:gpl3+))))

(define (last-commit-hash)
  (receive (in out pids)
      (pipeline `(("git" "rev-parse" "HEAD")))
    (let ((val (read-line in)))
      (close in)
      (close out)
      val)))

(define-public emacs-srht
  (let ((commit (last-commit-hash))
        (revision "0")
        (version "0.2"))
    (package
      (name "emacs-srht")
      (version (git-version version revision commit))
      (source (local-file "./lisp" #:recursive? #t))
      (build-system emacs-build-system)
      (arguments (list #:emacs emacs-next))
      (propagated-inputs (list emacs-plz))
      (home-page "https://git.sr.ht/~akagi/srht.el")
      (synopsis "Interact with sourcehut")
      (description #f)
      (license license:gpl3+))))
emacs-srht
