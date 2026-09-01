;;; fj-inspect.el --- inspect fj.el -*- lexical-binding: t -*-

;; Copyright (C) 2026 Marty Hiatt
;; Author: Marty Hiatt <martianh@disroot.org>
;; Homepage: https://codeberg.org/martianh/fj.el

;; This file is not part of GNU Emacs.

;; This file is part of fj.el.

;; fj.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; fj.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with fj.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some tools to help inspect / debug / profile stuff, calqued from
;; fjdon-inspect.el.

;;; Code:

(require 'url-util)

(defgroup fj-inspect nil
  "Tools to help inspect fj.el."
  :group 'external)

(defcustom fj-inspect-profile-requests nil
  "Whether to profile requests info.
Parses outpout of function `url-debug' to list what requests a given
timeline entails."
  :type '(boolean))

(defvar fj-host)

(defun fj-inspect-profile-requests (&optional endpoint host)
  "Enable variable `url-debug' and call `fj-inspect-requests'.
Function to insert into timeline and other view loading functions.
Deletes contents of *URL-DEBUG* after calling `fj-inspect-requests'.
ENDPOINT is a string, to create a heading for a group of
requests.
HOST is a top level domain to filter requests for."
  (setq url-debug t)
  (when (get-buffer "*URL-DEBUG*")
    ;; before the new request: list previous set:
    (fj-inspect-requests endpoint (or host
                                      (url-host
                                       (url-generic-parse-url
                                        fj-host))))
    (with-current-buffer "*URL-DEBUG*"
      ;; then delete previous set:
      (erase-buffer))))

(defun fj-inspect-reqs-by-host (reqs host)
  "Return only the REQS whose host equals HOST."
  (cl-remove-if-not
   (lambda (x) ;; list of all reqs
     (let ((req-host (car (member-if
                           (lambda (y) (string-prefix-p "Host: " y))
                           x))))
       (when req-host
         (string= host (cadr (split-string req-host))))))
   reqs))

(defun fj-inspect-reqs-by-verb (reqs)
  "Filter all REQS for their Verb/Endpoint line.
Return a list."
  (flatten-list
   (mapcar
    (lambda (x) ;; reqs list
      (mapcar
       (lambda (y) ;; req strs
         (when (member (car (split-string y))
                       '("GET" "PUT" "POST" "PATCH" "DELETE"))
           y))
       x))
    reqs)))

(defun fj-inspect-requests (&optional endpoint host)
  "Collect recent url.el requests into a buffer.
Filters *URL-DEBUG* for requests, dumps them into *fj-requests*. Note
that for simplicity's sake in handling async requests, we collect all
the requests made until just *before* the page being loaded (and since
the last one).
ENDPOINT is a string, to create a heading for a group of
requests.
HOST is a top level domain to filter requests for."
  (with-current-buffer "*URL-DEBUG*"
    (let* ((list (split-string (buffer-string) "http -> Request is:"))
           (split-lists (mapcar (lambda (x)
                                  (split-string x "\n"))
                                list))
           (by-host (fj-inspect-reqs-by-host split-lists host))
           (verbs (fj-inspect-reqs-by-verb by-host)))
      (with-current-buffer
          (get-buffer-create "*fj-requests*")
        (goto-char (point-max))
        (insert
         (format "\n\n***\nCalls *before* hitting endpoint: %s\n%s"
                 (or endpoint "")
                 (mapconcat #'identity verbs "\n")))))))

(provide 'fj-inspect)
;;; fj-inspect.el ends here
