;;; helm-bitbucket.el --- Search Bitbucket with Helm -*- lexical-binding: t -*-
;;
;; Copyright 2019 Peter Urbak
;;
;; Author: Peter Urbak <tolowercase@gmail.com>
;; Maintainer: Peter Urbak <tolowercase@gmail.com>
;; Created: 14th April 2019
;; Keywords: helm bitbucket
;; Homepage: https://github.com/dragonwasrobot/helm-bitbucket
;; Version: 0.1.1
;; Package-Requires: ((json "1.4") (helm "3.0"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; A helm interface for searching Bitbucket.

;; ** Installation

;; Download and install the `helm-bitbucket.el' file in your preferred way.

;; `helm-bitbucket' uses the credentials stored in `.authinfo.gpg' for
;; authenticating against the bitbucket API. So you need to add a line like
;; this: `machine api.bitbucket.org login <my-username> password <my-password>
;; port https' to your `.authinfo.gpg' file.

;; If you are not familiar with `.authinfo', check out
;; https://www.emacswiki.org/emacs/GnusAuthinfo for further information.

;; It is not currently possible to search across all Bitbucket repositories, so
;; `helm-bitbucket' searches all repositories for which your registered
;; Bitbucket user is a member. Thus, `helm-bitbucket' searches both your
;; personal repositories and the repositories of any Bitbucket team you are a
;; member of.

;; API Reference: https://developer.atlassian.com/bitbucket/api/2/reference/

;; ** Usage

;; Run `M-x helm-bitbucket' and type a search string. (The search begins after
;; you've typed at least 2 characters).

;; Hitting =RET= with an item selected opens the corresponding repository in your
;; browser.

;; *** Keys

;; | =C-n=   | Next item.                       |
;; | =C-p=   | Previous item.                   |
;; | =RET=   | Open repository page in browser  |
;; | =C-h m= | Full list of keyboard shortcuts. |

;;; Code:

(require 'url)
(require 'json)
(require 'helm)

(defun bitbucket-credentials ()
  "Return Bitbucket credentials from local .authinfo.gpg file.

Result format is (USERNAME . PASSWORD) if credentials are found,
nil otherwise."
  (let* ((bitbucket-auth-source (auth-source-user-and-password "api.bitbucket.org"))
         (username (car bitbucket-auth-source))
         (password (cadr bitbucket-auth-source)))
    (if (or (eq username nil) (eq password nil))
        nil
      (cons username password))))

(defun bitbucket-auth-header ()
  "Return 'Authorization' header for authenticating with Bitbucket API.

Result format is (\"Authorization\" . \"Basic <CREDENTIALS>\")."
  (let* ((bitbucket-credentials (bitbucket-credentials))
         (username (car bitbucket-credentials))
         (password (cdr bitbucket-credentials))
         (base64-header (base64-encode-string (concat username ":" password)))
         (auth-header (format "Basic %s" base64-header)))
    `("Authorization" . ,auth-header)))

(defun bitbucket-open-repository (repository)
  "Opens the web page for the specified Bitbucket REPOSITORY.

Opens the web page \"https://bitbucket.org/<user>/<repository>/\"
using the local machine's web browser of choice."
  (let* ((repository-links (assoc 'links repository))
        (repository-url (cdadr (assoc 'html repository-links))))
    (browse-url repository-url)))

(defvar url-http-end-of-headers)
(defun bitbucket-search (search-term)
  "Search Bitbucket for SEARCH-TERM, returning the results as a Lisp structure.

The SEARCH-TERM must be a substring of the repository name(s) you
want to search for."
  (let* ((url-request-extra-headers (list (bitbucket-auth-header)))
         (query-string (concat "name~\"" search-term "\""))
         (a-url (format "https://api.bitbucket.org/2.0/repositories?role=member&q=%s"
                        query-string)))
    (with-current-buffer
	      (url-retrieve-synchronously a-url)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun bitbucket-format-repository (repository)
  "Given a REPOSITORY, return a formatted string suitable for display."
  (let ((repository-name (cdr (assoc 'full_name repository))))
    repository-name))

(defun bitbucket-search-formatted (search-term)
  "Formats the resulting helm results when searching for SEARCH-TERM."
  (mapcar (lambda (repository)
	          (cons (bitbucket-format-repository repository) repository))
	        (cdr (assoc 'values (bitbucket-search search-term)))))

(defun helm-bitbucket-search ()
  "Helm function for searching bitbucket repositories."
  (bitbucket-search-formatted helm-pattern))

(defun helm-bitbucket-actions-for-repository (actions repository)
  "Return a list of helm ACTIONS available for this REPOSITORY."
  `((,(format "Open Repository - %s" (assoc 'full_name repository))
     . bitbucket-open-repository)))

;;;###autoload
(defvar helm-source-bitbucket-repository-search
  '((name . "Bitbucket")
    (volatile)
    (delayed)
    (requires-pattern . 2)
    (candidates . helm-bitbucket-search)
    (action-transformer . helm-bitbucket-actions-for-repository)))

;;;###autoload
(defun helm-bitbucket ()
  "Use Helm to search for and open Bitbucket repositories in your browser."
  (interactive)
  (if (eq (bitbucket-credentials) nil)
      (message "Could not find credentials for api.bitbucket.org in local .authinfo.gpg")
      (helm :sources '(helm-source-bitbucket-repository-search)
	          :buffer "*helm-bitbucket*")))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

(provide 'helm-bitbucket)
;;; helm-bitbucket.el ends here
