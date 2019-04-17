;;; helm-bitbucket.el --- Search Bitbucket with Helm.
;;
;; Copyright 2019 Peter Urbak
;;
;; Author: Peter Urbak <tolowercase@gmail.com>
;; Maintainer: Peter Urbak <tolowercase@gmail.com>
;; Keywords: helm bitbucket
;; URL: https://github.com/dragonwasrobot/helm-bitbucket
;; Created: 18th April 2019
;; Version: 0.1.0

;;; Commentary:
;;
;; A helm interface for searching Bitbucket.
;;
;; The search space is limited to a specific user or organization due to the
;; limitations of Bitbucket's current REST API.
;;
;; API Reference: https://developer.atlassian.com/bitbucket/api/2/reference/

;;; Code:
(require 'url)
(require 'json)
(require 'helm)

(defun bitbucket-credentials ()
  "Return Bitbucket credentials from local .authinfo.gpg file.

Result format is (USERNAME . PASSWORD)."
  (let* ((bitbucket-auth-source (auth-source-user-and-password "api.bitbucket.org"))
        (username (car bitbucket-auth-source))
        (password (cadr bitbucket-auth-source)))
    (cons username password)))

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

(defvar bitbucket-username)
(defvar url-http-end-of-headers)
(defun bitbucket-search (search-term)
  "Search Bitbucket for SEARCH-TERM, returning the results as a Lisp structure.

The SEARCH-TERM must be a substring of the repository name(s) you
want to search for."
  (let* ((url-request-extra-headers (list (bitbucket-auth-header)))
         (query-string (concat "name~\"" search-term "\""))
         (a-url (format "https://api.bitbucket.org/2.0/repositories/%s?q=%s"
                        bitbucket-username
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
  "Bring up a Bitbucket search interface in helm."
  (interactive)
  (helm :sources '(helm-source-bitbucket-repository-search)
	      :buffer "*helm-bitbucket*"))

(provide 'helm-bitbucket)
;;; helm-bitbucket.el ends here
