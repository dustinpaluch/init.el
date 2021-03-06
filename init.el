;;;; PACKAGE SETUP

(package-initialize)

;; ADD LISP DIRECTORY TO LOAD PATH

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)


;;;; LOAD THEME

(use-package spolsky-theme
  :config
  (add-to-list
   'custom-safe-themes
   "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11")
  (add-to-list
   'custom-safe-themes
   "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa")
  (load-theme 'spolsky))
(use-package smart-mode-line
  :config
  (smart-mode-line-enable))


;;;; SET DEFAULTS
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq explicit-shell-file-name "/usr/local/bin/bash")
(setq custom-file "~/.emacs.d/customize.el")
(setq-default tab-width 4)
(setq tags-revert-without-query t)
(setq visible-bell t)

(setq confirm-kill-emacs 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq-default fill-column 80)
(setq frame-resize-pixelwise t)
(setq global-visual-line-mode nil)
(setq help-window-select t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(delete-selection-mode 1)
(blink-cursor-mode -1)
(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)
(setq global-auto-revert-mode t)
(setq scroll-conservatively 10000)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq-default indent-tabs-mode t)
;; backups
(setq backup-by-copying t ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backups")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
;; disable backups for files opened with su or sudo
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo")))))))) ; use versioned backups
;;indent after paste
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
		   (member
			major-mode
			'(emacs-lisp-mode lisp-mode clojure-mode scheme-mode haskell-mode
							  ruby-mode rspec-mode python-modec-mode c++-mode
							  objc-mode latex-mode plain-tex-mode scss-mode
							  css-mode))
		   (let ((mark-even-if-inactive transient-mark-mode))
			 (indent-region (region-beginning) (region-end) nil))))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; don't show messages when we're in the minibuffer
(add-hook 'minibuffer-setup-hook (lambda () (setq inhibit-message t)))
(add-hook 'minibuffer-exit-hook (lambda () (setq inhibit-message nil)))

(defun my-toggle-line-numbers ()
  (interactive)
  (display-line-numbers-mode 'toggle))
(global-set-key (kbd "<left-fringe> <mouse-1>") 'my-toggle-line-numbers)


;;;; REMOVE DEFAULT BINDINGS

(global-unset-key (kbd "s-p"))			; print
(global-unset-key (kbd "s-t"))			; macOS font dialog box
(global-unset-key (kbd "s-m"))			; minimize
(global-unset-key (kbd "C-x C-z"))		; suspend frame
(global-unset-key (kbd "C-z"))			; suspend frame

;;;; REMOVE AND REBIND DEFAULT BINDINGS

(global-unset-key (kbd "s-w"))			; close frame
(global-set-key (kbd "s-W") 'delete-frame)

;;;; ADD BINDINGS

(global-set-key (kbd "M-s r") 'rgrep)

;;;; PACKAGE CONFIGURATION

(use-package ag
  :bind (("C-c s" . ag))
  :config
  (setq ag-ignore-list '("*.min.*" "*.map"))
  (setq ag-reuse-window nil)) ; ag

(use-package ange-ftp
  :defer t
  :config
  (setq ange-ftp-netrc-filename "~/.authinfo.gpg"))

(use-package apropos
  :bind (("C-h a" . apropos))) ; apropos

(use-package auth-source
  :defer t
  :config
  (setq auth-source-debug 'trivia)
  (setq auth-sources '("~/.authinfo.gpg")))

(use-package buffer-move
  :bind (("<M-s-up>" . buf-move-up)
		 ("<M-s-down>" . buf-move-down)
		 ("<M-s-left>" . buf-move-left)
		 ("<M-s-right>" . buf-move-right))) ; buffer-mode

(use-package company
  :config
  (global-company-mode)
  (setq company-auto-complete-chars '(32 95 41 46))
  (setq company-dabbrev-code-modes
		'(prog-mode batch-file-mode csharp-mode css-mode erlang-mode
					haskell-mode jde-mode lua-mode python-mode web-mode
					scss-mode))
  (setq company-idle-delay 0.2)) ; company

(use-package company-statistics
  :config
  (company-statistics-mode))



(use-package counsel
  :bind (("M-y" . counsel-yank-pop)
		 ("M-x" . counsel-M-x)
		 ("C-c g" . counsel-git)
		 ("C-c j" . counsel-git-grep)
		 ("C-c k" . counsel-ag)
		 ("C-c SPC" . counsel-mark-ring))) ; counsel

(use-package css-mode
  :defer t
  :config
  (add-hook 'css-mode-hook 'my-css-mode-hook)
  (defun my-css-mode-hook ()
	(toggle-truncate-lines 1)))

(use-package cus-edit
  :bind (("C-h g" . customize-group))) ; cus-edit

(use-package delete-window
  :bind (("s-0" . delete-window)
		 ("s-1" . delete-other-windows))) ; delete-window

(use-package dired
  :defer t
  :config
  (setq dired-dwim-target t)
  (defun my-dired-mode-hook ()
	(dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'my-dired-mode-hook)
  (defun osx-open ()
    "Tell macOS to open the file at point."
    (interactive)
    (dired-smart-shell-command (concat "open " (dired-copy-filename-as-kill))))
  (defun osx-quick-look ()
    "Tell macOS to quick-view the file at point with `qlmanage -p`"
    (interactive)
    (dired-smart-shell-command
     (concat "qlmanage -p " (dired-copy-filename-as-kill) " > /dev/null 2>&1")))
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "SPC") 'osx-quick-look)
  (define-key dired-mode-map (kbd "<s-return>") 'osx-open)
  (defun my-dired-download-file (url file-name)
	(interactive
	 ;; We're forced to let-bind url here since we access it before interactive
	 ;; binds the function parameters.
	 (let ((url (read-from-minibuffer "URL: ")))
	   (list url
			 (read-from-minibuffer "File name: "
								   (car (last (split-string url "/")))))))
	(url-copy-file url (concat dired-directory file-name))
	(revert-buffer))) ; dired

(use-package dired-aux
  :defer t
  :config
  (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))) ; dired-aux

(use-package dired-x
  :bind (("C-x C-j" . dired-jump))) ; dired-x

(use-package ediff
  :defer t
  :config
  (defalias 'ediff 'ediff-buffers)) ; ediff

(use-package elisp-mode
  :defer t
  :config
  (defun my-emacs-lisp-mode-hook ()
    (eldoc-mode 1)
    (paredit-mode 1))
  (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)) ; elisp-mode

(use-package emmet-mode
  :defer t
  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-preview-default nil))

(use-package erc
  :commands erc
  :config
  (setq erc-accidental-paste-threshold-seconds 1)
  (setq erc-server-auto-reconnect nil)
  (setq erc-modules
		'(autojoin button completion fill irccontrols list match menu
				   move-to-prompt netsplit networks noncommands
				   readonly ring stamp track truncate))
  (setq erc-max-buffer-size 300000)
  (setq erc-input-line-position -1)
  (setq erc-lurker-threshold-time 3600)
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (setq erc-prompt-for-password nil)
  (setq erc-nick "paluche")
  ;; ERC BUFFER LOCAL FILL COLUMN
  (add-hook 'window-configuration-change-hook
			(lambda ()
			  (save-excursion
				(walk-windows
				 (lambda (w)
				   (let ((buffer (window-buffer w)))
					 (set-buffer buffer)
					 (when (eq major-mode 'erc-mode)
					   (setq erc-fill-column (- (window-width w) 2)))))))))
  ;; ;; MOVE ERC TIMESTAMP
  ;; (setq erc-timestamp-only-if-changed-flag nil
  ;; 	 erc-timestamp-format "%s "
  ;; 	 erc-fill-prefix "      ")
  ;; (setq erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (defun my-erc-mode-hook ()
    (company-mode -1)
    (electric-pair-local-mode -1))
  (add-hook 'erc-mode-hook 'my-erc-mode-hook)) ; erc

(use-package etags-update) ; etags-update

(use-package exec-path-from-shell
  :config
  ;; match path with shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))) ; exec-path-from-shell

(use-package expand-region
  :bind (("C-c x" . er/expand-region))) ; expand-region

(use-package geben
  :commands geben
  :bind (("<f16>" . geben)
		 ("<f19>" . geben-end))
  :config
  (defun my-vagrant-geben-path-prefix (path)
    (let ((ip (geben-session-ip-get session)))
      (format "/ssh:vagrant@%s:" ip)))
  (fset 'geben-get-tramp-spec-for (symbol-function 'my-vagrant-geben-path-prefix))) ; geben

(use-package grep
  :config
  (setq wgrep-enable-key "r")
  (add-hook 'grep-mode-hook 'my-truncate-hook)
  (add-to-list 'grep-find-ignored-files "*.map")
  (add-to-list 'grep-find-ignored-files "*.min.*")
  (defun my-truncate-hook () (toggle-truncate-lines 1))) ; grep

(use-package highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|))

(use-package highlight-parentheses
  :config
  (setq hl-paren-colors '("red" "orange" "yellow" "green" "cyan3"
						  "DodgerBlue3" "SlateBlue3" "HotPink3")))

(use-package hydra
  :init
  (require 'lorem-ipsum)
  (defhydra hydra-insert-lipsum (global-map "C-c l")
    "Insert lorem ipsum"
    ("s" lorem-ipsum-insert-sentences "sentence")
    ("l" lorem-ipsum-insert-list "list")
    ("p" lorem-ipsum-insert-paragraphs "paragraph")
    ("RET" newline "newline")
    ("q" nil "quit"))) ; hydra

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-saved-filter-groups
		'(("Projects"
		   ("beacon" (filename . "Projects/Beacon"))
		   ("crs" (filename . "Projects/crs")))))
  (setq ibuffer-saved-filters
		'(("FTP"
		   ((filename . "ftp")))
		  ("gnus"
		   ((or
			 (mode . message-mode)
			 (mode . mail-mode)
			 (mode . gnus-group-mode)
			 (mode . gnus-summary-mode)
			 (mode . gnus-article-mode))))
		  ("programming"
		   ((or
			 (mode . emacs-lisp-mode)
			 (mode . cperl-mode)
			 (mode . c-mode)
			 (mode . java-mode)
			 (mode . idl-mode)
			 (mode . lisp-mode))))))) ; ibuffer

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (add-to-list 'ivy-completing-read-handlers-alist
			   '(dired-create-directory . completing-read-default))
  (setq ivy-use-virtual-buffers 1)
  (setq ivy-wrap t)
  (add-to-list 'ivy-sort-functions-alist '(describe-function . string-lessp))
  (add-to-list 'ivy-sort-matches-functions-alist '(describe-function . string-lessp))
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (add-hook 'ivy-occur-mode-hook 'my-ivy-occur-mode-hook)
  (defun my-ivy-occur-mode-hook () (toggle-truncate-lines))
  :bind (("C-x b" . ivy-switch-buffer)
		 ("C-c C-r" . ivy-resume)
		 ("C-x B" . ivy-switch-buffer-other-window))) ; ivy

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'magit-process-find-password-functions
			'magit-process-password-auth-source)
  (setq magit-completing-read-function 'ivy-completing-read)) ; magit

(use-package mu4e
  :init
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
  :commands mu4e
  :config
  (setq mu4e-update-interval 120)
  (setq send-mail-function 'smtpmail-send-it)
  (setq mu4e-compose-signature-auto-include nil)
  (setq message-send-mail-function 'smtpmail-send-it
		smtpmail-smtp-server "smtp.gmail.com"
		smtpmail-smtp-service 587)
  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-maildir  "~/Mail/gmail")
  (setq mu4e-drafts-folder "/drafts")
  (setq mu4e-sent-folder   "/sent")
  (setq mu4e-trash-folder  "/trash")
  (setq mu4e-refile-folder "/all")
  ;; better html visibility for dark themes
  (setq shr-color-visible-luminance-min 80)
  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)
  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "mbsync gmail")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-compose-dont-reply-to-self t)
  ;; these must start with a "/", and must exist
  ;; (i.e.. /home/user/Maildir/sent must exist)
  ;; you use e.g. 'mu mkdir' to make the Maildirs if they don't
  ;; already exist
  (setq mu4e-maildir-shortcuts
		'( ("/inbox"  . ?i)
		   ("/drafts" . ?d)
		   ("/sent"   . ?s)
		   ("/trash"  . ?t)
		   ("/all"    . ?a)))

  (setq mu4e-bookmarks
		`(,(make-mu4e-bookmark
			:name "Unread messages"
			:query "flag:unread AND NOT flag:trashed"
			:key ?u)
		  ,(make-mu4e-bookmark
			:name "Today's inbox"
			:query "date:today..now maildir:/inbox"
			:key ?t)
		  ,(make-mu4e-bookmark
			:name "Last seven days inbox"
			:query "date:7d..now maildir:/inbox"
			:key ?w)))

  (add-to-list 'mu4e-view-actions
			   '("View in browser" . mu4e-action-view-in-browser) t)
  (define-key mu4e-headers-mode-map (kbd "f") 'mu4e-headers-mark-for-flag)
  (define-key mu4e-headers-mode-map (kbd "a") 'mu4e-headers-mark-for-refile)
  (define-key mu4e-headers-mode-map (kbd "r") 'mu4e-compose-reply)
  (define-key mu4e-headers-mode-map (kbd "G")
    (lambda () (interactive) (mu4e-update-mail-and-index t)))
  (defun my-set-dynamic-mu4e-headers-width ()
	(let* ((window-width (window-text-width (get-buffer-window)))
		   (offset 5)
		   (date-width 12)
		   (flags-width 6)
		   (from-width 20)
		   (subject-width (- window-width
							 (+ date-width flags-width from-width offset))))
	  (setq mu4e-headers-fields
			`((:human-date . ,date-width)
			  (:flags      .  ,flags-width)
			  (:subject    . ,subject-width)
			  (:from       . ,from-width)))))
  (add-hook 'mu4e-main-mode-hook 'my-set-dynamic-mu4e-headers-width)
  (add-hook 'mu4e-headers-mode-hook 'my-set-dynamic-mu4e-headers-width)) ; mu4e

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
		 ("C->" . mc/mark-next-like-this)
		 ("C-c C-<" . mc/mark-all-like-this)
		 ([s-mouse-1] . mc/add-cursor-on-click))) ; multiple-cursors

(use-package org
  :config
  (setq org-support-shift-select t)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (add-hook 'org-mode-hook 'my-org-mode-hook)
  (defun my-org-mode-hook ()
    (org-indent-mode 1)
    (local-set-key (kbd "s-p") 'my-org-pomodoro)
	(local-set-key (kbd "s-P") (lambda () (interactive)
								 (let ((org-clock-continuously t))
								   (my-org-pomodoro)))))
  (setq org-default-notes-file "~/Documents/notes.org")) ; org

(use-package org-pomodoro
  :commands my-org-pomodoro
  :config
  (setq org-pomodoro-finished-sound-p nil)
  (setq org-pomodoro-length 120)
  (setq org-pomodoro-long-break-length 0)
  (setq org-pomodoro-short-break-length 0)
  (setq org-pomodoro-long-break-sound-p nil)
  (setq org-pomodoro-play-sounds nil)

  (defun minutes-elapsed-today ()
	(let* ((time (decode-time))
		   (minutes (nth 1 time))
		   (hours (nth 2 time)))
	  (+ (* 60 hours) minutes)))

  (defun dynamic-pomodoro-length (cutoff)
	(let* ((minutes-elapsed (minutes-elapsed-today))
		   (minutes-until-cutoff (- cutoff minutes-elapsed)))
	  ;; return a value between 1 and 60 minutes depending on how close we are
	  (if (< minutes-until-cutoff 1)
		  org-pomodoro-length
		(min minutes-until-cutoff org-pomodoro-length))))

  (defun my-org-pomodoro ()
	(interactive)
	(let ((org-pomodoro-length (dynamic-pomodoro-length 1020)))
	  (org-pomodoro)))

  (add-hook 'org-pomodoro-finished-hook 'my-org-pomodoro-finished-hook)
  (defun my-org-pomodoro-finished-hook ()
    (beep)
    (call-process-shell-command
     "terminal-notifier -message \"\" -title \"Time's up\!\"" nil 0))) ; org-pomodoro

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (setq package-archive-priorities
		'(("melpa-stable" . 20)
		  ("gnu" . 10)
		  ("melpa" . 0)))) ; package

(use-package paradox
  :config
  (setq paradox-github-token t))

(use-package prog-mode
  :config
  (defun my-prog-mode-hook ()
    (electric-indent-mode 1)
    (highlight-parentheses-mode 1)
    (show-paren-mode)
    (ruler-mode))
  (add-hook 'prog-mode-hook 'my-prog-mode-hook)) ; prog-mode

(use-package projectile
  :init
  (projectile-global-mode 1)
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map
	(kbd "C-c p D") 'projectile-find-dir-other-window)
  (define-key projectile-mode-map
	(kbd "C-c p f") 'projectile-find-file)
  (define-key projectile-mode-map
	(kbd "C-c p d") 'projectile-find-dir)
  (define-key projectile-mode-map
    (kbd "C-M-.") 'projectile-find-tag)) ; projectile

(use-package rainbow-mode
  :config
  (setq rainbow-html-colors-major-mode-list
		'(html-mode css-mode scss-mode php-mode nxml-mode xml-mode)))

(use-package recentf
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-menu-items 20)) ; recentf

(use-package rjsx-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  :config
  (add-hook 'rjsx-mode-hook 'my-rjsx-mode-hook)
  (defun my-rjsx-mode-hook () (smartparens-mode 1)))

(use-package savehist
  :init (savehist-mode 1)) ; savehist

(use-package shackle
  :init
  (shackle-mode 1)
  :config
  (setq shackle-rules '((magit-revision-mode :same t))))

(use-package simple
  :init
  (column-number-mode 1)
  :bind (("<s-left>" . move-beginning-of-line)
		 ("<s-right>" . move-end-of-line)
		 ("<s-up>" . beginning-of-buffer)
		 ("<s-down>" . end-of-buffer))) ; simple

(use-package swiper
  :bind (("C-s" . swiper)
		 ("C-r" . swiper)
		 ("C-S-s" . swiper-all))) ; swiper

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  ;; ignore backup-directory-alist for TRAMP files
  (add-to-list 'backup-directory-alist
			   (cons tramp-file-name-regexp nil))) ; tramp

(use-package term
  :config
  (add-hook 'term-mode-hook 'my-term-mode-hook)
  (defun my-term-mode-hook () (yas-minor-mode -1))) ; term

(use-package transpose-frame
  :bind (("<f13>" . rotate-frame-anticlockwise)
		 ("<f15>" . rotate-frame-clockwise)
		 ("<f14>" . flip-frame))) ; transpose-frame

;; disable bell for some events
(setq ring-bell-function 'my-bell-function)

;; dunno where this hook gets defined
;;(defun my-git-rebase-mode-hook ()
;;  (shackle-mode -1))
;;(add-hook 'git-rebase-mode-hook 'my-git-rebase-mode-hook)

(use-package web-mode
  :bind (:map web-mode-map
			  ("C-j" . emmet-expand-line))
  :mode ("\\.phtml\\'"
		 "\\.php\\'"
		 "\\.js\\'"
		 "\\.php.inactive\\'")
  :config
  (setq web-mode-engines-alist '(("php" . "\\.php.inactive\\'")))
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-content-types-alist '(("jsx"  . ".*\\.js[x]?\\'")))

  (define-key web-mode-map (kbd "C-c <M-tab>") 'my-toggle-web-mode-indentation)
  (defun my-toggle-web-mode-indentation ()
	"Some documents have screwed up indentation that I can't
fix,so we can toggle the auto indent features off."
	(interactive)
	(if web-mode-enable-auto-indentation
		(progn (setq-local web-mode-enable-auto-indentation nil)
			   (electric-indent-local-mode -1)
			   (message "auto-indentation disabled"))
	  (setq-local web-mode-enable-auto-indentation t)
	  (electric-indent-local-mode 1)
	  (message "auto-indentation enabled")))

  (defun my-mark-inside-single-quotes ()
    "Mark the inside of the current single-quoted string, not
including the quotation marks."
    (interactive)
    (let ((cursor (point)))
      (when (search-backward "'" (line-beginning-position) t)
		(forward-char)
		(set-mark (point))
		(when (search-forward "'" (line-end-position) t)
		  (backward-char)
		  (exchange-point-and-mark)))))

  (defun my-mark-inside-double-quotes ()
    "Mark the inside of the current double-quoted string, not
including the quotation marks."
    (interactive)
    (let ((cursor (point)))
      (when (search-backward "\"" (line-beginning-position) t)
		(forward-char)
		(set-mark (point))
		(when (search-forward "\"" (line-end-position) t)
		  (backward-char)
		  (exchange-point-and-mark)))))

  (defun my-mark-outside-single-quotes ()
    "Mark the current single-quoted string, including the quotation
marks."
    (interactive)
    (let ((cursor (point)))
      (when (search-backward "'" (line-beginning-position) t)
		(set-mark (point))
		(forward-char)
		(when (search-forward "'" (line-end-position) t)
		  (exchange-point-and-mark)))))

  (defun my-mark-outside-double-quotes ()
    "Mark the current double-quoted string, including the
quotation marks."
    (interactive)
    (let ((cursor (point)))
      (when (search-backward "\"" (line-beginning-position) t)
		(set-mark (point))
		(forward-char)
		(when (search-forward "\"" (line-end-position) t)
		  (exchange-point-and-mark)))))

  (defun my-add-web-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list (append
							  er/try-expand-list
							  '(my-mark-inside-single-quotes
								my-mark-inside-double-quotes
								my-mark-outside-single-quotes
								my-mark-outside-double-quotes))))

  (defun my-yas-after-exit-snippet-hook ()
    (web-mode-buffer-highlight))
  (defun my-web-mode-hook ()
	(toggle-truncate-lines 1)
    (emmet-mode 1)
    (git-gutter+-mode 1)
    (setq require-final-newline nil)
    (my-add-web-mode-expansions)
    (add-hook 'yas-after-exit-snippet-hook
			  'my-yas-after-exit-snippet-hook)
	(smartparens-mode 1))
  (add-hook 'web-mode-hook  'my-web-mode-hook)) ; web-mode

(use-package which-key
  :init (which-key-mode 1)
  :config
  (setq which-key-idle-delay 3.0)) ; which-key

(use-package windmove
  :bind (("<C-s-up>" . windmove-up)
		 ("<C-s-down>" . windmove-down)
		 ("<C-s-left>" . windmove-left)
		 ("<C-s-right>" . windmove-right))) ; windmove

(use-package winner
  :init
  (winner-mode 1))

(use-package yaml-mode
  :mode "\\.yml\\'") ; yaml-mode

(use-package zop-to-char
  :bind (("M-z" . zop-to-char)
		 ("C-c C-c M-z" . zap-to-char))) ; zop-to-char

;;;; DEFINE AND BIND CUSTOM FUNCTIONS

(global-set-key (kbd "C-c i")
				(lambda () (interactive) (find-file user-init-file)))

(global-set-key (kbd "M-w") 'my-kill-ring-save-dwim)
(defun my-kill-ring-save-dwim (beg end &optional region)
  "If region is active, call copy-region-as-kill as usual. Otherwise, set region
to the current line, then call copy-region-as-kill."
  (interactive (list (mark) (point)
					 (prefix-numeric-value current-prefix-arg)))
  (save-excursion
    (if (region-active-p)
		(copy-region-as-kill beg end region)
      (move-beginning-of-line nil)
      (set-mark-command nil)
      (move-end-of-line nil)
      (setq deactivate-mark nil)
      (copy-region-as-kill beg end region))))

(defun my-clone-dwim ()
  "Duplicate the line at point."
  (interactive)
  (if mark-active
      (progn
		(kill-region (region-beginning) (region-end))
		(yank)
		(newline-and-indent)
		(yank))
    (progn
      (kill-whole-line)
      (yank)
      (yank)
      (backward-char))))

(defun my-bell-function ()
  (unless (memq this-command
				'(isearch-abort abort-recursive-edit exit-minibuffer
								keyboard-quit mwheel-scroll down up next-line
								previous-line backward-char forward-char))
    (ding)))

(defun my-kill-word-at-point ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (unless (equal bounds nil)
      (delete-region (car bounds) (cdr bounds)))))
(global-set-key (kbd "C-M-S-k") 'my-kill-symbol-at-point)

(defun my-kill-symbol-at-point ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (unless (equal bounds nil)
      (delete-region (car bounds) (cdr bounds)))))
(defun my-yank-replacing-line ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (set-mark-command nil)
    (move-end-of-line nil)
    (setq deactivate-mark nil)
    (delete-region (mark) (point))
    (yank)))

(global-set-key (kbd "s-@") 'my-clone-dwim)
(global-set-key (kbd "C-S-y") 'my-yank-replacing-line)
(global-set-key (kbd "M-D") 'my-kill-word-at-point)

(defun my-split-window-below-at-point ()
  "Split the selected window into two windows along the current line."
  (interactive)
  (split-window-below (+ 2 (my-visible-line-number-at-point))))

(defun my-split-window-right-at-point ()
  "Split the selected window into two windows along the current column."
  (interactive)
  (split-window-right (current-column)))

(defun my-visible-line-number-at-point ()
  "Get the line number at point relative to the first visible
line in the current window."
  (- (line-number-at-pos) (line-number-at-pos (window-start))))

;;;; LOAD CUSTOM-FILE

(load "~/.emacs.d/customize.el")
