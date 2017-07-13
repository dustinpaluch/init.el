;;;; VANILLA TWEAKS

(menu-bar-mode 0)
(tool-bar-mode 0)
(delete-selection-mode 1)
(setq scroll-conservatively 10000)
(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-h g") 'customize-group)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-c x") 'er/expand-region)
(global-set-key (kbd "<S-s-left>") nil) ; redundant?
(global-set-key (kbd "<S-s-right>") nil) ; redundant?
(global-set-key (kbd "<s-left>") 'move-beginning-of-line)
(global-set-key (kbd "<s-right>") 'move-end-of-line)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)
(global-set-key (kbd "<C-s-up>") 'windmove-up)
(global-set-key (kbd "<C-s-down>") 'windmove-down)
(global-set-key (kbd "<C-s-left>") 'windmove-left)
(global-set-key (kbd "<C-s-right>") 'windmove-right)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-@") 'my-clone-dwim)
(global-set-key (kbd "<f13>") 'rotate-frame-anticlockwise)
(global-set-key (kbd "<f15>") 'rotate-frame-clockwise)
(global-set-key (kbd "<f14>") 'flip-frame)

(global-set-key (kbd "s-p") nil)

;; make it harder to accidentally delete a frame
(global-set-key (kbd "s-w") nil)
(global-set-key (kbd "s-W") 'delete-frame)
;; disable OSX font dialog
(global-set-key (kbd "s-t") nil)

;; Who needs minimize on a hotkey?
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

;; overwrite line
(global-set-key (kbd "C-S-y") 'my-yank-replacing-line)

(global-set-key (kbd "M-D") 'my-kill-word-at-point)
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


(require 'ediff)
(defalias 'ediff 'ediff-buffers)

(column-number-mode 1)
(recentf-mode 1)
(add-hook 'emacs-lisp-mode-hook (lambda ()
								  (eldoc-mode 1)))

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

(global-set-key (kbd "M-w") 'my-kill-ring-save-dwim)

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

;; disable bell for some events
(defun my-bell-function ()
  (unless (memq this-command
				'(isearch-abort abort-recursive-edit exit-minibuffer
								keyboard-quit mwheel-scroll down up next-line
								previous-line backward-char forward-char))
	(ding)))
(setq ring-bell-function 'my-bell-function)

;; emacs backups
(setq
 backup-by-copying t ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/file backups")) ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t) ; use versioned backups

;; disable backups for files opened with su or sudo
(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo"))))))))



;; ignore backup-directory-alist for TRAMP files
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; indentation
(setq-default indent-tabs-mode t)

;; scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

;; visual lines
(global-visual-line-mode 1)

;;;; PACKAGE CONFIG

;; manually installed packages
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))


(package-initialize)

(global-set-key (kbd "M-y") 'counsel-yank-pop)

;; match path with shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; buffer-move
(global-set-key (kbd "<M-s-up>")     'buf-move-up)
(global-set-key (kbd "<M-s-down>")   'buf-move-down)
(global-set-key (kbd "<M-s-left>")   'buf-move-left)
(global-set-key (kbd "<M-s-right>")  'buf-move-right)


;; swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-S-s") 'swiper-all)
(global-set-key "\C-r" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key [f6] 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x B") 'ivy-switch-buffer-other-window)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

(add-hook 'grep-mode-hook 'my-truncate-hook)
(defun my-truncate-hook ()
  (toggle-truncate-lines 1))

;; term-mode
(add-hook 'term-mode-hook 'my-term-mode-hook)

(defun my-term-mode-hook ()
  (yas-minor-mode -1))

;; org mode
(add-hook 'org-mode-hook (lambda ()
						   (org-indent-mode 1)))
;; ivy-occur
(add-hook 'ivy-occur-mode-hook 'my-ivy-occur-mode-hook)
(defun my-ivy-occur-mode-hook ()
  (toggle-truncate-lines))

;; tramp
(setq tramp-default-method "ssh")

;; dired-x
(require 'dired-x)
(define-key dired-mode-map [mouse-2] 'dired-find-file)

;; zop-to-char
(global-set-key [remap zap-to-char] 'zop-to-char)
(global-set-key (kbd "C-c C-c M-z") 'zap-to-char) ; old zap-to-char

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'ivy-completing-read)

;; change wgrep key
(setq wgrep-enable-key "r")

;; wdired hotkey
; I should do this in dired-mode-map...
(add-hook 'dired-mode-hook
		  (lambda ()
			(local-set-key
			 (kbd "r")
			 'wdired-change-to-wdired-mode)))

;; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(setq web-mode-engines-alist
	 '(("php"    . "\\.php.inactive\\'")))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4)
  (emmet-mode 1)
  (git-gutter+-mode 1))

(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-enable-current-element-highlight t)

;; SCSS mode
(add-hook 'scss-mode-hook (lambda()
			    (rainbow-mode 1)
			    (rainbow-delimiters-mode 1)
			    (git-gutter+-mode 1)))

(defun my-disable-truncation ()
  "Locally enable line truncation"
  (visual-line-mode 0)
  (setq truncate-lines t))

(add-hook 'occur-mode-hook 'my-disable-truncation)
(add-hook 'dired-mode-hook 'my-disable-truncation)
(add-hook 'paradox-menu-mode-hook 'my-disable-truncation)
(add-hook 'ag-mode-hook 'my-disable-truncation)
(add-hook 'scss-mode-hook 'my-disable-truncation)
(add-hook 'buffer-menu-mode-hook 'my-disable-truncation)

;; ag
(global-set-key (kbd "C-c s") 'ag)

;; company-mode
(global-company-mode 1)
(company-statistics-mode 1)
(setq company-idle-delay 0)

;; prog-mode
(add-hook 'prog-mode-hook  (lambda ()
							 (electric-indent-mode 1)
							 (electric-pair-mode 1)))

;;multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key [s-mouse-1] 'mc/add-cursor-on-click)

;; ERC BUFFER LOCAL FILL COLUMN
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook 
		  '(lambda ()
			 (save-excursion
			   (walk-windows
				(lambda (w)
				  (let ((buffer (window-buffer w)))
					(set-buffer buffer)
					(when (eq major-mode 'erc-mode)
					  (setq erc-fill-column (- (window-width w) 2)))))))))

;; MOVE ERC TIMESTAMP
(setq erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%s "
      erc-fill-prefix "      "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)

;; ERC SCROLL TO BOTTOM
(setq erc-input-line-position -1)

;; turn off company mode in ERC
(add-hook 'erc-mode-hook (lambda ()
						   (company-mode 0)
						   (electric-pair-mode 0)))

(defhydra hydra-insert-lipsum (global-map "C-c l")
    "Insert lorem ipsum"
    ("s" lorem-ipsum-insert-sentences "sentence")
    ("l" lorem-ipsum-insert-list "list")
    ("p" lorem-ipsum-insert-paragraphs "paragraph")
	("RET" newline "newline")
	("q" nil "quit"))

;; macros

(fset 'wrap-css-property
	  [?\{ return ?\C-e ?\C-b return ?\C-p ?\C-p ?  ?\C-b])

(fset 'osx-open
	  [?w ?\M-! ?o ?p ?e ?n ?  ?\s-v return])

(global-set-key (kbd "<s-return>") 'osx-open)

(fset 'osx-quick-look
	  [?w ?\M-! ?q ?l ?m ?a ?n ?a ?g ?e ?  ?- ?p ?  ?. ?/ ?\C-y ?  ?> ?/ ?d ?e ?v ?/ ?n ?u ?l ?l ?  ?2 ?> ?& ?1 return])

(fset 'kill-nested-css-selector
	  [?\C-a ?\M-\\ backspace ?\C-b ?\M-x ?d ?e ?l ?e ?t ?e ?- ?p ?a ?i ?r return])


(add-hook 'dired-mode-hook
		  (lambda ()
			(local-set-key
			 (kbd "SPC")
			 'osx-quick-look)))

;; circe
(setq circe-network-options
      '(("Freenode"
         :tls t
         :nick "paluche"
         :sasl-username "paluche"
         :sasl-password ""
         :channels ("#emacs-circe")
         )))

(setq circe-reduce-lurker-spam t)
(setq circe-format-server-topic "*** Topic change by {userhost}: {topic-diff}")
(setq
 lui-time-stamp-position 'right-margin
 lui-fill-type nil)

(add-hook 'lui-mode-hook 'my-lui-setup)
(defun my-lui-setup ()
  (setq
   fringes-outside-margins t
   right-margin-width 5
   word-wrap t
   wrap-prefix "    "))

;; indent after paste
(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode lisp-mode
                                                      clojure-mode    scheme-mode
                                                      haskell-mode    ruby-mode
                                                      rspec-mode      python-mode
                                                      c-mode          c++-mode
                                                      objc-mode       latex-mode
                                                      plain-tex-mode  scss-mode
													  css-mode        web-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))

;; make it harder to accidentally delete a frame
(global-set-key (kbd "s-w") nil)
(global-set-key (kbd "s-W") 'delete-frame)


;;;; VIA CUSTOMIZE GUI

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-ignore-list (quote ("*.min.*")))
 '(ag-reuse-window nil)
 '(bm-annotate-on-create t)
 '(bm-buffer-persistence t)
 '(bm-cycle-all-buffers t)
 '(bm-highlight-style (quote bm-highlight-only-fringe))
 '(bm-recenter t)
 '(bm-repository-file "/Users/imacintel/.emacs.d/.bm-repository")
 '(column-number-mode t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-enabled-themes (quote (spolsky)))
 '(custom-safe-themes
   (quote
	("0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" default)))
 '(delete-selection-mode t)
 '(dired-dwim-target t)
 '(emmet-move-cursor-between-quotes t)
 '(emmet-preview-default nil)
 '(erc-fill-function (quote erc-fill-static))
 '(erc-fill-static-center 18)
 '(erc-fools (quote ("salih666")))
 '(erc-hide-list (quote ("JOIN" "QUIT")))
 '(erc-lurker-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-nick "paluche")
 '(erc-scrolltobottom-mode t)
 '(frame-resize-pixelwise t)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-visual-line-mode nil)
 '(help-window-select t)
 '(hl-paren-colors
   (quote
	("red" "orange" "yellow" "green" "cyan3" "DodgerBlue3" "SlateBlue3" "HotPink3")))
 '(ibuffer-saved-filter-groups
   (quote
	(("Projects"
	  ("beacon"
	   (filename . "Projects/Beacon"))
	  ("crs"
	   (filename . "Projects/crs"))))))
 '(ibuffer-saved-filters
   (quote
	(("FTP"
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
		(mode . lisp-mode)))))))
 '(initial-scratch-message ";; Scratch
")
 '(ivy-mode t)
 '(ivy-wrap t)
 '(menu-bar-mode nil)
 '(paradox-github-token t)
 '(projectile-completion-system (quote ivy))
 '(projectile-global-mode t)
 '(rainbow-html-colors-major-mode-list
   (quote
	(html-mode css-mode scss-mode php-mode nxml-mode xml-mode)))
 '(safe-local-variable-values
   (quote
	((projectile-project-name . "beacon")
	 (projectile-project-name . "crs"))))
 '(scss-sass-options (quote ("-E 'UTF-8'")))
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(web-mode-enable-current-column-highlight nil)
 '(winner-mode t)
 '(yas-global-mode t nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#161A1F" :foreground "#DEDEDE" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "nil" :family "Roboto Mono"))))
 '(bm-fringe-persistent-face ((t (:background "selectedMenuItemColor" :foreground "White"))))
 '(column-enforce-face ((t (:inherit trailing-whitespace))))
 '(eldoc-highlight-function-argument ((t (:inherit bold :underline t))))
 '(erc-current-nick-face ((t (:foreground "cyan" :weight bold))))
 '(erc-input-face ((t (:foreground "#777"))))
 '(erc-my-nick-face ((t (:foreground "#FF0000" :weight bold))))
 '(erc-nick-default-face ((t (:foreground "#FD971F" :weight bold))))
 '(erc-notice-face ((t (:foreground "#2A4B54" :weight bold))))
 '(erc-prompt-face ((t (:foreground "#EEEEEE" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "#A6E22E" :weight bold))))
 '(hl-paren-face ((t (:weight bold))) t)
 '(ivy-current-match ((t (:background "dark magenta"))))
 '(ivy-remote ((t (:foreground "coral1"))))
 '(mu4e-flagged-face ((t (:inherit outline-1 :weight bold))))
 '(mu4e-header-highlight-face ((t (:inherit swiper-line-face :weight bold))))
 '(mu4e-replied-face ((t (:inherit outline-4 :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "violet"))))
 '(swiper-line-face ((t (:background "#2A313A"))))
 '(trailing-whitespace ((t (:background "#21262E"))))
 '(web-mode-comment-keyword-face ((t (:foreground "Pink" :slant italic :weight bold))))
 '(web-mode-current-element-highlight-face ((t (:inherit swiper-line-face)))))
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
