;;;; VANILLA TWEAKS

(menu-bar-mode 0)
(tool-bar-mode 0)
(delete-selection-mode 1)
(setq scroll-conservatively 10000)
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
(defalias 'ediff 'ediff-buffers)

(add-hook 'emacs-lisp-mode-hook (lambda ()
								  (eldoc-mode 1)))

(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "<home>") 'dired-up-directory))
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "<end>") 'dired-find-file))
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "<prior>") 'dired-previous-line))
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "<next>") 'dired-next-line))

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
				'(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit mwheel-scroll down up next-line previous-line backward-char forward-char))
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

(package-initialize)
(global-set-key (kbd "M-y") 'counsel-yank-pop)

;; match path with shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key [f6] 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; org mode
(add-hook 'org-mode-hook (lambda ()
						   (org-indent-mode 1)))

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

;; projectile
;(projectile-global-mode 1)

;; change wgrep key
(setq wgrep-enable-key "r")
(add-hook 'dired-mode-hook
		  (lambda ()
			(local-set-key
			 (kbd "r")
			 'wgrep-change-to-wgrep-mode)))

;; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4)
  (emmet-mode 1)
  (add-hook 'local-write-file-hooks
            (lambda ()
               (delete-trailing-whitespace)
               nil))
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-enable-current-element-highlight t)

;; SCSS mode
(add-hook 'scss-mode-hook (lambda()
							(rainbow-mode 1)
							(nlinum-mode 1)))



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

;; avy
(global-set-key (kbd "C-c ;") 'avy-goto-char)

;; ag
(global-set-key (kbd "C-c s") 'ag)

;; company-mode
(global-company-mode 1)
(company-statistics-mode 1)
(setq company-idle-delay 0)

;; nlinum
(add-hook 'prog-mode-hook  (lambda ()
							 (nlinum-mode 1)
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
      erc-timestamp-format "%H:%M "
      erc-fill-prefix "      "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)

;; ERC SCROLL TO BOTTOM
(setq erc-input-line-position -1)

;; turn off company mode in ERC
(add-hook 'erc-mode-hook (lambda ()
						   (company-mode 0)))
(defhydra hydra-insert-lipsum (global-map "C-c l")
    "Insert lorem ipsum"
    ("s" lorem-ipsum-insert-sentences "sentence")
    ("l" lorem-ipsum-insert-list "list")
    ("p" lorem-ipsum-insert-paragraphs "paragraph")
	("RET" newline "newline")
	("q" nil "quit"))
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
 '(bm-annotate-on-create t)
 '(bm-buffer-persistence t)
 '(bm-cycle-all-buffers t)
 '(bm-highlight-style (quote bm-highlight-only-fringe))
 '(bm-recenter t)
 '(bm-repository-file "/Users/imacintel/.emacs.d/.bm-repository")
 '(custom-enabled-themes (quote (spolsky)))
 '(custom-safe-themes
   (quote
	("0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" default)))
 '(dired-dwim-target t)
 '(electric-pair-mode t)
 '(emmet-move-cursor-between-quotes t)
 '(emmet-preview-default nil)
 '(erc-fill-function (quote erc-fill-static))
 '(erc-fill-static-center 14)
 '(erc-nick "paluche")
 '(erc-scrolltobottom-mode t)
 '(frame-resize-pixelwise t)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(initial-scratch-message ";; Scratch
")
 '(ivy-mode t)
 '(ivy-wrap t)
 '(menu-bar-mode nil)
 '(paradox-github-token t)
 '(projectile-completion-system (quote ivy))
 '(rainbow-html-colors-major-mode-list
   (quote
	(html-mode css-mode scss-mode php-mode nxml-mode xml-mode)))
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(web-mode-enable-current-column-highlight nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#161A1F" :foreground "#DEDEDE" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Menlo"))))
 '(bm-fringe-persistent-face ((t (:background "selectedMenuItemColor" :foreground "White"))))
 '(erc-current-nick-face ((t (:foreground "cyan" :weight bold))))
 '(erc-input-face ((t (:foreground "#555"))))
 '(erc-my-nick-face ((t (:foreground "#FF0000" :weight bold))))
 '(erc-nick-default-face ((t (:foreground "#FD971F" :weight bold))))
 '(erc-notice-face ((t (:foreground "#0C2D36" :weight bold))))
 '(erc-prompt-face ((t (:foreground "#EEEEEE" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "#A6E22E" :weight bold))))
 '(trailing-whitespace ((t (:background "#21262E"))))
 '(web-mode-comment-keyword-face ((t (:foreground "Pink" :slant italic :weight bold)))))
