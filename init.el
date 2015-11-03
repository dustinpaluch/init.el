;;;; VANILLA TWEAKS

(menu-bar-mode 0)
(tool-bar-mode 0)
(delete-selection-mode 1)
(setq scroll-conservatively 10000)
(global-set-key (kbd "C-h g") 'customize-group)

;; disable bell for some events
(defun my-bell-function ()
  (unless (memq this-command
				'(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit mwheel-scroll down up next-line previous-line backward-char forward-char))
	(ding)))
(setq ring-bell-function 'my-bell-function)

(defun split-horizontally-for-temp-buffers ()
       "Split the window horizontally for temp buffers."
       (when (and (one-window-p t)
     	     (not (active-minibuffer-window)))
         (split-window-horizontally)))
(add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)

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
(package-initialize) ;; You might already have this line

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
(projectile-global-mode 1)

;; change wgrep key
(setq wgrep-enable-key "r")

;; web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4)
  (emmet-mode 1)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-enable-current-element-highlight t)


(defun my-disable-truncation ()
  "Turn off truncate lines"
  (setq truncate-lines t))

(add-hook 'ag-mode-hook 'my-disable-truncation)
(add-hook 'occur-mode-hook 'my-disable-truncation)
(add-hook 'dired-mode-hook 'my-disable-truncation)
(add-hook 'paradox-menu-mode-hook 'my-disable-truncation)

;; occur
(global-set-key (kbd "C-c o") 'occur)

;; avy
(global-set-key (kbd "C-c j") 'avy-goto-char)

;; ag
(global-set-key (kbd "C-c s") 'ag)

;; company-mode
(global-company-mode 1)
(company-statistics-mode 1)
(setq company-idle-delay 0.3)

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

;;;; VIA CUSTOMIZE GUI

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(emmet-move-cursor-between-quotes t)
 '(emmet-preview-default nil)
 '(frame-resize-pixelwise t)
 '(global-auto-revert-mode t)
 '(ido-vertical-mode t)
 '(ivy-mode t)
 '(menu-bar-mode nil)
 '(paradox-github-token t)
 '(projectile-completion-system (quote ivy))
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
 '(trailing-whitespace ((t (:background "#21262E"))))
 '(web-mode-comment-keyword-face ((t (:foreground "Pink" :slant italic :weight bold)))))
