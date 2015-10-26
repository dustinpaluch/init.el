;;;; VANILLA TWEAKS

(menu-bar-mode 0)
(tool-bar-mode 0)
(delete-selection-mode 1)
(setq scroll-conservatively 10000)

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

;;;; PACKAGES

;; manually installed packages
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize) ;; You might already have this line

;; Space -> hyphen
(defadvice smex (around space-inserts-hyphen activate compile)
  (let ((ido-cannot-complete-command 
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall ,ido-cannot-complete-command)))))
    ad-do-it))

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

;; dired-x
(require 'dired-x)

;; SMEX
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; zop-to-char
(global-set-key [remap zap-to-char] 'zop-to-char)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'ivy-completing-read)

;; projectile
(projectile-global-mode 1)

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

;; org mode
(defun my-org-mode-hook ()
  "Hooks for Org mode."
  (org-indent-mode 1)
  )
(add-hook 'org-mode-hook  'my-org-mode-hook)

;; enable truncate lines based on context
(add-hook 'ag-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'occur-mode-hook (lambda () (setq truncate-lines t)))

;; occur
(global-set-key (kbd "C-c o") 'occur)

;; avy
(global-set-key (kbd "C-c j") 'avy-goto-char)

;; ag
(global-set-key (kbd "C-c s") 'ag)

;; company-mode
(setq company-idle-delay 0)

;; nlinum
(defun my-prog-mode-hook ()
  "Hooks for prog mode."
  (nlinum-mode 1))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

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
