;; -*- eval: (git-auto-commit-mode 1) -*-

;; bootstrap begin
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; boostrap end

(add-to-list 'load-path "~/.emacs.d/site-lisp")


(use-package cli-lib)
(use-package emacs
  :demand t
  :config
  (setq-default inhibit-startup-message t
		indent-tabs-mode nil
		tab-width 4
		c-basic-offset 4
		transient-mark-mode t
		x-select-enable-clipboard t)

  (setq max-lisp-eval-depth 1000
	gc-cons-threshold (* 100 1024 1024)
	custom-safe-themes t

	locale-coding-system 'utf-8

	require-final-newline t
	indicate-empty-lines t

	message-truncate-lines t

	backup-by-copying t
	backup-directory-alist '(("." . "~/.emacs-backups")))

  (set-face-attribute 'default nil 
                      :height 80
                      :family "bitstream vera sans mono")
  
  (server-start)
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  (line-number-mode 1)
  (global-linum-mode 1)                   
  (column-number-mode 1)
  
  (electric-pair-mode)
  (delete-selection-mode t)
  (set-fill-column 80)

  (set-terminal-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system        'utf-8)

  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  
  (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.ixx$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.?xx$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.tpp$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

  (add-hook 'text-mode-hook (lambda () (hl-line-mode 1)))
  (add-hook 'text-mode-hook 'turn-on-auto-fill)

  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)

  :bind

  (([f1] . 'goto-line)
   ([(shift f3)] . 'gud-stepi)
   ([f7] . 'ff-find-other-file)

   ([(shift f9)] . 'ibuffer)
   ([f11] . 'shell)
   (([shift f11]) . 'run-python)

   ([C-left] . 'backward-word)
   ([C-right] . 'forward-word)
   ([C-up] . 'backward-paragraph)
   ([C-down] . 'forward-paragraph)
   ([C-prior] . 'beginning-of-buffer)
   ([C-next] . 'end-of-buffer)
   ([delete] . 'delete-char)
   ([(control tab)] . 'dabbrev-expand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers Save 
;; Add 'r' revert option to 'save-some-buffers
(setq save-some-buffers-action-alist (assq-delete-all 114 save-some-buffers-action-alist))
(add-to-list 
 'save-some-buffers-action-alist
 (list ?r  #'(lambda (buf)
               (save-excursion
                 (set-buffer buf)
                 (if  (null (buffer-file-name buf))
                     (message "Not applicable: no file")
                   (revert-buffer t t))))
       (purecopy "revert this buffer")))
(put 'erase-buffer 'disabled nil)

(use-package bookmark
  :init
  (setq bookmark-save-flag 1)
  :bind
  ([(shift f8)] . 'bookmark-set)
  ([f8] . 'bookmark-bmenu-list))

(use-package desktop
  :config
  (setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist))))

(use-package git-auto-commit-mode
  :ensure t
  :demand t)		

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-s-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-c C-<" . 'mc/mark-all-like-this)
   ("C-S-<mouse-1>" . 'mc/add-cursor-on-click)))

(use-package ripgrep
  :ensure t)

(use-package magit
  :ensure t
  :bind ([f6] . 'magit-status))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  :bind
  (("C-x C-j" . 'projectile-find-file)))

(use-package miscellanea
  :hook ((c-mode . my-c-mode-common-hook)
	 (c++-mode . my-c-mode-common-hook))
  :demand t
  :config
  (defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
    (cl-fet ((one-window-p (&optional nomini all-frames) t)) ad-do-it))
  :bind
  (("C-d" . 'my-toggle-window-dedicated)
   ("C-x C-r" . 'my-replace-last-sexp)
   ("C-x C-i" . 'my-insert-last-sexp)
   ([f2] . 'my-next-error)
   ("S-<f2>" . 'previous-error)
   
   ([f12] . 'my-really-kill-buffer)
   ("C-<delete>" . 'my-fold-whitespace)
   ("S-<delete>" . 'my-kill-whitespace)
   ("S-<backspace>" . 'my-kill-whitespace)))

(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line
  :config
  (setq powerline-arrow-shape 'curve
	powerline-default-separator-dir '(right . left)
	sml/theme 'powerline)
  (sml/setup))

(use-package python
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :bind
  (([shift f11] . 'run-python)))

(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings))

(use-package my-compile
  :bind
  (("S-<f4>" . 'my-compile)
   ([f4] . 'my-recompile))
  )

(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (setq indent-region-function #'lsp-format-region))

(use-package flycheck)

(use-package ivy
  :ensure t
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :ensure t
  :disable
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package lsp-mode
  :ensure t
  :config  
  (setq lsp-keymap-prefix "C-l"
	lsp-idle-delay 0.25)
  :hook ((c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :bind
  ("C-<return>" . 'lsp-find-definition))

(use-package yasnippet
  :ensure t
  :after lsp-mode
  :config
  (yas-global-mode 1))

(use-package company
  :after lsp-mode
  :config
  (setq  company-idle-delay 0.0
	company-minimum-prefix-length 1))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; split window vertically by default
;;
(unless (boundp 'split-window-preferred-function)
  (defadvice split-window (before by-default-split-horizontally activate)
    "When the system is going to split the window and we have
     lots of horizontal space, I want to split it horizontally!
     The default of splitting vertically (i.e. with a horizontal
     divider) leaves me looking at really long lines through
     really narrow spaces, which kinda blows."
    (when (and (not (ad-get-arg 2)) (> (window-width (or (ad-get-arg 0) (selected-window))) 160))
      (ad-set-arg 2 t)
      (ad-set-arg 1 nil)
      ))

  (defadvice split-window-vertically (around explicit-split-vertically activate protect)
    "See split-window advice \"by-default-split-horizontally\".
     However, if the system thinks it knows what it's doing,
     i.e. someone called split-window-vertically, I want to
     respect that"
    (ad-disable-advice 'split-window 'before 'by-default-split-horizontally)
    (ad-activate 'split-window)
    ad-do-it
    (ad-enable-advice 'split-window 'before 'by-default-split-horizontally)
    (ad-activate 'split-window)
    )
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(my-underwater))
 '(custom-safe-themes
   '("dbf18473be87255b3d32e68c027073d4c0db97bbbbadd43bc8bbd429df690f5e" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default))
 '(package-selected-packages
   '(which-key lsp-mode lsp magit ripgrep multiple-cursors dash-functional ag use-package))
 '(safe-local-variable-values '((mmm-classes . embedded-c++)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


