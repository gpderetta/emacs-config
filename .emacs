(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/rtags/src")

;;;;;;;;;;;;;;;;;;;;;;;
;;;  ELPA sources   
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;;;;;;;;;;;;;;;;
;; Initial setup

; enable this to debug hangs on startup
(setq debug-on-quit nil)
; workaround tramp/ssh init hang, required by magit
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

(if (fboundp 'gnutls-available-p)
    (fmakunbound 'gnutls-available-p))
(setq tls-program '("gnutls-cli --tofu -p %p %h")
      imap-ssl-program '("gnutls-cli --tofu -p %p %s")
      smtpmail-stream-type 'starttls
      starttls-extra-arguments '("--tofu")
      )
(require 'package)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;
;;; Required Packages

(require 'cl)
(require 'emerge)
(require 'ido)
(require 'python)  ; indentation 

;; 3rd party packages
(require 'ag)
(require 'auto-complete-config)
(require 'buffer-move)
(require 'flymake-cursor)
(require 'framemove)
(require 'helm)
(require 'multiple-cursors)
;;(require 'org-install)
(require 'projectile)
(require 'rtags)
;;(require 'rtags-ac)
(require 'smartparens)
(require 'smex)
(require 'tramp)

;; from site-lisp
(require 'bubble-buffer)
(require 'miscellanea)
(require 'my-python)
(require 'my-c++)
(require 'my-dired)
(require 'mmm-auto)
;;(require 'cfiles)
;;(require 'show-wspace)
;;(require 'grep-buffers)
;;(require 'pos-tip)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General initialization
(server-start)

(setq max-lisp-eval-depth 1000
      gc-cons-threshold 100000000
      garbage-collection-message "garbage collection")

;; truncate too long messages to avoid the echo area 'jumping'
(setq message-truncate-lines t)
(setq-default inhibit-startup-message t)

(set-fill-column 80)

(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq-default indent-tabs-mode nil
              tab-width 4 
              c-basic-offset 4)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs-backups")))

(setq-default transient-mark-mode t
              x-select-enable-clipboard t)
(delete-selection-mode t)       ; delete the selection area with a keypress

;; Scrollbar
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)
(setq scroll-step 1)            ; scroll one line at a time

;; Bookmark autosave
(setq bookmark-save-flag 1)

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ixx$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.?xx$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(tool-bar-mode -1)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

(ac-config-default)
(global-auto-complete-mode t)

(projectile-global-mode)

(setq ido-enable-flex-matching t) ;; enable fuzzy matching
(ido-everywhere t)
(ido-mode t)

(smex-initialize)
(smex-auto-update)
(autoload 'idomenu "idomenu" nil t)

;;; tramp should not expire passwords
(setq password-cache-expiry nil)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
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
                register-alist)))

;; Colorfull emacs
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)
(show-paren-mode t)

(setq scalable-fonts-allowed nil)

(set-face-attribute 'default nil 
                    :height 60
                    :family "bitstream vera sans mono")

;; keyboard-escape-quit happily closes other windows if there is nothing else to
;; do, fix it
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (flet ((one-window-p (&optional nomini all-frames) t)) ad-do-it)) 

;; Show line/column numbers and function name
(line-number-mode 1)
(global-linum-mode 1)                   
(column-number-mode 1)

;; shift-<arrow> to move between frames
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; grep
(setq-default grep-command "grep -i ")

;; Ignore extensions in completion
(setq completion-ignored-extensions
      '(".o" ".lo" ".mh" ".elc" "~"
        ".bin" ".lbin" ".fasl" ".dvi" ".toc"
        ".aux" ".lof" ".blg" ".bbl"
        ".glo" ".idx" ".lot"))

;; good habbits...
(setq require-final-newline t)
(setq indicate-empty-lines t)

(setq org-log-done t)
(setq org-agenda-files (list "~/org/News.org" 
                             "~/org/SuperFresh.org" 
                             "~/org/Todo.org"))

(setq org-todo-keywords
      '((sequence "TODO" "BLOCKED" "DELAYED" "DONE")))


(add-hook 'text-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)

(add-hook 'emerge-startup-hook (lambda () (emerge-skip-prefers 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom Variables, set by emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ac-auto-show-menu t)
 '(ac-auto-start t)
 '(ac-delay 0.0)
 '(ac-show-menu-immediately-on-auto-complete t)
 '(ac-trigger-key "TAB")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(ansi-term-color-vector
   [unspecified "#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"] t)
 '(auto-revert-verbose nil)
 '(background-color "#002b36")
 '(background-mode dark)
 '(cc-other-file-alist
   '(("\\.cc\\'"
      (".hh" ".h"))
     ("\\.hh\\'"
      (".cc" ".C"))
     ("\\.c\\'"
      (".h"))
     ("\\.h\\'"
      (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))
     ("\\.C\\'"
      (".H" ".hh" ".h"))
     ("\\.H\\'"
      (".C" ".CC"))
     ("\\.CC\\'"
      (".HH" ".H" ".hh" ".h"))
     ("\\.HH\\'"
      (".CC"))
     ("\\.c\\+\\+\\'"
      (".h++" ".hh" ".h"))
     ("\\.h\\+\\+\\'"
      (".c++"))
     ("\\.cpp\\'"
      (".hpp" ".hh" ".h"))
     ("\\.hpp\\'"
      (".ipp" ".cpp"))
     ("\\.cxx\\'"
      (".hxx" ".hh" ".h"))
     ("\\.ipp\\'"
      (".cpp" ".hpp"))
     ("\\.hxx\\'"
      (".ixx" ".cxx"))
     ("\\.ixx\\'"
      (".cxx" ".hxx"))))
 '(compile-command ". ~/.bashrc && make -k ")
 '(cursor-color "#93a1a1")
 '(custom-enabled-themes '(my-underwater))
 '(custom-safe-themes
   '("dbf18473be87255b3d32e68c027073d4c0db97bbbbadd43bc8bbd429df690f5e" "49103d8c6c0258ecd289b0aa617824682d3e678e15f72ae94f659d6a528fd249" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "fa29856e364e2b46254503f913637ef6561faadae62668609cc671ecfcf1c3d2" "3ad55e40af9a652de541140ff50d043b7a8c8a3e73e2a649eb808ba077e75792" "60e70079a187df634db25db4bb778255eaace1ef4309e56389459fb9418b4840" "7a2c92b6267b84ae28a396f24dd832e29a164c1942f1f8b3fe500f1c25f8e09d" "1f3304214265481c56341bcee387ef1abb684e4efbccebca0e120be7b1a13589" "9ea054db5cdbd5baa4cda9d373a547435ba88d4e345f4b06f732edbc4f017dc3" "d971315c813b0269a86e7c5e73858070063016d9585492bd8d0f27704d50fee7" "5bff694d9bd3791807c205d8adf96817ee1e572654f6ddc5e1e58b0488369f9d" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d921083fbcd13748dd1eb638f66563d564762606f6ea4389ea9328b6f92723b7" "1278386c1d30fc24b4248ba69bc5b49d92981c3476de700a074697d777cb0752" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" "3abfa7be20483ace65a2dc36b5d8de341528bcafb5cafcb6fbb8fcba2a51465c" "a68fa33e66a883ce1a5698bc6ff355b445c87da1867fdb68b9a7325ee6ea3507" "383806d341087214fd44864170161c6bf34a41e866f501d1be51883e08cb674b" "466ae54a7b157ad02fd91da72b7871bccfb9bac98fdab95cf7a0d405c8572bd0" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" "9f42bccce1e13fa5017eb8718574db099e85358b9f424db78e7318f86d1be08f" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "a3f42f216305516caab36369dfe1fba8e14358423f6faf1d403d46a3515cd842" "04643edb183240f680d5465cf9e9ac3037086f701df09ce5d9183e6c69e73a7e" "050beead9159996a613ba4bc734de8b13b882f1c6596d1dffa4f51d096662cf6" "446c73cdfb49f1dab4c322e51ac00a536fb0e3cb7e6809b9f4616e0858012e92" "fe0a47cc3952fede574527a1c28ddf3a1af381fc1fb5843ca60d22e4c841011a" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "a2e0816c1a4bea13ac9c7b7c84f22408e1ffe23cfef4c6c75a71e3dafdc9343b" "9562e9eb5fd01677ac6b76b66f9a81338991fa9d270270802eeae5232c8da0a6" "75d4ccc5e912b93f722e57cca3ca1a15e079032cd69fd9bc67268b4c85639663" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "817ce7168851955b2d67a9dfb2f4bb283504e3be87b17932bd8a3ee4b43cfeb1" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "d7c5f11553cc4955f571771265d96a58cbf51c5d85c52c3f8d960c593d2d1bd8" "d6f40229a3be82430722f15837c2a049fc3b048c4bb2f43a944c89992fefb46f" "85751ed8fc10441b0878d372951c47e81f03594c59d5550521f4a53bfad115b2" "dd22788fed3f5aeb4fd23830d48c58dd4fa04fb258bdd92aa33dedf785810906" "cfd9f46d4bb6852a78e026d516a8c7425ce3888c9764841e0c70153d5378f5b9" "eff790a57ffd3bf7a7779d2ef8715c202b57a3ff49ffd117092644bbb72815d0" "23d6d37158f3160625ac55cc1f20f6f274f9af9c2ebc694180c21395c166e9cc" "2135c33244cafaa5cb5ffc39d4482a89bdc4a1b0f69570eda2701e6b33a82865" "936e5cac238333f251a8d76a2ed96c8191b1e755782c99ea1d7b8c215e66d11e" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))
 '(dabbrev-case-fold-search nil)
 '(dabbrev-case-replace nil)
 '(delete-selection-mode t)
 '(desktop-restore-eager 20)
 '(desktop-save t)
 '(desktop-save-mode t)
 '(dired-at-point-require-prefix t)
 '(dired-dwim-target t)
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#c7c7c7")
 '(ff-always-try-to-create nil)
 '(ff-search-directories
   '("." "~/packages/tibra/trunk/" "~/packages/hf/master/" "~/tibra.functional/1.00.00/include" "~/liffe/10.7.1/include" "~/tibra.meta/1.00.00/include" "~/tibra.math/1.00.02/include" "~/tibra.math/1.00.03/include" "~/tibra.allocation/1.00.00/include" "~/tibra/trunk/external/zeromq-2.1.7/win32/include" "~/tibra/trunk/external/libcap/linux/include" "~/tibra/trunk/external/libcap/win/include" "~/tibra/trunk/external/krx_encryptor/include" "~/tibra/trunk/external/sqlite-3071000/win32/include" "~/tibra/trunk/external/openonload-201210/include" "~/tibra/trunk/external/openonload-2012Q3-beta1/include" "~/tibra/trunk/external/matlab/include" "~/tibra/trunk/external/zeromq-2.1.11/win32/include" "~/tibra/trunk/gui/Libs/zeroMQ/libzmq/include" "~/vpm/1.04.00/thirdparty/yaml-cpp-0.3.0/include" "~/kx/0.0.1/include" "~/tibra.base/1.01.01/include" "~/tibra.container/1.00.00/include" "~/tibra.container/1.00.01/include" "~/avl_array/1.1.0/include" "~/tibra.serialization/1.00.01/include" "~/tibra.conversion/1.00.04/include" "~/build/tibra/trunk/" "/usr/local/include/" "/usr/include/"))
 '(ffap-require-prefix t)
 '(flymake-allowed-file-name-masks
   '(("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-my-make-init)
     ("\\.xml\\'" flymake-xml-init)
     ("\\.html?\\'" flymake-xml-init)
     ("\\.cs\\'" flymake-simple-make-init)
     ("\\.p[ml]\\'" flymake-perl-init)
     ("\\.php[345]?\\'" flymake-php-init)
     ("\\.js\\'" flymake-javascript-init)
     ("\\.css\\'" flymake-css-init)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
     ("\\.tex\\'" flymake-simple-tex-init)
     ("\\.idl\\'" flymake-simple-make-init)
     ("\\.spec\\'" flymake-specfile-init)
     ("\\.po\\'" flymake-pofile-init)))
 '(flymake-gui-warnings-enabled nil)
 '(flymake-log-level -1)
 '(flymake-max-parallel-syntax-checks 1)
 '(flymake-no-changes-timeout 2)
 '(flymake-start-syntax-check-on-find-file nil)
 '(foreground-color "#93a1a1")
 '(frame-brackground-mode 'dark)
 '(fringe-mode 6 nil (fringe))
 '(help-at-pt-display-when-idle '(flymake-overlay) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(ido-create-new-buffer 'always)
 '(ido-default-buffer-method 'selected-window)
 '(ido-everywhere t)
 '(linum-format " %d ")
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(main-line-separator-style 'chamfer)
 '(mmm-global-mode 'maybe nil (mmm-mode))
 '(package-selected-packages
   '(lively dash-functional smex smartparens rtags projectile multiple-cursors mmm-mode magit helm framemove flymake-cursor buffer-move auto-complete ag ace-jump-mode))
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111")
 '(rtags-autostart-diagnostics t)
 '(rtags-completions-enabled t)
 '(rtags-completions-timer-interval 0.1)
 '(safe-local-variable-values
   '((mmm-classes . universal)
     (mmm-classes . embedded-c++)
     (mode-classes . embedded-c++)
     (mode-classes . c++)
     (mode-classes . universal)
     (mmm-classes . c++-mode)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(solarized-contrast 'high)
 '(vc-annotate-background "#d4d4d4")
 '(vc-annotate-color-map
   '((20 . "#437c7c")
     (40 . "#336c6c")
     (60 . "#205070")
     (80 . "#2f4070")
     (100 . "#1f3060")
     (120 . "#0f2050")
     (140 . "#a080a0")
     (160 . "#806080")
     (180 . "#704d70")
     (200 . "#603a60")
     (220 . "#502750")
     (240 . "#401440")
     (260 . "#6c1f1c")
     (280 . "#935f5c")
     (300 . "#834744")
     (320 . "#732f2c")
     (340 . "#6b400c")
     (360 . "#23733c")))
 '(vc-annotate-very-old-color "#23733c"))


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

;; ffap
(defalias 'my-other-file  'ff-find-other-file)
(defadvice my-other-file (around my-ignore-include activate)
  (let ((ff-ignore-include t))
    ad-do-it))


;;;; mmm-mode
(mmm-add-classes
 '((embedded-c++
    :submode c++-mode
    :front "<code>"
    :back "</code>"
    )))

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

;;;;;;;;;;;;;;;;;;;;
;;; key-bindings ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart Parens
;; keybinding management

(define-key sp-keymap (kbd "M-i") 'sp-forward-sexp)
(define-key sp-keymap (kbd "M-o") 'sp-backward-sexp)

(define-key sp-keymap (kbd "M-0") 'sp-up-sexp)
(define-key sp-keymap (kbd "M-9") 'sp-backward-up-sexp)

(define-key sp-keymap (kbd "C-M-0") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-9") 'sp-backward-down-sexp)

(define-key sp-keymap (kbd "M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

;;(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
;;(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
;;(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
;;(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
;;(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
;;(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
;;(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer-move
;; C-S-<arrow> to move buffers around

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remap some keycodes for emacs in xterm

(define-key function-key-map "\e[1;3A" [M-up])
(define-key function-key-map "\e[1;3B" [M-down])
(define-key function-key-map "\e[1;3C" [M-right])
(define-key function-key-map "\e[1;3D" [M-left])
(define-key function-key-map "\e[1;5A" [C-up])
(define-key function-key-map "\e[1;5B" [C-down])
(define-key function-key-map "\e[1;5C" [C-right])
(define-key function-key-map "\e[1;5D" [C-left])
(define-key function-key-map "\e[5;5~" [C-prior]) ;Ctrl-PgUp
(define-key function-key-map "\e[6;5~" [C-next])  ;Ctrl-PgDown
(define-key function-key-map "\e[3;5~" [C-delete])

;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom 'IDE' mode
;; Fn keybindings

(global-set-key [f1]  'goto-line)

(global-set-key [f2] 'my-next-error)
(global-set-key [(shift f2)] 'previous-error)
(global-set-key [(control f2)] 'flymake-goto-next-error)
(global-set-key [(shift control f2)] 'flymake-goto-next-error)

(global-set-key [f3]  'gud-nexti)
(global-set-key [(shift f3)]  'gud-stepi)

(global-set-key [f4]  'recompile)
(global-set-key [(shift f4)]  'compile)
(global-set-key [(control f4)]  'kill-compilation)

(global-set-key [f5]  'my-syntax-recheck)
(global-set-key [(shift f5)]  'my-syntax-check)
(global-set-key [(control f5)]  'kill-compilation)

(global-set-key [f6]  'magit-status)
(global-set-key [(shift f6)]  'projectile-find-file)

(global-set-key [f7] 'my-other-file)
(global-set-key [(shift f7)] 'ff-find-other-file)

(global-set-key [(shift f8)] 'bookmark-set)
(global-set-key [ f8 ] 'bookmark-bmenu-list)

(global-set-key [f9] 'ido-switch-buffer)
(global-set-key [(shift f9)] 'ibuffer)

(global-set-key [f11] 'shell)
(global-set-key [(shift f11)] 'run-python)

(global-set-key [f12] 'my-really-kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors keybinding

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key [(shift control mouse-1)] 'mc/add-cursor-on-click)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bubble-buffer keybindings
;; navigate buffers of windows

(global-set-key [M-left]  'bubble-buffer-next)
(global-set-key [M-right] 'bubble-buffer-previous)
(global-set-key (kbd "<XF86Back>") 'bubble-buffer-next)
(global-set-key (kbd "<XF86Forward>") 'bubble-buffer-previous)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp inline evaluation

(global-set-key (kbd "C-x C-r") 'my-replace-last-sexp)
(global-set-key (kbd "C-x C-i") 'my-insert-last-sexp)

;;;;;;;;;;;;;;;;;;;
;; navigate in text

(global-set-key [C-left]  'backward-word)
(global-set-key [C-right] 'forward-word)
(global-set-key [C-up]    'backward-paragraph)
(global-set-key [C-down]  'forward-paragraph)
;; quickly jump to begin... or end
(global-set-key [C-prior]  'beginning-of-buffer)
(global-set-key [C-next]   'end-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;
;; Minor one-off stuff

; toggle window dedication
(global-set-key "\C-d"   'my-toggle-window-dedicated)

; need to rebind this as it is bound to C-d
(global-set-key [delete]   'delete-char)

(define-key sp-keymap (kbd "C-(") 'my-wrap-with-paren)

; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

; M-x replacement w/ IDO
(global-set-key (kbd "M-x") 'smex)

(global-set-key [(control return)]  'my-follow-link)
(global-set-key [(shift control return)]  'my-follow-link-other-window)

(global-set-key [(backtab)] 'my-auto-complete)
(global-set-key  [(control tab)]    'dabbrev-expand)

(global-set-key [C-delete] 'my-fold-whitespace)
(global-set-key [(shift delete)] 'my-kill-whitespace)
(global-set-key (kbd "<S-backspace>") 'my-kill-whitespace)

(put 'scroll-left 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
