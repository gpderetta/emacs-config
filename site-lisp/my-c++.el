

(defun my-inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+"))))

(defun my-align-enum-class (langelem)
  (if (my-inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun my-align-enum-class-closing-brace (langelem)
  (if (my-inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun my-fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . my-align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . my-align-enum-class-closing-brace)))

;; Aligns all variable declarations in this buffer
(defun my-align-vars-buffer()
  "Aligns c/c++ variable declaration names on the same column in this buffer."
  (interactive)
  (save-excursion
    (let (beg end)
      (beginning-of-buffer)
      (setq beg (point))
      (end-of-buffer)
      (setq end (point))
      (align-vars beg end))))

(defun my-c-mode-common-hook ()
  ;; use Ellemtel style for all C like languages
  (c-set-style "bsd"))

(defun my-c++-hooks ()
  (rtags-enable-standard-keybindings)

  (hl-line-mode 1)  ;; highlight current line

  (setq ac-sources (append ac-sources '(ac-source-rtags)))

  (setq c-basic-offset 4
        tab-width 4
        indent-tabs-mode nil)

  (c-set-offset 'innamespace 0) ; do not indent in namespaces

  (my-fix-enum-class)
)

(add-hook 'c++-mode-hook 'my-c++-hooks)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(setq-default c-basic-offset 4)
(setq-default tab-width 4)

(provide 'my-c++)
