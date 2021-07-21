;; -*- eval: (git-auto-commit-mode 1) -*-

(defun my-wrap-with-paren (&optional arg)
  "Select ARG next things and wrap them with a () pair."
  (interactive "p")
  (sp-select-next-thing-exchange arg)
  (execute-kbd-macro (kbd "(")))

(defun my-jsj-ac-show-help ()
  "show docs for symbol at point or at beginning of list if not on a symbol"
  (interactive)
  (let ((s (save-excursion
             (or (symbol-at-point)
                 (progn (backward-up-list)
                        (forward-char)
                        (symbol-at-point))))))
    (pos-tip-show (if (equal major-mode 'emacs-lisp-mode)
                      (ac-symbol-documentation s)
                    (ac-slime-documentation (symbol-name s)))
                  'popup-tip-face
                  ;; 'alt-tooltip
                  (point)
                  nil
                  -1)))

;; custom bufer reverting logic
(defun my-revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (buffer-file-name buffer)
        (progn
          (set-buffer buffer)
          (revert-buffer t t t)))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshing open files"))

;; select text between symbols
(defun my-select-quoted-text ()
  (interactive)
  (let (b1 b2)
    (skip-chars-backward "<'\"" )
    (setq b1 (point))
    (skip-chars-forward ">'\"" )
    (setq b2 (point))
    (set-mark b1)))

;; copy/cut the whole buffer
(defun my-copy-all ()
  "Copy all the buffer"
  (interactive)
  (kill-ring-save (point-min) (point-max)))

(defun my-cut-all ()
  "cut all the buffer"
  (interactive)
  (kill-region (point-min) (point-max)))

;; This function will format the whole file for you
(defun my-indent-whole-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil))

;; Dosify a path /foo/bar -> /foo/bar
(defun my-dosify ()
  (interactive)
  (let (end)
    (end-of-line)
    (setq end (point))
    (beginning-of-line)
    (replace-string "/" "\\")
    (comint-send-input)))

;; Unixify a path /foo/bar -> /foo/bar
(defun my-unixify ()
  (interactive)
  (let (end)
    (end-of-line)
    (setq end (point))
    (beginning-of-line)
    (replace-string "\\" "/")
    (comint-send-input)))

;; Kill all the damn '\r' chars...
(defun my-dos2unix ()
  "Convert a buffer from dos ^M end of lines to unix end of lines"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;;  no... wait... I like them!
(defun my-unix2dos ()
  "Opposite of dos2unix"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;; Tabs to space
(defun my-untabify-buffer ()
  "Untabify the whole (accessible part of the) current buffer"
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

;; Word count
(defun my-count-words-buffer ()
  "Count the number of words in the current buffer; print a
   message in the minibuffer with the result."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (1+ count)))
      (message "buffer contains %d words." count))))


;; I hate spaces at the end of lines
(defun my-remove-trailing-spaces ()
  "Remove trailing spaces in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d Trailing spaces removed from buffer." remove-count))))))
;; Kill all spaces
(defun my-kill-whitespace ()
  "Delete all whitespaces around point"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

;; Kill extra spaces
(defun my-fold-whitespace ()
  "Replace consecutive whitespaces around point with single
   space"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match " " nil nil))))))

(defun my-join-line ()
  (interactive)
  (delete-blank-lines)
  (next-line)
  (join-line)
  (c-indent-line-or-region))

;; Truncate lines
(defun my-toggle-truncate-lines ()
  "toggle the variable truncate-lines between true and false"
  (interactive)
  (if (eq truncate-lines 't)
      (set-variable 'truncate-lines nil)
    (set-variable 'truncate-lines 't)))


(defun my-really-kill-buffer ()
  (interactive)
  (kill-buffer))

(defun my-next-error ()
  (interactive)
  (or (and (not my-next-error-state) (next-error))
      (tags-loop-continue)))

(defun my-navigate-left ()
  (interactive) 
  (cond ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        (t (backward-word))))

(defun my-navigate-right ()
  (interactive) 
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t (forward-word))))

;; Toggle window dedication
(defun my-toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message 
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;;;;;;;;;;;;;;
;;; Lisp stuff

(defalias 'eb 'eval-buffer)

(defun my-replace-last-sexp ()
  (interactive) 
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(defun my-insert-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (insert (format ";; = %s" value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell mode Tweaks and Helpers

;; grab output of last command to 
(defvar my-grab-output-last-buffer nil "last buffer used by my-grab-output")
(defun my-grab-output (&optional buffer-name)
  (interactive)
  (save-excursion
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line 0)
      (let* ((start comint-last-input-end)
             (end (point))
             (buffer-old (buffer-name))
             (buffer-new (or buffer-name "*shell-output*")))
        (switch-to-buffer buffer-new)
        (insert-buffer-substring buffer-old start end))))

(defun my-shell-mode-hook ()
  "shell mode customizations."
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  (local-set-key '[(control insert)] 'my-grab-output)
  (setq comint-input-sender 'my-shell-simple-send)
  )

(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)

(defun my-shell-simple-send (proc command)
  "Various commands pre-processing before sending to shell."
  (cond
   ;; Checking for clear command and execute it.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer)
    )
   ;; Checking for man command and execute it.
   ((string-match "^[ \t]*man[ \t]*" command)
    (comint-send-string proc "\n")
    (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
    (setq command (replace-regexp-in-string "[ \t]+$" "" command))
    ;;(message (format "command %s command" command))
    (funcall 'man command)
    )
   ;; Send other commands to the default handler.
   (t (comint-simple-send proc command))
   )
  )


;;;;;;;;;;;;;;;;;;;
;;; Electric buffer
(setq my-buffer-menu-buffer-font-lock-keywords
      '(("^....[*]Man .*Man.*"   . font-lock-variable-name-face) ;Man page
        (".*Dired.*"             . font-lock-comment-face)       ; Dired
        ("^....[*]shell.*"       . font-lock-preprocessor-face)  ; shell buff
        (".*[*]scratch[*].*"     . font-lock-function-name-face) ; scratch buffer
        ("^....[*].*"            . font-lock-string-face)        ; "*" named buffers
        ("^..[*].*"              . font-lock-constant-face)      ; Modified
        ("^.[%].*"               . font-lock-keyword-face)))     ; Read only

(defun my-buffer-menu-custom-font-lock  ()
  (let ((font-lock-unfontify-region-function
         (lambda (start end)
           (remove-text-properties start end '(font-lock-face nil)))))
    (font-lock-unfontify-buffer)
    (set (make-local-variable 'font-lock-defaults)
         '(my-buffer-menu-buffer-font-lock-keywords t))
    (font-lock-fontify-buffer)))
(add-hook 'buffer-menu-mode-hook 'my-buffer-menu-custom-font-lock)
(add-hook 'electric-buffer-menu-mode-hook 'my-buffer-menu-custom-font-lock)
(add-hook 'electric-buffer-menu-mode-hook 'my-buffer-menu-custom-keybindings)

(defun my-llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))

;; Add a cc-mode style for editing LLVM C and C++ code
(c-add-style "llvm.org"
             '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)
				   (statement-cont . my-llvm-lineup-statement)))))

(defun my-c-mode-common-hook ()
  (c-set-style "llvm.org"))


(provide 'miscellanea)
