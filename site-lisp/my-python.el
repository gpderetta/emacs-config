(defadvice python-calculate-indentation (around outdent-closing-brackets)
  "Handle lines beginning with a closing bracket and indent them so that   they line up with the line containing the corresponding opening bracket."
  (save-excursion   
    (beginning-of-line)
    (let ((syntax (syntax-ppss)))
      (if (and (not (eq 'string (syntax-ppss-context syntax)))
               (python-continuation-line-p)
               (cadr syntax)
               (skip-syntax-forward "-")              
               (looking-at "\\s)"))
          (progn
            (forward-char 1)           
            (ignore-errors (backward-sexp))
            (setq ad-return-value (current-indentation)))       
        ad-do-it))))

(ad-activate 'python-calculate-indentation) 

;;;; python configuration for Python's flying circus support from Emacs
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; (setq
;;  python-shell-interpreter "C://Python27//python.exe"
;;  python-shell-interpreter-args
;;  "-i C://Python27//Scripts//ipython-script.py")

(defun my-python-mode-hook ()
  (define-key python-mode-map "\C-x \C-e" 'python-shell-send-region))


(defun my-inferior-python-mode-hook ()
  "inferior python customizations."
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  (local-set-key '[(control insert)] 'my-grab-output)
  (setq comint-input-sender 'n-shell-simple-send)
  )

(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'inferior-python-mode-hook 'my-inferior-python-mode-hook)

(provide 'my-python)
