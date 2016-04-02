(provide 'my-compile)

;;
;; my-compile, my-recompile - easy compilation with scrolling errors, and easy
;; recompilation without worrying about what buffer you're in.
;;

;; Used by my-compile and my-recompile to get back to the bottom of a
;; compilation buffer after save-excursion brings us back to the place we
;; started.
;;

;;
;; If true we are in compile mode, 
;; in the future make this a toggle
;;
(defvar my-next-error-state nil)

(defvar my-compilation-buffer-name "*compilation*")
(defvar my-syntax-checking-buffer-name "*syntax-check*")

(defun my-end-of-current-compilation-buffer()
  (if (equal (buffer-name) my-compilation-buffer-name)
      (end-of-buffer)))

(defun my-get-compilation-buffer-name (&optional major)
  my-compilation-buffer-name)

(setq compilation-buffer-name-function 'my-get-compilation-buffer-name)

(funcall compilation-buffer-name-function)

(defun my-compile(&optional command)
  (interactive)
  (setq my-next-error-state nil)
  (let ((my-compilation-buffer-name "*real-compilation*"))
    (if (interactive-p)
        (call-interactively 'compile)
      (compile command))
    (save-excursion
      (pop-to-buffer my-compilation-buffer-name)
      (end-of-buffer))
    ;; force scrolling despite save-excursion
    (my-end-of-current-compilation-buffer)))

(defun my-syntax-check ()
  (interactive)
  (let ((my-compilation-buffer-name "*syntax-check*"))
    (my-compile "make")))

(defun my-buffer-exists (buffer)
  "Return t if the buffer exists.
buffer is either a buffer object or a buffer name"
  (bufferp (get-buffer buffer)))

(defun my-recompile ()
  "Run recompilation but put the point at the *end* of the buffer
so we can watch errors as they come up"
  (interactive)
  (setq my-next-error-state nil)
  (let ((my-compilation-buffer-name "*real-compilation*"))
    (if (and (my-buffer-exists (my-get-compilation-buffer-name))
             compile-command)
        (save-excursion
          ;; switching to the compilation buffer here causes the compile command to be
          ;; executed from the same directory it originated from.
          (pop-to-buffer (my-get-compilation-buffer-name))
          (recompile)
          (pop-to-buffer (my-get-compilation-buffer-name))
          (end-of-buffer))
      ;; else
      (call-interactively 'my-compile))
    ;; force scrolling despite save-excursion
    (my-end-of-current-compilation-buffer)))

(defun my-syntax-recheck(&optional command)
  (interactive)
  (let ((my-compilation-buffer-name "*syntax-check*"))
    (interactive)
    (setq my-next-error-state nil)
    (if (and (my-buffer-exists (my-get-compilation-buffer-name))
             compile-command)
        (save-excursion
          ;; switching to the compilation buffer here causes the compile command to be
          ;; executed from the same directory it originated from.
          (pop-to-buffer (my-get-compilation-buffer-name))
          (recompile)
          (pop-to-buffer (my-get-compilation-buffer-name))
          (end-of-buffer))
      ;; else
      (my-compile  "~/bin/compile.sh"))
    ;; force scrolling despite save-excursion
    (my-end-of-current-compilation-buffer)))

