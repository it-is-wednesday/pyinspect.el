;;; pyinspect.el --- Python object inspector -*- lexical-binding: t; -*-
;;
;; Author: Maor Kadosh <git@avocadosh.xyz>
;; URL: https://github.com/it-is-wednesday/pyinspect.el
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Keywords: tools
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Inspect objects in existing Python REPL.
;;
;; Once you have statrted a Python process with `run-python' and defined a variable, you can
;; inspect it by putting your cursor on it and running `pyinspect-inspect-at-point'.
;; You'll get into an inspection buffer dedicated to this variable. You can navigate in and out of
;; each field via `i' and `u', and move between same-level fields via `n' and `p'.
;; Once you're done, hit `q' to close all pyinspect buffers.
;;
;;; Code:

(require 'python)

(defvar pyinspect--primary-face '(:foreground "orange red"))

(defvar pyinspect--history '()
  "Stack of current inspection path.
List of currently inspected object's ancestor.")

(defvar pyinspect--python-boilerplate-file-path
  (concat
   ;; `load-file-name' will be nil when we manually evaluate this buffer
   (file-name-directory (or load-file-name buffer-file-name))
   "pyinspect.py"))

(defun pyinspect--var-exists-p (var)
  "Return t if VAR is defined in locals() of running Python process, nil otherwise."
  (equal "True"
         (string-trim
          (python-shell-send-string-no-output
           (format "'%s' in locals()" var)))))

(defun pyinspect--fix-json-bool (str)
  "If STR is t/`:json-false', return 'True'/'False' respectively."
  (pcase str
    (:json-false "False")
    ('t "True")
    (_ str)))

(defun pyinspect--make-key-callback (obj-name)
  "To be called when a field name of inspected object OBJ-NAME is clicked."
  (lambda (_btn)
    (push (buffer-name) pyinspect--history)
    (pyinspect--inspect obj-name nil)))

(defun pyinspect--inspect-in-current-buffer (obj-name)
  "Replace current buffer content with OBJ-NAME inspection, gathered from Python process."
  ;; Ensure we're in `pyinspect-mode'
  (if (not (eq major-mode 'pyinspect-mode))
      (user-error "This function should only be called in pyinspect-mode buffers"))

  ;; Extract some details regarding OBJ from running Python process
  (let* ((buffer-read-only nil)
         (json (json-read-from-string
                (python-shell-send-string-no-output
                 ;; _pyinspect_json is defined in pyinspect.py, loaded on pyinspect-mode entrance
                 (format "_pyinspect_json(%s)" obj-name))))
         (type (alist-get 'type json))
         (val (alist-get 'value json)))
    (erase-buffer)

    (pcase type
      ;; obj is str/bool/int/float/complex (complex is a numeric type)
      ;; Inspector will merely display its literal value
      ("primitive"
       (insert (format "%s" (pyinspect--fix-json-bool val))))

      ;; tuple/list
      ;; Display as if it's a dictionary, where indexes are the keys
      ;; `val' is a list of collection elements here.
      ("collection"
       (cl-loop for i from 0 to (- (length val) 1) do
                (insert-button (format "%s: " i)
                               'face pyinspect--primary-face
                               'action (pyinspect--make-key-callback
                                        (format "%s[%s]" obj-name i)))
                (insert (format "%s\n" (elt val i)))))

      ;; Display pairs of "key: value"
      ("dict"
       (let ((;; Fix booleans in all values of the JSON alist returned by `json-read-from-string'.
              ;; See `pyinspect--fix-json-bool'
              items (cl-loop for (k . v) in val
                             collect (list k (pyinspect--fix-json-bool v)))))
         (cl-loop for (k . (v)) in items do
                  ;; insert key
                  (insert-button (format "%s: " k)
                                 'face pyinspect--primary-face
                                 'action (pyinspect--make-key-callback
                                          (format "%s[%s]" obj-name k)))
                  ;; insert value
                  (insert (format "%s\n" v)))))

      ;; Everything that isn't one of the above. In this case will display "key: val" pairs
      ;; for each field (also called here member).
      ;; `val' is a list of object members here
      ("object"
       (cl-loop for (k . v) in val do
                (insert-button (symbol-name k)
                               'face pyinspect--primary-face
                               'action (pyinspect--make-key-callback
                                        (format "%s.%s" obj-name k)))
                (insert " = " (if (equal "" v) "\"\"" v) "\n"))))

    (goto-char (point-min))))

(defun pyinspect--inspect (obj-name pop)
  "Inspect OBJ-NAME in a new buffer.
If POP is non-nil, new buffer will be created with `pop-to-buffer'. Otherwise
replaces current buffer."
  (let ((buf-func (if pop #'pop-to-buffer #'generate-new-buffer))
        (buf-name (format "*Pyinspect: %s*" obj-name)))
    (funcall buf-func buf-name)
    (switch-to-buffer buf-name))
  (pyinspect-mode)
  (pyinspect--inspect-in-current-buffer obj-name))

;;;###autoload
(defun pyinspect-goto-parent-object ()
  "Inspect parent object of currently inspected object.
E.g. if we're inspecting `x.y.z', this function switches to buffer `x.y'.
If this objecet has no parent, quit all pyinspect buffers."
  (interactive)
  (let ((elem (pop pyinspect--history)))
    (if elem
        (switch-to-buffer elem))))

;;;###autoload
(defun pyinspect-inspect-at-point ()
  "Inspect symbol at point in `pyinspect-mode'."
  (interactive)
  (setq pyinspect--history '())
  (let ((var (symbol-at-point)))
    (if (pyinspect--var-exists-p var)
        (pyinspect--inspect (symbol-at-point) 'pop)
      (message "Variable %s doesn't exist!" var))))

;;;###autoload
(defun pyinspect-kill-all-buffers ()
  "Kill all pyinspect inspection buffers."
  (interactive)
  (kill-matching-buffers "*Pyinspect: " nil t)
  (previous-window-any-frame))

(defvar pyinspect-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<tab>") #'forward-button)
    (define-key keymap (kbd "<backtab>") #'backward-button)
    (define-key keymap "q" #'pyinspect-kill-all-buffers)

    ;; vim-like bindings
    (define-key keymap "l" #'push-button) ;; emulate RET
    (define-key keymap "h" #'pyinspect-goto-parent-object)
    (define-key keymap "j" #'next-line)
    (define-key keymap "k" #'previous-line)

    ;; normie bindings
    (define-key keymap "i" #'push-button) ;; emulate RET
    (define-key keymap "u" #'pyinspect-goto-parent-object)
    (define-key keymap "n" #'next-line)
    (define-key keymap "p" #'previous-line)

    keymap))

;;;###autoload
(define-derived-mode pyinspect-mode special-mode "Python Inspector"
  ;; Evaluate boilerplate file in current Python process.
  ;; I'm not using `python-shell-send-file' since it litters the process output
  ;; and prevents us from reading JSON output later on
  (python-shell-send-string-no-output
   (with-temp-buffer
     (insert-file-contents pyinspect--python-boilerplate-file-path)
     (buffer-string)))
  (set-syntax-table python-mode-syntax-table))

(provide 'pyinspect)
;;; pyinspect.el ends here
