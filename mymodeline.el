
(defcustom my-modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)


(setq mode-line-format nil)
(kill-local-variable 'mode-line-format)
(force-mode-line-update)
(setq mode-line-compact nil) ; Emacs 28
  ;; (setq mode-line-right-align-edge 'right-margin) ; Emacs 30
(setq-default mode-line-format
	      '("%e"
		my-modeline-kbd-macro
		my-modeline-buffer-status
		" "
		my-modeline-buffer-name
		"  "
		my-modeline-major-mode
		"  "
		my-modeline-eglot
		"  "
		my-modeline-flymake
		"  "
		my-modeline-vc-branch
		"     "
		my-modeline-god
		;; "        "
		(:eval (propertize " " 'display '((space :align-to (- right 25)))))
		my-modeline-misc-info

		))



(defvar-local my-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

;;;; Keyboard macro indicator

(defvar-local my-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'prot-modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

(defvar-local my-modeline-god
    '(:eval
      (when (and (mode-line-window-selected-p) god-local-mode)
        (propertize " GOD" 'face 'error)
	;; (propertize " GOD" 'face 'bold)
	;; (propertize "GOD" 'face '(:foreground "red" :weight bold))
	
	))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")


(defvar-local my-modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize " @ "
                    ;; 'face 'my-modeline-indicator-red-bg
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")



(defvar-local my-modeline-buffer-name
    '(:eval
      (format "%s" (propertize (buffer-name) 'face 'mode-line-buffer-id)))
  "Mode line construct to display the buffer name.")

;; (defvar-local my-modeline-buffer-name
;;   '(:eval
;;     (let ((buffer-file (buffer-file-name)))
;;       (if buffer-file
;;           (format "%s%s"
;;                   (propertize (file-name-directory buffer-file) 'face 'font-lock-comment-face)
;;                   (propertize (buffer-name) 'face 'warning))
;;         (propertize (buffer-name) 'face 'warning))))
;;   "Mode line construct to display the buffer name and file location.")

(defun my-modeline-major-mode-name ()
  (capitalize (symbol-name major-mode)))

(defvar-local my-modeline-major-mode
    '(:eval
      (format "%s" (propertize (my-modeline-major-mode-name) 'face 'italic)))
  "Mode line construct to display the major mode.")

(defvar-local my-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")



;;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun my-modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    (capitalize branch)))

;; NOTE 2023-07-27: This is a good idea, but it hardcodes Git, whereas
;; I want a generic VC method.  Granted, I only use Git but I still
;; want it to work as a VC extension.

;; (defun my-modeline-diffstat (file)
;;   "Return shortened Git diff numstat for FILE."
;;   (when-let* ((output (shell-command-to-string (format "git diff --numstat %s" file)))
;;               (stats (split-string output "[\s\t]" :omit-nulls "[\s\f\t\n\r\v]+"))
;;               (added (nth 0 stats))
;;               (deleted (nth 1 stats)))
;;     (cond
;;      ((and (equal added "0") (equal deleted "0"))
;;       "")
;;      ((and (not (equal added "0")) (equal deleted "0"))
;;       (propertize (format "+%s" added) 'face 'shadow))
;;      ((and (equal added "0") (not (equal deleted "0")))
;;       (propertize (format "-%s" deleted) 'face 'shadow))
;;      (t
;;       (propertize (format "+%s -%s" added deleted) 'face 'shadow)))))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar my-modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map)
  "Keymap to display on VC indicator.")

(defun my-modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun my-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (my-modeline--vc-help-echo file)
               'local-map my-modeline-vc-map)
   ;; " "
   ;; (my-modeline-diffstat file)
   ))

(defun my-modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (my-modeline-string-cut-end
   (my-modeline--vc-text file branch face)))

(defvar my-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun my-modeline--vc-get-face (key)
  "Get face from KEY in `my-modeline--vc-faces'."
   (alist-get key my-modeline--vc-faces 'up-to-date))

(defun my-modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (my-modeline--vc-get-face (vc-state file backend)))

(defvar-local my-modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  ;; ((vc-git-registered file))
                  (branch (my-modeline--vc-branch-name file backend))
                  (face (my-modeline--vc-face file backend)))
        (my-modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")


;;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

;; Based on `flymake--mode-line-counter'.
(defun my-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar my-modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
    map)
  "Keymap to display on Flymake indicator.")

(defmacro my-modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "my-modeline-flymake-%s" type)) ()
     (when-let ((count (my-modeline-flymake-counter
                        ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'shadow)
        (propertize count
                    'face ',(or face type)
                     'mouse-face 'mode-line-highlight
                     ;; FIXME 2023-07-03: Clicking on the text with
                     ;; this buffer and a single warning present, the
                     ;; diagnostics take up the entire frame.  Why?
                     'local-map my-modeline-flymake-map
                     'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

(my-modeline-flymake-type error "☣")
(my-modeline-flymake-type warning "!")
(my-modeline-flymake-type note "·" success)

(defvar-local my-modeline-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         ;; See the calls to the macro `my-modeline-flymake-type'
         '(:eval (my-modeline-flymake-error))
         '(:eval (my-modeline-flymake-warning))
         '(:eval (my-modeline-flymake-note)))))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")



;; eglot
(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local my-modeline-eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")


(put 'my-modeline-buffer-name 'risky-local-variable t)
(put 'my-modeline-major-mode 'risky-local-variable t)
(put 'my-modeline-eglot 'risky-local-variable t)
(put 'my-modeline-flymake 'risky-local-variable t)
(put 'my-modeline-vc-branch 'risky-local-variable t)
(put 'my-modeline-kbd-macro 'risky-local-variable t)
(put 'my-modeline-buffer-status 'risky-local-variable t)
(put 'my-modeline-misc-info 'risky-local-variable t)
(put 'my-modeline-god 'risky-local-variable t)

;;;; Common helper functions
(defun my-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (if (string-empty-p str)
      str
    (and (my-common-window-narrow-p)
         (> (length str) my-modeline-string-truncate-length)
         (not (one-window-p :no-minibuffer)))))

(defun my-modeline--truncate-p ()
  "Return non-nil if truncation should happen.
This is a more general and less stringent variant of
`my-modeline--string-truncate-p'."
  (and (my-common-window-narrow-p)
       (not (one-window-p :no-minibuffer))))

(defun my-modeline-string-cut-end (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the end of STR by counting from its start up to
`my-modeline-string-truncate-length'."
  (if (my-modeline--string-truncate-p str)
      (concat (substring str 0 my-modeline-string-truncate-length) "...")
    str))

(defun my-modeline-string-cut-beginning (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the beginning of STR by counting from its end up to
`my-modeline-string-truncate-length'."
  (if (my-modeline--string-truncate-p str)
      (concat "..." (substring str (- my-modeline-string-truncate-length)))
    str))

(defun my-modeline-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`my-modeline-string-truncate-length' both from its beginning
and end."
  (let ((half (floor my-modeline-string-truncate-length 2)))
    (if (my-modeline--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- half)))
      str)))

(defun my-modeline--first-char (str)
  "Return first character from STR."
  (substring str 0 1))

(defun my-modeline-string-abbreviate (str)
  "Abbreviate STR individual hyphen or underscore separated words.
Also see `my-modeline-string-abbreviate-but-last'."
  (if (my-modeline--string-truncate-p str)
      (mapconcat #'my-modeline--first-char (split-string str "[_-]") "-")
    str))

(defun my-modeline-string-abbreviate-but-last (str nthlast)
  "Abbreviate STR, keeping NTHLAST words intact.
Also see `my-modeline-string-abbreviate'."
  (if (my-modeline--string-truncate-p str)
      (let* ((all-strings (split-string str "[_-]"))
             (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
             (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
             (first-component (mapconcat #'my-modeline--first-char nbutlast-strings "-"))
             (last-component (mapconcat #'identity last-strings "-")))
        (if (string-empty-p first-component)
            last-component
          (concat first-component "-" last-component)))
    str))

(defun my-common-window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))


(provide 'mymodeline)
