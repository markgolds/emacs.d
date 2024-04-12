(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun avy-action-kill-whole-line (pt)
  (goto-char pt)
  (kill-whole-line)
  (indent-for-tab-command nil)
  t)

(defun avy-action-end-of-line (pt)
  "Goto PT."
  (let ((frame (window-frame (selected-window))))
    (unless (equal frame (selected-frame))
      (select-frame-set-input-focus frame)
      (raise-frame frame))
    (goto-char pt)
    (move-end-of-line nil)
    ))

(defun avy-action-beginning-of-line (pt)
  "Goto PT."
  (let ((frame (window-frame (selected-window))))
    (unless (equal frame (selected-frame))
      (select-frame-set-input-focus frame)
      (raise-frame frame))
    (goto-char pt)
    (move-beginning-of-line nil)
    ))

(defun avy-action-zap-region (pt)
  "Kill from point up to PT."
  (if (> pt (point))      
      (progn
	(set-mark (point))
	(goto-char pt)
	(move-end-of-line nil)
	(kill-region nil nil t)  ;; use current region
	)
    (progn
	(set-mark (point))
	(goto-char pt)
	(move-beginning-of-line nil)
	(kill-region nil nil t)  ;; use current region
	)
    ))

(defun avy-action-comment-region (pt)
  "Comment from point up to PT."
  (if (> pt (point))      
      (progn
	(set-mark (point))
	(goto-char pt)
	(move-end-of-line nil)
	(comment-dwim-line nil)  ;; use current region
	)
    (progn
	(set-mark (point))
	(goto-char pt)
	(move-beginning-of-line nil)
	(comment-dwim-line nil)  ;; use current region
	)
    ))

(defun avy-action-comment-dupe-region (pt)
  "Comment from point up to PT."
  (if (> pt (point))      
      (progn
	(set-mark (point))
	(goto-char pt)
	(move-end-of-line nil)
	(my-comment-copy-yank-line-or-region)  ;; use current region
	)
    (progn
	(set-mark (point))
	(goto-char pt)
	(move-beginning-of-line nil)
	(my-comment-copy-yank-line-or-region)  ;; use current region
	)
    ))


(defun avy-action-mark-until (pt)
  "Mark sexp at PT."
  (set-mark (point))
  (goto-char pt)
  (move-end-of-line nil))

(use-package avy
  :bind (
	 ;("C-S-a" . 'avy-goto-char-timer)
	 ; ("C-S-r" . 'avy-copy-region))  ; this will also paste to pt
	 ("C-S-r" . 'avy-kill-ring-save-region))
  ;;("C-S-a" . 'avy-goto-char-2)
  :config
  ;; (setq avy-keys '(?q ?w ?a ?s ?k ?l ?o ?p))
  (setq avy-keys '(?q ?r ?u ?o ?p
                    ?s ?d ?f ?g ?h ?j
                    ?k ?l ?v ?b
                    ?n))
  (setq avy-styles-alist '((avy-goto-char-timer . pre)))
  (setq avy-timeout-seconds 0.3)
  (setq avy-background t)
  ;; (setq avy-dispatch-alist 
  ;; 	(assq-delete-all ?i avy-dispatch-alist)
  ;; 	)
  ;; (setq avy-dispatch-alist 
  ;; 	(assq-delete-all ?n avy-dispatch-alist)
  ;; 	)
  (setq avy-dispatch-alist
      (dolist (key '(?i ?n ?y ?w ?x ?X ?m ?t ?a ?e ?\s ?\C-w ?C ?z ?Y ?k ?Z ?\; ?') avy-dispatch-alist)
        (setq avy-dispatch-alist (assq-delete-all key avy-dispatch-alist))))
  (setf  ;; order is important here so dispatch help shows up nicely
   (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
   (alist-get ?' avy-dispatch-alist) 'avy-action-comment-dupe-region
   (alist-get ?Z avy-dispatch-alist) 'avy-action-zap-region
   (alist-get ?\C-w avy-dispatch-alist) 'avy-action-kill-whole-line
   (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
   (alist-get ?K avy-dispatch-alist) 'avy-action-kill-stay
   (alist-get ?e avy-dispatch-alist) 'avy-action-end-of-line
   (alist-get ?\s avy-dispatch-alist) 'avy-action-mark-until  ; space
   (alist-get ?w avy-dispatch-alist) 'avy-action-copy 
   (alist-get ?m avy-dispatch-alist) 'avy-action-mark
   (alist-get ?\; avy-dispatch-alist) 'avy-action-comment-region
   (alist-get ?z avy-dispatch-alist) 'avy-action-zap-to-char
   (alist-get ?C avy-dispatch-alist) 'avy-action-copy-whole-line
   (alist-get ?y avy-dispatch-alist) 'avy-action-yank 
   (alist-get ?k avy-dispatch-alist) 'avy-action-kill-move 
   (alist-get ?a avy-dispatch-alist) 'avy-action-beg-of-line
   )
  )


(defun avy-show-dispatch-help ()  
  (let* ((len (length "avy-action-"))
         (fw (frame-width))
         (raw-strings (mapcar
                   (lambda (x)
                     (format "%2s: %-19s"
                             (propertize
                              (char-to-string (car x))
                              'face 'aw-key-face)
                             (substring (symbol-name (cdr x)) len)))
                   avy-dispatch-alist))
         (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
         (strings-len (length raw-strings))
         (per-row (floor fw max-len))
         display-strings)
    (cl-loop for string in raw-strings
             for N from 1 to strings-len do
             (push (concat string " ") display-strings)
             (when (= (mod N per-row) 0) (push "\n" display-strings)))
    (message "%s" (apply #'concat (nreverse display-strings)))))


(provide 'myavy)
