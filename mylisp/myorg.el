;; -*- lexical-binding: t; outline-regexp: ";;;" -*-

(setq org-agenda-start-on-weekday 1)
(setq calendar-week-start-day 1)
(setq org-agenda-span 'month)
(setq org-agenda-show-all-dates t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; turn off annoying confirmation requests
(setq org-confirm-babel-evaluate nil) 

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq holiday-bahai-holidays nil)
(setq holiday-islamic-holidays nil)
(setq calendar-hebrew-all-holidays-flag t)

;; show/don't show asterix in *bold* in org mode, etc...
(setq org-hide-emphasis-markers nil) 

;;;------------------------------Capture templates------------------------------

(setq org-capture-templates
      '(("n"               ; key
         "Note"            ; name
         entry             ; type
         (file+headline "~/Dropbox/org/notes.org" "Notes")  ; target
         "* %? %(org-set-tags)  :note: \n:PROPERTIES:\n:Created: %U\n:Linked: %A\n:END:\n%i"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties)
	
	("w"               ; key
         "Weights"         ; name
         table-line        ; type
         (file "~/Dropbox/org/habits/weights.org" )  ; target
	 "|%U|%^{weight}|%^{comment}|"
         :prepend t        ; properties
         :kill-buffer t    ; properties)
	
	("q"               ; key
	 "Quick note"      ; name
	 entry             ; type
	 (file "~/Dropbox/org/quicknotes.org")  ; target
	 "* %?\nEntered on %U\n" ;template
	 :prepend t        ; properties
	 :empty-lines 1    ; properties
	 :created t        ; properties)
	 
	("d"               ; key
	 "Todos"           ; name
	 entry             ; type
	 (file "~/Dropbox/org/todos.org")  ; target
	 "* %?\nEntered on %U\n" ;template
	 :prepend t        ; properties
	 :empty-lines 1    ; properties
	 :created t        ; properties)
	
	("t"               ; key
	 "Time Log"        ; name
	 entry             ; type
	 (file "~/Dropbox/org/timelog.org")  ; target
	 "* %?\nEntered on %U\n" ;template
	 :prepend t        ; properties
	 :empty-lines 1    ; properties
	 :created t        ; properties)
	 
	 ("j"              ; key
	  "Journal"        ; name
	  entry            ; type
	  (file+datetree "~/Dropbox/org/journal.org") ; target
	  "* \n%?\n"       ; template
	  :clock-in t)
        ))

;;;------------------------------Custom agenda------------------------------
(setq
 org-agenda-custom-commands
 '(
   ("d" "My Agenda"
    ((todo "TODO"
	   ((org-agenda-overriding-header
	     "TODOS\n------------------------------------------------------------"))
	   )
     (agenda ""
	     ((org-agenda-block-separator nil)
	      (org-agenda-span 90)
	      (org-deadline-warning-days 0)
	      (org-agenda-format-date "%-e-%A-%B-%Y")
	      (org-agenda-skip-deadline-if-done 1)
	      (org-agenda-skip-timestamp-if-done 1)
	      (org-agenda-skip-scheduled-if-done 1)
	      (org-agenda-overriding-header
	       "\nAGENDA\n------------------------------------------------------------")))
     ))))
;;;------------------------------End Custom agenda------------------------------


;; indentation for lists in org mode
(setq org-list-indent-offset 2)

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-agenda-files (list
			"~/Dropbox/org/quicknotes.org"
			"~/Dropbox/org/todos.org"
			"~/Dropbox/org/timelog.org"
			"~/Dropbox/org/birthdays.org"
			"~/Dropbox/org/Ped.org"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "READ" "STUDY" "PAPERS" "CODE" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff39a3" :weight bold))
	("READ" . (:foreground "#c4efcd" :weight bold))
	;;("STUDY" . (:foreground "white" :background "#4d4d4d" :weight bold))
	("STUDY" . (:foreground "white" :weight bold))
	("PAPERS" . (:foreground "#b3c6e5" :weight bold))
	("CODE" . (:foreground "#bc86e0" :weight bold))
	))

;; sort by todo state (for todo list) and then time (for daily logs)
(setq org-agenda-sorting-strategy 
      '((agenda todo-state-up time-up)))

;; Turn this off in org mode:
;;https://emacs.stackexchange.com/questions/73986/how-do-i-stop-org-babel-from-trying-to-edit-a-source-block-in-a-dedicated-buffer
;; electric-indent-mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (electric-indent-local-mode -1)))

(setq org-image-actual-width nil)

(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

;;;------------------------------Org babel------------------------------
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   ;; (julia . t)
   (python . t)
   ;;   (jupyter . t)
   ))

;; (defun org-edit-src-code nil)

(defun insert-my-jupyter-block ()
  "Inserts a custom code block into the Org mode buffer."
  (interactive)
  ;; (insert "#+BEGIN_SRC jupyter-python :session py :results scalar :display plain\n")
  (insert "#+BEGIN_SRC jupyter-python\n")
  (insert "\n")
  (insert "#+END_SRC\n")
  (forward-line -2)
  (move-end-of-line 1))

(defun insert-my-python-block ()
  "Inserts a custom code block into the Org mode buffer."
  (interactive)
  ;; (insert "#+BEGIN_SRC python :session py :results scalar :display plain\n")
  (insert "#+BEGIN_SRC python\n")
  (insert "\n")
  (insert "#+END_SRC\n")
  (forward-line -2)
  (move-end-of-line 1))

;; Create a new jupyter block with C-c j
(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c j") 'insert-my-jupyter-block)
  '(define-key org-mode-map (kbd "C-c p") 'insert-my-python-block)
  )

;; Make org headers smaller
(defun my-org-customize-font ()
  "Customize font size for Org mode lines starting with #+."
  (setq-local face-remapping-alist
              '((org-meta-line . (:height 0.6))
                (org-block-begin-line . (:height 0.6))
                (org-block-end-line . (:height 0.6)))))

(add-hook 'org-mode-hook 'my-org-customize-font)

(provide 'myorg)
