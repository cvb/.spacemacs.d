(defun pinentry-emacs (desc prompt ok error)
  "This allows to enter password for encrypted files in minibuffer"
  (let ((str (read-passwd
              (concat
               (replace-regexp-in-string
                "%22"
                "\""
                (replace-regexp-in-string "%0A" "\n" desc))
               prompt ": "))))
    str))

(pinentry-start)

(defun pom-hook (type)
  (let ((state org-pomodoro-state)
        (count org-pomodoro-countdown)
        (head  org-clock-heading))
    (message (format "Pomodoro %s, state: %s, countdown: %s" type state count))))

(with-eval-after-load 'org
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0))
  (custom-set-faces
   '(org-todo ((t (:foreground "#D9D9D9"
                               :weight bold
                               :background nil))))
   '(org-done ((t (:foreground "forest green"
                               :background nil
                               :weight bold)))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (latex . t)
     (clojure . t)
     (ditaa . t)
     (dot . t)
     (haskell . t)
     (gnuplot . t)
     (python . t)
     (ruby . t)))
  ;; (setq
  ;;  org-pomodoro-started-hook (lambda () (pom-hook "started"))
  ;;  org-pomodoro-finished-hook (lambda () (pom-hook "finished"))
  ;;  org-pomodoro-killed-hook (lambda () (pom-hook "killed")))
  )

(provide 'cvb-org)
