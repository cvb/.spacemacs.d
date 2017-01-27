(require 'helm)

(defun dwim-helm-find-files-up-one-level-maybe ()
  (interactive)
  (if (looking-back "/" 1)
      (call-interactively 'helm-find-files-up-one-level)
    (delete-backward-char 1)))

(defun dwim-helm-find-files-navigate-forward (orig-fun &rest args)
  "Adjust how helm-execute-persistent actions behaves, depending on context"
  (if (file-directory-p (helm-get-selection))
      (apply orig-fun args)
    (helm-maybe-exit-minibuffer)))

(define-key helm-read-file-map
  (kbd "<backsqpace>") 'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-read-file-map
  (kbd "DEL") 'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-find-files-map
  (kbd "<backspace>") 'dwim-helm-find-files-up-one-level-maybe)
(define-key helm-find-files-map
  (kbd "DEL") 'dwim-helm-find-files-up-one-level-maybe)

(define-key helm-map
  (kbd "<return>") 'helm-maybe-exit-minibuffer)
(define-key helm-map
  (kbd "RET") 'helm-maybe-exit-minibuffer)
(define-key helm-find-files-map
  (kbd "<return>") 'helm-execute-persistent-action)
(define-key helm-read-file-map
  (kbd "<return>") 'helm-execute-persistent-action)
(define-key helm-find-files-map
  (kbd "RET") 'helm-execute-persistent-action)
(define-key helm-read-file-map
  (kbd "RET") 'helm-execute-persistent-action)

(advice-add 'helm-execute-persistent-action
            :around
            #'dwim-helm-find-files-navigate-forward)

(provide 'cvb-helm)
