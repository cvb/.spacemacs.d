(defun cvb-user-init ()
  (require 'cvb-helm)
  (require 'cvb-org)
  (require 'cvb-haskell)

  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)

  (setq x-select-enable-clipboard t)
  (setq x-select-enable-primary t)

  (setq powerline-default-separator nil)
  (global-set-key (kbd "M-`") 'other-window)
  (global-set-key (kbd "M-SPC") 'toggle-input-method)
  (setq fci-rule-width 1)
  (setq fci-rule-use-dashes t)
  (setq fci-dash-pattern 0.3)
  (setq fci-rule-color "#475662")
  (add-hook 'prog-mode-hook 'turn-on-fci-mode)

  (setenv "BOOT_JVM_OPTIONS" "-XX:-OmitStackTraceInFastThrow")
  (global-visual-line-mode)
  (setq whitespace-global-modes '(not org-mode))
  (global-whitespace-mode t)
  (evil-leader/set-key "cs" (lambda () (interactive) (cycle-spacing -1 t)))

  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  (with-eval-after-load 'clojure
    (define-clojure-indent
      (alet 'defun)
      (mlet 'defun)
      (match 'defun)))

  (with-eval-after-load 'erlang
    (setq erlang-indent-level 2))
  (global-linum-mode)
  (global-hl-line-mode -1)

  (set-mouse-color "white")

  ;; Make v$ exclude the carriage return
  (evil-define-motion evil-last-non-blank (count)
    :type inclusive
    (evil-end-of-line count)
    (re-search-backward "^\\|[^[:space:]]")
    (setq evil-this-type (if (eolp) 'exclusive 'inclusive)))
  (define-key evil-visual-state-map "$" 'evil-last-non-blank)

  (global-hl-line-mode -1)

  (setq scroll-conservatively 10
        scroll-margin 10
        scroll-preserve-screen-position 't)

  (advice-add 'executable-find  :around #'executable-find-try-nix)

  (electric-indent-mode -1)
  (add-to-list 'spacemacs-indent-sensitive-modes 'elixir-mode)

  (add-hook 'elixir-mode-hook
            (lambda ()
              (setq-local tab-always-indent 't)
              ;; (setq-local indent-line-function #'indent-relative-line)
              ))

  ;; (setq dante-debug (list 'inputs 'outputs 'responses 'command-line))
  )

(defun indent-relative-line ()
  (interactive)
  (cond ((eq (line-beginning-position) (point))
         (indent-relative))
        ((current-line-empty-p)
         (indent-relative))
        (t
         (save-excursion
           (back-to-indentation)
           (indent-relative)))))

(defun current-line-empty-p ()
  (string-match-p "^\\s-*$" (thing-at-point 'line)))

(defun executable-find-try-nix (orig command)
  (let ((sb (nix-current-sandbox)))
    (if sb
        (let ((exec-path (nix-exec-path sb)))
          (and exec-path (funcall orig command)))
      (funcall orig command))))

(provide 'cvb-user-config)
