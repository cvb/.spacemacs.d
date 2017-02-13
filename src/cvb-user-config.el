(defun cvb-user-init ()
  (require 'cvb-helm)
  (require 'cvb-org)

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

  (eval-after-load 'haskell-mode
    '(progn
       (advice-add 'intero-make-options-list
                   :around #'intero-with-better-print)

       (setq haskell-process-wrapper-function
             (lambda (argv) (append (list "nix-shell" "-I" "." "--command" )
                                    (list (mapconcat 'identity argv " ")))))

       (setq flycheck-command-wrapper-function
             (lambda (command)
               (apply 'nix-shell-command (nix-current-sandbox) command))
             flycheck-executable-find
             (lambda (cmd)
               (nix-executable-find (nix-current-sandbox) cmd)))))

  (add-hook 'haskell-mode-hook 'my-ghc-mod-setup)

  (advice-add 'executable-find  :around #'executable-find-try-nix))

(defun executable-find-try-nix (orig command)
  (let ((sb (nix-current-sandbox)))
    (if sb
        (let ((exec-path (nix-exec-path sb)))
          (and exec-path (funcall orig command)))
      (funcall orig command))))

(defun my-ghc-mod-setup ()
  (set (make-local-variable 'my-ghc-mod) (nix-executable-find (nix-current-sandbox) "ghc-mod"))
  (message "local ghc-mod is: %S" my-ghc-mod)
  (set (make-local-variable 'ghc-module-command) my-ghc-mod)
  (set (make-local-variable 'ghc-command) my-ghc-mod))

(defun intero-with-better-print (orig-fun &rest args)
  (let* ((newlst
          (append
           (list "--ghci-options" "-interactive-print=Text.Show.Unicode.uprint"
                 "--package" "unicode-show")
           (car args)))
         (res (apply orig-fun (append (list newlst) (cdr args)))))
    res))

(provide 'cvb-user-config)
