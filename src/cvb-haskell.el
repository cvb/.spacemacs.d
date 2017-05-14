(eval-after-load 'haskell-mode
  '(progn
     (add-hook 'haskell-mode-hook 'haskell-setup)
     (advice-add 'intero-make-options-list
                 :around #'intero-with-better-print)))

(defun intero-with-better-print (orig-fun &rest args)
  (let* ((newlst
          (append
           (list "--ghci-options" "-interactive-print=Text.Show.Unicode.uprint"
                 "--package" "unicode-show")
           (car args)))
         (res (apply orig-fun (append (list newlst) (cdr args)))))
    res))

(defun haskell-setup ()
  "Sets the current buffer to use Haskell Style. Meant to be
  added to `haskell-mode-hook'"
  (interactive)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4
        haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans")
        haskell-process-type 'cabal-repl
        haskell-process-wrapper-function
        (lambda (argv)
          (append
           (list "nix-shell" "-I" "." "--command")
           (list (mapconcat 'identity argv " "))))))

;; (setq dante-repl-command-line-methods-alist
;;       `((styx  . ,(lambda (root) (dante-repl-by-file root "styx.yaml" '("styx" "repl"))))
;;         (nix   . ,(lambda (root) (dante-repl-by-file root "shell.nix" '("nix-shell" "--run" "cabal repl" "shell.nix"))))
;;         (stack . ,(lambda (root) (dante-repl-by-file root "stack.yaml" '("stack" "repl"))))
;;         (bare  . ,(lambda (_) '("cabal" "repl")))))

;; (use-package dante
;;   :commands dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   (add-hook 'haskell-mode-hook 'flycheck-mode)
;;   :config
;;   (progn
;;     (dolist (mode haskell-modes)
;;       (spacemacs/set-leader-keys-for-major-mode mode
;;         "tt" 'dante-type-at
;;         "ti" 'dante-info
;;         "df" 'dante-auto-fix
;;         "tg" 'xref-find-definition
;;         "tu" 'xref-find-references
;;         "se" 'dante-eval-block
;;         "sR" 'dante-restart))
;;     (evil-define-key '(insert normal) dante-mode-map
;;       (kbd "M-.") 'xref-find-definition)))

(provide 'cvb-haskell)
