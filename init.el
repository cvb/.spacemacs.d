;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(git
     ruby
     ruby-on-rails
     yaml
     html
     python
     auto-completion
     (haskell :variables haskell-completion-backend 'intero)
     scala
     javascript
     markdown
     clojure
     docker
     org
     regexp
     shell
     themes-megapack
     emacs-lisp
     elixir
     erlang
     custom
     nixos
     ansible
     ipython-notebook
     autohotkey
     purescript
     ;; mu4e
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages
   '(rainbow-delimiters org-bullets)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-startup-lists '(recents projects)

   dotspacemacs-additional-packages '()
   ;; Specify the startup banner. If the value is an integer then the
   ;; text banner with the corresponding index is used, if the value is
   ;; `random' then the banner is chosen randomly among the available banners,
   ;; if the value is a string then it must be a path to a .PNG file,
   ;; if the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'random
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(tao-yin
                         planet
                         tronesque
                         molokai
                         zenburn
                         solarized-dark
                         solarized-light
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   ;; dotspacemacs-default-font '("PragmataPro"
   ;; :size 14)
   ;; :weight normal
   ;; :width normal
   ;; :powerline-scale 1.1)
   ;; "-xos4-Terminus-bold-normal-normal-*-16-*-*-*-c-80-iso10646-1"
   dotspacemacs-default-font '("Ubuntu Mono"
                               :size 26
                               :weight normal
                               :width normal
                               :powerline-scale 0.3)

   ;; The leader key
   dotspacemacs-leader-key "SPC"

   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   dotspacemacs-version-check-enable nil
   ;; User initialization goes here
   ruby-enable-ruby-on-rails-support t
   ruby-version-manager 'rbenv
   fci-rule-color "#475662"
   ;;cider-pprint-fn "fipp"
   cider-repl-use-pretty-printing t
   ))

(defun dotspacemacs/user-init ()
  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (let ((default-directory  "~/.spacemacs.d/src"))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))
  (require 'cvb-user-config)
  (cvb-user-init))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 4 t)
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-term-color-vector
   [unspecified "#081724" "#ff694d" "#68f6cb" "#fffe4e" "#bad6e2" "#afc0fd" "#d2f1ff" "#d3f9ee"] t)
 '(ccm-ignored-commands
   (quote
    (mouse-drag-region mouse-set-point widget-button-click scroll-bar-toolkit-scroll evil-mouse-drag-region)))
 '(ccm-recenter-at-end-of-file t)
 '(cider-cljs-lein-repl
   "(do (require 'weasel.repl.websocket) (cemerick.piggieback/cljs-repl (weasel.repl.websocket/repl-env :ip \"127.0.0.1\" :port 9001)))")
 '(clojure-defun-style-default-indent t)
 '(clojure-indent-style :always-indent)
 '(enh-ruby-bounce-deep-indent t)
 '(enh-ruby-deep-arglist t)
 '(fci-rule-color "#475662")
 '(inf-ruby-implementations
   (quote
    (("ruby" . "irb --prompt default --noreadline -r irb/completion")
     ("jruby" . "jruby -S irb --prompt default --noreadline -r irb/completion")
     ("rubinius" . "rbx -r irb/completion")
     ("yarv" . "irb1.9 -r irb/completion")
     ("macruby" . "macirb -r irb/completion")
     ("pry" . "pry"))))
 '(ispell-program-name "hunspell")
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(magit-use-overlays nil)
 '(org-cycle-level-faces nil)
 '(org-export-backends (quote (ascii html icalendar latex deck)))
 '(org-pomodoro-format "~%s")
 '(org-pomodoro-keep-killed-pomodoro-time t)
 '(org-pomodoro-length 40)
 '(org-pomodoro-long-break-format "~%s")
 '(org-pomodoro-short-break-format "~%s")
 '(package-selected-packages
   (quote
    (log4e gntp simple-httpd parent-mode gitignore-mode flx evil goto-chg json-snatcher json-reformat diminish nixos-options ghc peg packed pythonic pkg-info epl avy popup package-build autothemer alert seq bind-key bind-map request skewer-mode company psci purescript-mode psc-ide ahk-mode helm-company helm-c-yasnippet company-web web-completion-data company-tern dash-functional tern company-statistics company-nixos-options company-cabal company-anaconda clojure-snippets auto-yasnippet ac-ispell auto-complete ein websocket pcache ob-elixir org minitest hide-comnt anzu highlight flycheck async inf-ruby dash dockerfile-mode docker tablist docker-tramp zenburn-theme yaml-mode ws-butler which-key web-mode toc-org spacemacs-theme spaceline sass-mode ruby-test-mode robe projectile-rails pip-requirements persp-mode paradox organic-green-theme org-plus-contrib open-junk-file omtose-phellack-theme neotree move-text monokai-theme moe-theme material-theme macrostep less-css-mode js2-refactor indent-guide hl-todo hindent help-fns+ helm-themes helm-pydoc helm-projectile helm-descbinds helm-ag haskell-snippets gruvbox-theme grandshell-theme google-translate evil-surround evil-search-highlight-persist evil-mc evil-matchit evil-iedit-state iedit eshell-prompt-extras erlang ensime sbt-mode scala-mode emmet-mode darktooth-theme cyberpunk-theme color-theme-sanityinc-tomorrow clj-refactor hydra yasnippet cider-eval-sexp-fu cider clojure-mode badwolf-theme anaconda-mode ample-theme alchemist ace-window ace-link ace-jump-helm-line smartparens undo-tree elixir-mode helm helm-core haskell-mode markdown-mode projectile magit magit-popup git-commit with-editor f js2-mode s visual-regexp tao-theme zonokai-theme zen-and-art-theme yapfify xterm-color window-numbering web-beautify volatile-highlights visual-regexp-steroids vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tangotango-theme tango-plus-theme tango-2-theme tagedit sunny-day-theme sublime-themes subatomic256-theme subatomic-theme stekene-theme spinner spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle slim-mode shell-pop seti-theme scss-mode rvm ruby-tools rubocop rspec-mode reverse-theme restart-emacs rbenv rake railscasts-theme queue quelpa pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme powerline popwin planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pcre2el pastels-on-dark-theme paredit orgit org-projectile org-present org-pomodoro org-download oldlace-theme occidental-theme obsidian-theme noflet noctilux-theme nix-mode niflheim-theme naquadah-theme mustang-theme multiple-cursors multi-term monochrome-theme molokai-theme mmm-mode minimal-theme markdown-toc majapahit-theme magit-gitflow lush-theme lorem-ipsum livid-mode live-py-mode linum-relative link-hint light-soap-theme keyfreq json-mode js-doc jinja2-mode jbeans-theme jazz-theme ir-black-theme intero inkpot-theme info+ inflections ido-vertical-mode hy-mode hungry-delete htmlize hlint-refactor highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-swoop helm-nixos-options helm-mode-manager helm-make helm-hoogle helm-gitignore helm-flx helm-css-scss hc-zenburn-theme haml-mode gruber-darker-theme gotham-theme golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme flycheck-mix flx-ido flatui-theme flatland-theme firebelly-theme fill-column-indicator feature-mode farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-numbers evil-nerd-commenter evil-magit evil-lisp-state evil-indent-plus evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z esh-help elisp-slime-nav edn dumb-jump dracula-theme django-theme define-word darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode company-ghci company-ghc column-enforce-mode colorsarenice-theme color-theme-sanityinc-solarized coffee-mode cmm-mode clues-theme clean-aindent-mode chruby cherry-blossom-theme busybee-theme bundler bubbleberry-theme birds-of-paradise-plus-theme auto-highlight-symbol auto-compile apropospriate-theme anti-zenburn-theme ansible-doc ansible ample-zen-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap)))
 '(powerline-default-separator nil)
 '(powerline-height 20)
 '(powerline-text-scale-factor 0.7)
 '(python-shell-interpreter "docker-compose run grab python" t)
 '(ring-bell-function (quote ignore))
 '(rspec-use-rake-when-possible t)
 '(sp-show-pair-from-inside t)
 '(standard-indent 2)
 '(wakatime-api-key "f3f4f28d-ae2d-48c9-a09e-3cb2d00f47e3")
 '(wakatime-cli-path "/Users/cvb/.pyenv/shims/wakatime")
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
'(whitespace-style
(quote
 (face tabs trailing space-before-tab space-after-tab tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:cell-input-area ((t (:background "#202020"))))
 '(org-done ((t (:foreground "forest green" :weight bold))))
 '(org-meta-line ((t (:foreground "#9D9D9D" :height 1.0))))
 '(org-pomodoro-mode-line ((t nil)))
 '(org-pomodoro-mode-line-break ((t (:foreground "pale green"))))
 '(org-todo ((t (:foreground "#D9D9D9" :weight bold)))))