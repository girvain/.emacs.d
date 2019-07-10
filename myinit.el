;; [[file:~/.emacs.d/myinit.org::*interface%20tweaks][interface tweaks:1]]
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)

;; mac key remap
;;(setq ns-command-modifier 'meta)
;;(setq mac-option-modifier 'control)
;;(setq ns-function-modifier 'control)
(setq ns-command-modifier 'control)

;; show trailing spaces
(setq-default show-trailing-whitespace t)

;; set tabs to indent as white spaces and set default tab width to 4 white spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;(setq-default indent-line-function 'insert-tab)

;; setup: M-y saves the new yank to the clipboard.
(setq yank-pop-change-selection t)

(show-paren-mode 1)
(setq column-number-mode t)

;; minimalistic Emacs at startup
(menu-bar-mode 1)
(tool-bar-mode 0)
(set-scroll-bar-mode nil)

;; don't use global line highlight mode
(global-hl-line-mode 0)

;; supress welcome screen
(setq inhibit-startup-message t)

;; Bind other-window (and custom prev-window) to more accessible keys.
(defun prev-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-'") 'other-window)
(global-set-key (kbd "C-;") 'prev-window)

;; enable line numbers for all programing modes
;;(add-hook 'prog-mode-hook 'linum-mode)

; Highlights the current cursor line
(global-hl-line-mode t)

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

;; compile key command
(global-set-key [(f7)] 'compile)
(global-set-key [(f8)] 'recompile)

;; backup in one place. flat, no tree structure
;; this stops emacs making ~file copies of everything
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; enable recent file mode on startup
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
;;(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; disable line wrapping
(set-default 'truncate-lines t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
;; interface tweaks:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Themes][Themes:1]]
(use-package doom-themes
  :ensure t
  :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    (load-theme 'doom-vibrant t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme (all-the-icons must be installed!)
;;    (doom-themes-neotree-config)
    ;; or for treemacs users
 ;;   (doom-themes-treemacs-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)
    )

(use-package all-the-icons
:ensure t
:config )
;; (use-package zenburn-theme
;;   :ensure t
;;  :cosnfig (load-theme 'zenburn t))

;; (use-package monokai-theme
;;   :ensure t
;;   :config (load-theme 'monokai t))
(use-package doom-modeline
:ensure t
:hook (after-init . doom-modeline-mode)
:config
;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 3)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are expereicing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; Whether display icons in mode-line or not.
(setq doom-modeline-icon t)

;; Whether display the icon for major mode. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display color icons for `major-mode'. It respects
;; `doom-modeline-icon' and `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display icons for buffer states. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display buffer modification icon. It respects `doom-modeline-icon'
;; and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether display minor modes in mode-line or not.
(setq doom-modeline-minor-modes nil)

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)

;; Whether display buffer encoding.
(setq doom-modeline-buffer-encoding t)

;; Whether display indentation information.
(setq doom-modeline-indent-info nil)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)

;; Whether display perspective name or not. Non-nil to display in mode-line.
(setq doom-modeline-persp-name t)

;; Whether display icon for persp name. Nil to display a # sign. It respects `doom-modeline-icon'
(setq doom-modeline-persp-name-icon nil)

;; Whether display `lsp' state or not. Non-nil to display in mode-line.
(setq doom-modeline-lsp t)

;; Whether display github notifications or not. Requires `ghub` package.
(setq doom-modeline-github nil)

;; The interval of checking github.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display environment version or not
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python")
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
(setq doom-modeline-mu4e t)

;; Whether display irc notifications or not. Requires `circe' package.
(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)

(doom-modeline-mode 1)
)
;; Themes:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Fonts][Fonts:1]]
;;(set-frame-font "Monaco 13") ;; this is a mac font that needds installed on linux:
;; Fonts:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Rainbow%20Delimiters][Rainbow Delimiters:1]]
(use-package rainbow-delimiters
:ensure t
:config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'rjsx-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'js2-mode-hook 'rainbow-delimiters-mode))
;; Rainbow Delimiters:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Try][Try:1]]
(use-package try
	:ensure t)
;; Try:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Which%20key][Which key:1]]
(use-package which-key
	:ensure t
	:config
	(which-key-mode))
;; Which key:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Org%20Bullets][Org Bullets:1]]
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; Org Bullets:1 ends here

;; [[file:~/.emacs.d/myinit.org::*beacon][beacon:1]]
; flashes the cursor's line when you scroll
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  ; (setq beacon-color "#666600")
  )
;; beacon:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Highlight%20indent%20guides][Highlight indent guides:1]]
;; indentation lines
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
;; Highlight indent guides:1 ends here

;; [[file:~/.emacs.d/myinit.org::*C++][C++:1]]
;; C++
(use-package c++-mode
  :after rtags
  :mode (("\\.h\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode))
  :bind (:map c++-mode-map
              ("<home>" . 'rtags-find-symbol-at-point)
              ("<prior>" . 'rtags-location-stack-back)
              ("<next>" . 'rtags-location-stack-forward))
  )

;; CMake
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :init (setq cmake-tab-width 4)
  )

;; (use-package lsp-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'lsp-language-id-configuration '(typescript-language-server . "javascript"))
;;  ;;  (lsp-register-client
;;  ;; (make-lsp-client :new-connection (lsp-stdio-connection "typescript-language-server")
;;  ;;                  :major-modes '(js2-mode)
;;   ;;                  :server-id 'typescript-language-server))
;;   (add-hook 'js2-mode-hook #'lsp)
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cquery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package cquery
;;   :ensure t
;;   :config
;;   ;; CHANGE THIS! when using linux or mac
;;   ;;linux
;;   ;;(setq cquery-executable "/home/gavin/cquery/build/release/bin/cquery")
;;   ;; mac
;;   (setq cquery-executable "/Users/gavinross/cquery/build/release/bin/cquery")
;;   (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))

;;   ;; Syntax Checker for cquery
;;   (setq cquery-sem-highlight-method 'font-lock)
;; ;; alternatively, (setq cquery-sem-highlight-method 'overlay)

;; ;; For rainbow semantic highlighting
;; ;; (cquery-use-default-rainbow-sem-highlight)
;;   ;; (setq cquery-executable "/path/to/cquery-install-prefix/bin/cquery")
;;   )

;; (defun cquery//enable ()
;;   (condition-case nil
;;       (lsp-cquery-enable)
;;     (user-error nil)))

;;   (use-package cquery
;;     :commands lsp-cquery-enable
;;     :init (add-hook 'c-mode-common-hook #'cquery//enable))
;; ;; Also see lsp-project-whitelist lsp-project-blacklist cquery-root-matchers

;; ;; Completion for cquery
;; (use-package company-lsp
;;   :ensure t
;;   :config
;; ;;  (push 'company-lsp company-backends)
;;   (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
;;   )


;; ;; lsp-ui
;; (use-package lsp-ui
;;   :ensure t
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package rtags
;;   :ensure t
;;   :config
;;   ;; Start the rdm process unless the process is already running.
;;   ;; --> Launch rdm externally and prior to Emacs instead.
;;     ;;(rtags-start-process-unless-running)
;;   ;;
;;   ;; Enable rtags-diagnostics.
;;   (setq rtags-autostart-diagnostics t)
;;   (rtags-diagnostics)
;;   ;;
;;   ;; Timeout for reparse on onsaved buffers.
;;   (rtags-set-periodic-reparse-timeout 0.5)
;;   ;;
;;   ;; Rtags standard keybindings ([M-. on symbol to go to bindings]).
;;   (rtags-enable-standard-keybindings)
;;   ;;
;;   ;; Enable completions in with rtags & company mode
;;   ;; -> use irony for completions
;;   ;;(setq rtags-completions-enabled t)
;;   ;;(require 'company)
;;   ;;(global-company-mode)
;;   ;;(push 'company-rtags company-backends) ; Add company-rtags to company-backends

  ;; ;; ivy integration
  ;; (setq rtags-display-result-backend 'ivy)
  ;; )

;; Flycheck rtags.
;; (use-package flycheck-rtags
;;   :after rtags
;;   :ensure t
;;   :config
;;   (defun my-flycheck-rtags-setup ()
;;     (flycheck-select-checker 'rtags)
;;     (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;     (setq-local flycheck-check-syntax-automatically nil))
;;   (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
;;   (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
;;   (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup))

;;Use rtags for auto-completion.
;; (use-package company-rtags
;;   :ensure t
;;   :config
;;   (progn
;;     (setq rtags-autostart-diagnostics t)
;;     (rtags-diagnostics)
;;     (setq rtags-completions-enabled t)
;;     (push 'company-rtags company-backends)
;;     ))

;; Ivy-Rtags
;; (use-package ivy-rtags
;;   :ensure t
;;   :config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake ide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package cmake-ide
;;   :after rtags
;;   :ensure t
;;   :config
;;   ;; set path to project build directory
;;  ;; (setq cmake-ide-build-dir
;;  ;;       (expand-file-name "~/src/stringent/build"))
;;   ;; CURRENTLY: hardcode to build dir of default project
;;   ;; TODO: fix via .dir-locals.el
;;   ;;
;;   ;; invoke cmake-ide setup
;;   (cmake-ide-setup)
;;   )
;; C++:1 ends here

;; [[file:~/.emacs.d/myinit.org::*flycheck][flycheck:1]]
(use-package flycheck
  :ensure t
  :config
  :init
  ;; set to have global completion or on specific modes.
  ;;(global-flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'js2-mode 'flycheck-mode)
  )

;; Color mode line for errors.
 (use-package flycheck-color-mode-line
   :ensure t
   :after flycheck
   :config '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
   )

;; Show pos-tip popups for errors.
 (use-package flycheck-pos-tip
   :ensure t
   :after flycheck
   :config (flycheck-pos-tip-mode)
   )

;; Flycheck-plantuml/
 (use-package flycheck-plantuml
   :after flycheck
   :ensure t
   :config (flycheck-plantuml-setup)
   )
;; flycheck:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Company][Company:1]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company Mode (Code Completion package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode.
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))
  ;;  (global-company-mode t)
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  )
;; Company:1 ends here

;; [[file:~/.emacs.d/myinit.org::*emmet][emmet:1]]
(use-package emmet-mode
  :ensure t
  :config)
;; emmet:1 ends here

;; [[file:~/.emacs.d/myinit.org::*web%20mode][web mode:1]]
(use-package web-mode
  :ensure t
    :config
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
;; 	(setq web-mode-engines-alist
;; 		  '(("django"    . "\\.html\\'")))
;; 	(setq web-mode-ac-sources-alist
;; 	      '(("css" . (ac-source-css-property))
;; 	        ("vue" . (ac-source-words-in-buffer ac-source-abbrev))
;;             ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
     (setq web-mode-enable-auto-closing t) ;)
     (setq web-mode-enable-auto-quoting t) ; this fixes the quote problem I mentioned

    (defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(setq tab-width 2)
(add-hook 'web-mode-hook  'emmet-mode)
)
;; web mode:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Web%20beautify%20(commented%20out)][Web beautify (commented out):1]]
;; (use-package web-beautify
;;   :ensure t
;;   :config
;;   (eval-after-load 'js2-mode
;;     '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
;;   (eval-after-load 'json-mode
;;   '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

;; (eval-after-load 'sgml-mode
;;   '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

;; (eval-after-load 'web-mode
;;   '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

;; (eval-after-load 'css-mode
;;   '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
;;   )
;; Web beautify (commented out):1 ends here

;; [[file:~/.emacs.d/myinit.org::*Prettier-js][Prettier-js:1]]
(use-package prettier-js
:ensure t
:config
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
)
;; Prettier-js:1 ends here

;; [[file:~/.emacs.d/myinit.org::*JavaScript][JavaScript:1]]
(use-package js2-mode
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  ;; turn on flychecking globally
  ;;(add-hook 'after-init-hook #'global-flycheck-mode)

  ;; turn off js2 syntax hilighting
    (setq js2-strict-missing-semi-warning nil)
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)

  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
;;  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; customize flycheck temp file prefix
  ;;(setq-default flycheck-temp-prefix ".flycheck")

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(json-jsonlist)))

    )


  ;; Better imenu
  ;;(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

  ;; (use-package company-tern
  ;;   :ensure t
  ;;   :config
  ;;   (add-to-list 'company-backends 'company-tern)
  ;;   (add-hook 'js2-mode-hook (lambda ()
  ;;                              (flycheck-mode)
  ;;                              (setq js2-basic-offset 2)
  ;;                              (tern-mode)

  ;;                              ;; disable jshint since we prefer eslint checking
  ;;                              (setq-default flycheck-disabled-checkers
  ;;                                            (append flycheck-disabled-checkers
  ;;                                                    '(javascript-jshint)))

  ;;                              (company-mode)))

  ;; ;; Disable completion keybindings, as we use xref-js2 instead
  ;; (define-key tern-mode-keymap (kbd "M-.") nil)
  ;; (define-key tern-mode-keymap (kbd "M-,") nil)
  ;; )

  ;; rjsx
    (use-package rjsx-mode
    :ensure t
    :config
    )
;; JavaScript:1 ends here

;; [[file:~/.emacs.d/myinit.org::*lsp-mode][lsp-mode:1]]
;;     ;;(setq exec-path (append exec-path '("~/.nvm/versions/node/v11.12.0/bin")))

;;     ;; Disable lsp-ui flymake default integration

;;     ;; https://github.com/emacs-lsp/lsp-ui/issues/226

;;     (setq lsp-prefer-flymake nil)


;;     (use-package typescript-mode :ensure t)

;;     (use-package lsp-ui 
;;     :ensure t
;;     :config
;; ;;    (lsp-ui-doc-mode)
;;     )

;;     (use-package js2-mode :ensure t)

;;     (use-package rjsx-mode :ensure t)



;;     ;; LSP requirements on the server

;;     ;; sudo npm i -g typescript-language-server; sudo npm i -g typescript

;;     ;; sudo npm i -g javascript-typescript-langserver

;;     (use-package lsp-mode
;;     :ensure t
;;     :config
;;     (require 'lsp-clients)
;;     (add-hook 'typescript-mode-hook 'lsp)
;;     (add-hook 'js2-mode-hook 'lsp)
;; ;;    (add-hook 'js2-mode-hook 'company-mode)
;;  ;;   (add-hook 'js2-mode-hook 'flycheck-mode)
;;     (add-hook 'js2-mode-hook 'lsp-ui-mode)

;;     (add-hook 'php-mode 'lsp)

;;     (add-hook 'css-mode 'lsp)

;;     (add-hook 'web-mode 'lsp))

;;     (use-package company-lsp
;;     :ensure t
;;     :config
;;     (push 'company-lsp company-backends)
;;     (setq
;;     ;company-lsp-enable-recompletion t
;;     ;company-lsp-enable-snippet t
;;     company-lsp-cache-candidates t
;;     company-lsp-async t
;;     ))


;;     (setq lsp-language-id-configuration '((java-mode . "java")

;;     (python-mode . "python")

;;     (gfm-view-mode . "markdown")

;;     (rust-mode . "rust")

;;     (css-mode . "css")

;;     (xml-mode . "xml")

;;     (c-mode . "c")

;;     (c++-mode . "cpp")

;;     (objc-mode . "objective-c")

;;     (web-mode . "html")

;;     (html-mode . "html")

;;     (sgml-mode . "html")

;;     (mhtml-mode . "html")

;;     (go-mode . "go")

;;     (haskell-mode . "haskell")

;;     (php-mode . "php")

;;     (json-mode . "json")

;;     (js2-mode . "javascript")

;;     (typescript-mode . "typescript")

;;     ))

;;     ;; LSP debugging

;;     ;;(setq lsp-print-io t)

;;     ;;(setq lsp-trace t)

;;     ;;(setq lsp-print-performance t)
;; lsp-mode:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Tide-mode][Tide-mode:1]]
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
;(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
;;(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
;; Tide-mode:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Pug%20mode][Pug mode:1]]
(use-package pug-mode
:ensure t
:config
)
;; Pug mode:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Ivy][Ivy:1]]
(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; Ivy integration with rtags.
  ;;(setq rtags-display-result-backend 'ivy)
  )
;; Ivy:1 ends here

;; [[file:~/.emacs.d/myinit.org::*IBuffer][IBuffer:1]]
(global-set-key (kbd "C-x C-b") 'ibuffer)
 (setq ibuffer-saved-filter-groups
	(quote (("default"
		 ("dired" (mode . dired-mode))
		 ("org" (name . "^.*org$"))
	       ("IRC" (or (mode . circe-channel-mode) (mode . circe-server-mode)))
		 ("web" (or (mode . web-mode) (mode . js2-mode)))
		 ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
		 ("mu4e" (or

                (mode . mu4e-compose-mode)
                (name . "\*mu4e\*")
                ))
		 ("programming" (or
				 (mode . python-mode)
				 (mode . c++-mode)))
		 ("emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")))
		 ))))
 (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-auto-mode 1)
	      (ibuffer-switch-to-saved-filter-groups "default")))

 ;; don't show these
					  ;(add-to-list 'ibuffer-never-show-predicates "zowie")
 ;; Don't show filter groups if there are no buffers in that group
 (setq ibuffer-show-empty-filter-groups nil)

 ;; Don't ask for confirmation to delete marked buffers
 (setq ibuffer-expert t)
;; IBuffer:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Swiper,%20Ivy%20and%20Counsel][Swiper, Ivy and Counsel:1]]
(use-package counsel
:ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line))
  :config
  (global-set-key "\C-x\ \C-r" 'counsel-recentf)
  )

  (use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

  (use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))
;; Swiper, Ivy and Counsel:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Yasnippet][Yasnippet:1]]
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)
;; Yasnippet:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Projectile][Projectile:1]]
(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
;;  (counsel-projectile-on)
  (counsel-projectile-mode 1)
  )
;; Projectile:1 ends here

;; [[file:~/.emacs.d/myinit.org::*multi-term][multi-term:1]]
(use-package multi-term
  :ensure t
  :config
;;  (setq multi-term-program "/usr/local/bin/zsh")

(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)))

(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

;; (defcustom term-unbind-key-list
;;   '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
;;   "The key list that will need to be unbind."
;;   :type 'list
;;   :group 'multi-term)

;; (defcustom term-bind-key-alist
;;   '(
;;     ("C-c C-c" . term-interrupt-subjob)
;;     ("C-p" . previous-line)
;;     ("C-n" . next-line)
;;     ("C-s" . isearch-forward)
;;     ("C-r" . isearch-backward)
;;     ("C-m" . term-send-raw)
;;     ("M-f" . term-send-forward-word)
;;     ("M-b" . term-send-backward-word)
;;     ("M-o" . term-send-backspace)
;;     ("M-p" . term-send-up)
;;     ("M-n" . term-send-down)
;;     ("M-M" . term-send-forward-kill-word)
;;     ("M-N" . term-send-backward-kill-word)
;;     ("M-r" . term-send-reverse-search-history)
;;     ("M-," . term-send-input)
;;     ("M-." . comint-dynamic-complete))
;;   "The key alist that will need to be bind.
;; If you do not like default setup, modify it, with (KEY . COMMAND) format."
;;   :type 'alist
;;   :group 'multi-term)

(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)) ))

;; Bind launch multi-term to C-`, the same as VSCode
(global-set-key (kbd "C-`") (kbd "M-x multi-term RET"))
;; multi-term:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Treemacs][Treemacs:1]]
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (treemacs--find-python3))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)
;; Treemacs:1 ends here

;; [[file:~/.emacs.d/myinit.org::*neotree][neotree:1]]
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (global-set-key [f8] 'neotree-toggle)
  ;;(neotree-projectile-action )
  )
;; neotree:1 ends here

;; [[file:~/.emacs.d/myinit.org::*Magit][Magit:1]]
(use-package magit
    :ensure t
    :init
    (progn
    (bind-key "C-x g" 'magit-status)
    ))

;; (setq magit-status-margin
;;   '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
;;     (use-package git-gutter
;;     :ensure t
;;     :init
;;     (global-git-gutter-mode +1))

;;     (global-set-key (kbd "M-g M-g") 'hydra-git-gutter/body)


;;     (use-package git-timemachine
;;     :ensure t
;;     )
;;   (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
;;                               :hint nil)
;;     "
;;   Git gutter:
;;     _j_: next hunk        _s_tage hunk     _q_uit
;;     _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
;;     ^ ^                   _p_opup hunk
;;     _h_: first hunk
;;     _l_: last hunk        set start _R_evision
;;   "
;;     ("j" git-gutter:next-hunk)
;;     ("k" git-gutter:previous-hunk)
;;     ("h" (progn (goto-char (point-min))
;;                 (git-gutter:next-hunk 1)))
;;     ("l" (progn (goto-char (point-min))
;;                 (git-gutter:previous-hunk 1)))
;;     ("s" git-gutter:stage-hunk)
;;     ("r" git-gutter:revert-hunk)
;;     ("p" git-gutter:popup-hunk)
;;     ("R" git-gutter:set-start-revision)
;;     ("q" nil :color blue)
;;     ("Q" (progn (git-gutter-mode -1)
;;                 ;; git-gutter-fringe doesn't seem to
;;                 ;; clear the markup right away
;;                 (sit-for 0.1)
;;                 (git-gutter:clear))
;;          :color blue))
;; Magit:1 ends here

;; [[file:~/.emacs.d/myinit.org::*undo-tree][undo-tree:1]]
(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))
;; undo-tree:1 ends here

;; [[file:~/.emacs.d/myinit.org::*dashboard][dashboard:1]]
(use-package dashboard
 :ensure t
 :config
(setq dashboard-banner-logo-title "I'm Batman")
(setq dashboard-startup-banner 'logo)

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-footer nil)
 (dashboard-setup-startup-hook))
;; dashboard:1 ends here

;; [[file:~/.emacs.d/myinit.org::*evil%20mode][evil mode:1]]
(use-package evil
  :ensure t
  :init
  ;;(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
)
(use-package evil-collection
  ;;:after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape
:ensure t
:config
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "jk"))
;; evil mode:1 ends here
