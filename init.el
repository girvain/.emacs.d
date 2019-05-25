;;; Package --- Summary
;;; Commentary:
;; Gavin Ross EMACS config
;; Disable cmake-ide when on mac, adjust fonts in font section accaordinly

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic behaviour and appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(add-hook 'prog-mode-hook 'linum-mode)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package zenburn-theme
;;   :ensure t
;;  :cosnfig (load-theme 'zenburn t))

;; (use-package monokai-theme
;;   :ensure t
;;   :config (load-theme 'monokai t))

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
    (doom-themes-neotree-config)
    ;; or for treemacs users
    (doom-themes-treemacs-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set-frame-font "Monaco 13") ;; this is a mac font that needds installed on linux:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package try
	:ensure t)

(use-package which-key
	:ensure t
	:config
	(which-key-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

; flashes the cursor's line when you scroll
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  ; (setq beacon-color "#666600")
  )

;; indentation lines
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))



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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; ;; ;; Color mode line for errors.
;; (use-package flycheck-color-mode-line
;;   :ensure t
;;   :after flycheck
;;   :config '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;;   )

;; ;; Show pos-tip popups for errors.
;; (use-package flycheck-pos-tip
;;   :ensure t
;;   :after flycheck
;;   :config (flycheck-pos-tip-mode)
;;   )

;; Flycheck-plantuml/
;; (use-package flycheck-plantuml
;;   :after flycheck
;;   :ensure t
;;   :config (flycheck-plantuml-setup)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; irony (C/C++ minor mode powered by libclang)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package irony
;;  :ensure t
;;  :config
;;  (add-hook 'c-mode-hook 'irony-mode)
;;  (add-hook 'c++-mode-hook 'irony-mode)
;;  (add-hook 'objc-mode-hook 'irony-mode)
;;  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;  (defun my-irony-mode-hook ()
;;  (define-key irony-mode-map [remap completion-at-point]
;;   'irony-completion-at-point-async)
;;  (define-key irony-mode-map [remap complete-symbol]
;;    'irony-completion-at-point-async))
;;  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;  )

;; ;; Eldoc shows argument list of the function you are currently writing in the echo area.
  ;; (use-package irony-eldoc
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (add-hook 'irony-mode-hook #'irony-eldoc)))

;; Flycheck irony
  ;; (use-package flycheck-irony
  ;;   :after flycheck
  ;;   :ensure t
  ;;   :config
  ;;   (progn
  ;;     (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

;;company-irony.
;; (use-package company-irony
;;   :after company
;;   :ensure t
;;   :config (global-company-mode)
;;   ;;(optional) adds CC special commands to `company-begin-commands' in order to
;;   ;;trigger completion at interesting places, such as after scope operator
;;   ;;    std::|
;;   (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;; )

;;Company-mode backend for C/C++ header files that works with irony-mode.
;;Complementary to company-irony by offering completion suggestions to header files.
;; (use-package company-irony-c-headers
;; :ensure t
;; :after company-irony
;; :ensure t
;; :config
;; ;;Load with `irony-mode` as a grouped backend
;; (eval-after-load 'company
;; '(add-to-list
;; 'company-backends '(company-irony-c-headers company-irony)))
;; )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company Mode (Code Completion package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode.
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format can be triggered using C-M-tab
(use-package clang-format
  :ensure t
  :config (global-set-key [C-M-tab] 'clang-format-region)
  )

;; If the repo does not have a .clang-format files, one can
;; be created using google style:
;; clang-format -style=google -dump-config > .clang-format
;; In this, default indent is 2 (see 'IndentWidth' key in generated file).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ mode modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'c-mode-common-hook 'google-set-c-style)

;; ;; use google style but modify offset to 4 (default for google is 2)
;; (c-add-style "my-style"
;; 	     '("google"
;; 	       (c-basic-offset . 4)            ; indent by four spaces
;; 	       ))

;; ;; also toggle on auto-newline and hungry delete minor modes
;; (defun my-c++-mode-hook ()
;;   (c-set-style "my-style")        ; use my-style defined above
;;   (auto-fill-mode))

;; (add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; ;; Autoindent using google style guide
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emmet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emmet-mode
  :ensure t
  :config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web beautify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-beautify
  :ensure t
  :config
  (eval-after-load 'js2-mode
    '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
  (eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))

(eval-after-load 'sgml-mode
  '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

  )


;; Better imenu
;;(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda ()
                             (flycheck-mode)
                             (setq js2-basic-offset 2)
                             (tern-mode)

                             ;; disable jshint since we prefer eslint checking
                             (setq-default flycheck-disabled-checkers
                                           (append flycheck-disabled-checkers
                                                   '(javascript-jshint)))

                             (company-mode)))

;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pug mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package pug-mode
  :ensure t
  :config
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ivy-mode (for completion in swiper)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; Ivy integration with rtags.
  ;;(setq rtags-display-result-backend 'ivy)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IBuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Swiper, Ivy and Counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAC KEY SWITCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq ns-command-modifier 'meta)
;; (setq mac-option-modifier 'control)
;; (setq ns-function-modifier 'control)
(setq ns-command-modifier 'control)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
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

;; Add this after adding projectile
(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package neotree
  :ensure t
  :config
  ;;(neotree-projectile-action )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
    :ensure t
    :init
    (progn
    (bind-key "C-x g" 'magit-status)
    ))

(setq magit-status-margin
  '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
    (use-package git-gutter
    :ensure t
    :init
    (global-git-gutter-mode +1))

    (global-set-key (kbd "M-g M-g") 'hydra-git-gutter/body)


    (use-package git-timemachine
    :ensure t
    )
  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                              :hint nil)
    "
  Git gutter:
    _j_: next hunk        _s_tage hunk     _q_uit
    _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
    ^ ^                   _p_opup hunk
    _h_: first hunk
    _l_: last hunk        set start _R_evision
  "
    ("j" git-gutter:next-hunk)
    ("k" git-gutter:previous-hunk)
    ("h" (progn (goto-char (point-min))
                (git-gutter:next-hunk 1)))
    ("l" (progn (goto-char (point-min))
                (git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("p" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (progn (git-gutter-mode -1)
                ;; git-gutter-fringe doesn't seem to
                ;; clear the markup right away
                (sit-for 0.1)
                (git-gutter:clear))
         :color blue))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(custom-safe-themes
   (quote
    ("cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(package-selected-packages
   (quote
    (git-timemachine git-gutter magit highlight-indent-guides multi-term emmet-mode web-beautify pug-mode lsp-ui company-lsp company-tern tern-auto-complete tern js2-refactor ac-js2 web-mode doom-themes cyberpunk-theme ivy-rtags company-irony-c-headers company-irony flycheck-irony irony flycheck-plantuml flycheck-pos-tip flycheck-color-mode-line company-rtags flycheck-rtags rtags counsel-projectile neotree evil-collection evil-escape evil cquery irony-eldoc yasnippet-snippets counsel evil-visual-mark-mode cmake-ide swiper which-key try use-package)))
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-I/Users/gavinross/c-demo-project/include1/" "-I/Users/gavinross/c-demo-project/include2/")
     (cmake-ide-build-dir . "~/cpp_primer/exercises/test_area/"))))
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
