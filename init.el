(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; (require 'package)
;; (setq package-enable-at-startup nil)
;; (add-to-list 'package-archives
;; 	     '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;  	     '("gnu" . "https://elpa.gnu.org/packages/"))
;; ;; (add-to-list 'package-archives
;; ;;	     '("melpa3" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/"))
;; ;;(add-to-list 'package-archives
;; ;; 	     '("melpa2" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))
;; (package-initialize)

;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;;
;; Use ctrl alt q to format the code
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This shit slows it right down when enabled!
(setq auto-save-default nil)

(setq inhibit-startup-message t)
            (tool-bar-mode -1)
            (setq gc-cons-threshold 100000000)
            (setq read-process-output-max (* 1024 1024)) ;; 1mb

            (fset 'yes-or-no-p 'y-or-n-p)
            (global-set-key (kbd "<f5>") 'revert-buffer)

;;start in full screen
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; mac key remap
;;(setq ns-command-modifier 'meta)
;;(setq mac-option-modifier 'control)
;;(setq ns-function-modifier 'control)
(setq ns-command-modifier 'control)

;; show trailing spaces
(setq-default show-trailing-whitespace nil)

;; set tabs to indent as white spaces and set default tab width to 4 white spaces
(setq-default indent-tabs-mode nil)
            (setq-default tab-width 4)
            ;(setq-default indent-line-function 'insert-tab)

;; setup: M-y saves the new yank to the clipboard.
(setq yank-pop-change-selection t)

(show-paren-mode 1)
            ;; (setq column-number-mode t)

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
            (global-set-key (kbd "C-`") 'other-frame)

            ;; enable line numbers for all programing modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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

        ;; Org mode settings
(setq org-startup-folded t)

;; gccemacs settings
(setq warning-minimum-level :error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package doom-themes
    :ensure t
    :config
      ;; Global settings (defaults)
      (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

      ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
      ;; may have their own settings.
      (load-theme 'doom-gruvbox t)
      (setq doom-gruvbox-dark-variant "hard")

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
  (setq doom-modeline-env-python-executable "python3")
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

  (use-package gruvbox-theme
  :ensure t
  :config
  ;; (load-theme 'gruvbox t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package evil
;;   :ensure t
;;   :init
;;   (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))

;;   (use-package evil-escape
;;   :ensure t
;;   :config
;;   (evil-escape-mode 1)
;;   (setq-default evil-escape-delay 0.2)
;;   (setq-default evil-escape-key-sequence "jk"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swiper, ivy and counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

    (use-package ivy-rich
    :ensure t
    :config
    (ivy-rich-mode 1)
    )

    (use-package all-the-icons-ivy-rich
    :ensure t
    :init (all-the-icons-ivy-rich-mode 1))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set-frame-font "Monaco 12") ;; this is a mac font that needds installed on linux:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rainbow delimiters
(use-package rainbow-delimiters
:ensure t
:config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'rjsx-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'js2-mode-hook 'rainbow-delimiters-mode))

;; Try
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

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  ; (setq beacon-color "#666600")
  )

;; Highlight indent guides
;; indentation lines
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package flycheck
    :ensure t
    :config
    ;; set to have global completion or on specific modes.
    (global-flycheck-mode)
    ;;    (add-hook 'c-mode-hook 'flycheck-mode)

    ;; use local eslint from node_modules before global
    ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
    (defun my/use-eslint-from-node-modules ()
      (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
    (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1)
  (with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))
  (global-company-mode t)
;;  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'lisp-mode 'company-mode)
  )


;;(use-package pos-tip
;;  :ensure t
;;  :hook (company-mode . pos-tip)
;;  :config)

(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode)
  :config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (use-package web-mode
      :ensure t
        :config
        (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
        (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
        (add-to-list 'auto-mode-alist '("\\.handlebars?\\'" . web-mode))
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
      (setq web-mode-css-indent-offset 2))
    (add-hook 'web-mode-hook  'my-web-mode-hook)
    (setq tab-width 2)
    (add-hook 'web-mode-hook  'emmet-mode)


    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prettier-js
(use-package prettier-js
:ensure t
:config
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
;;(add-hook 'web-mode-hook 'prettier-js-mode)
)

(use-package js2-mode
  :ensure t
  :config
  (setq js2-basic-offset 2)
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
(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

;; ;; Tide-mode
;;   (use-package tide
;;     :ensure t
;;     :after (typescript-mode company flycheck)
;;     :hook ((typescript-mode . tide-setup)
;;            (typescript-mode . tide-hl-identifier-mode)
;;            ;;(before-save . tide-format-before-save)
;;            )
;;   )

;;   (defun setup-tide-mode ()
;;     (interactive)
;;     (tide-setup)
;;     (flycheck-mode +1)
;;     ;; Set flycheck to only run when file is saved
;;   ;;  (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;     (eldoc-mode +1)
;;     (tide-hl-identifier-mode +1)
;;     ;; company is an optional dependency. You have to
;;     ;; install it separately via package-install
;;     ;; `M-x package-install [ret] company`
;;     (company-mode +1))

;;   ;; aligns annotation to the right hand side
;;   (setq company-tooltip-align-annotations t)

;;   ;; formats the buffer before saving
;;   ;;(add-hook 'before-save-hook 'tide-format-before-save)

;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)

;;   (add-hook 'js2-mode-hook #'setup-tide-mode)
;;   ;; configure javascript-tide checker to run after your default javascript checker
;;   ;(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

;;   (require 'web-mode)
;;   (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (when (string-equal "jsx" (file-name-extension buffer-file-name))
;;                 (setup-tide-mode))))
;;   ;; configure jsx-tide checker to run after your default jsx checker
;;   (flycheck-add-mode 'javascript-eslint 'web-mode)
;;   ;;(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)


;; (use-package yasnippet
;;   :ensure t
;;   :init
;;   (yas-global-mode 1))

;; (use-package yasnippet-snippets
;;   :ensure t)


;; Projectile

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-caching t) ;; Much better performance on large projects
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
;;  (counsel-projectile-on)
  (counsel-projectile-mode 1)
  )


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
;; (global-set-key (kbd "C-`") (kbd "M-x multi-term RET"))

(use-package magit
  :ensure t
  :init
  (progn
    (bind-key "C-x g" 'magit-status)))

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

;; (use-package undo-tree
;;   :ensure t
;;   :config
;;   (progn
;;     (global-undo-tree-mode)
;;     (setq undo-tree-visualizer-timestamps t)
;;     (setq undo-tree-visualizer-diff t)))


(use-package dashboard
 :ensure t
 :config
(setq dashboard-banner-logo-title "I'm Batman")
(setq dashboard-startup-banner 'logo)

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-footer nil)
 (dashboard-setup-startup-hook))

(use-package imenu-list
:ensure t
:config)

(use-package go-mode
:ensure t
:config
(add-hook 'go-mode-hook 'lsp-deferred))


(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode web-mode python-mode php-mode js2-mode typescript-mode go-mode) . lsp)
  :config
  ;; (setq lsp-diagnostic-package :none)
  (setq lsp-keymap-prefix "C-c l")
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  (setq lsp-headerline-breadcrumb-mode nil)
  (setq lsp-eslint-auto-fix-on-save t)
  )

  (setq lsp-keymap-prefix "C-c l")

;;  (setq lsp-enabled-clients '(jedi clangd))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; (setq lsp-ui-sideline-enable t)
  ;; (setq lsp-ui-sideline-show-hover nil)
  ;; (setq lsp-ui-doc-position 'bottom)
  ;; ;; lsp config stuff
  ;; (setq lsp-enable-links nil)
  ;; ;; (setq lsp-signature-render-documentation nil)
  (lsp-headerline-breadcrumb-mode nil)
;;  (lsp-ui-sideline-show-code-actions t)

  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-peek--show t)
  (lsp-ui-doc--display t)
  (lsp-signature-toggle-full-docs)
  (lsp-enable-file-watchers nil)
  ;; (setq lsp-completion-enable-additional-text-edit nil)
  ;; (setq web-mode-enable-current-element-highlight t)
  (lsp-ui-doc-show t)
  (lsp-ui-doc-show)
  ;;(lsp-ui-doc-position 'bottom)
 )


;;       (use-package dap-mode
;;         :ensure t
;;         :hook (lsp-mode . dap-mode)
;;         :config
;;         (dap-ui-mode 1)
;;         (dap-tooltip-mode 1)
;;         (require 'dap-node)
;;         ;;      (dap-node-setup)
;;         )

;;       (dap-auto-configure-mode)
;; ;;      (require 'dap-gdb-lldb)
;; ;;       (require 'dap-cpptools)
;; ;;      https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

;; eglot mode, try this out again soon!

;; (use-package eglot
;; :ensure t
;; :config
;; (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))
;;)


(use-package php-mode
:ensure t
:config)


(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(use-package yaml-mode
      :ensure t
      :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
      (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))


(use-package fzf
  :ensure t
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

;; vlf, used for opening large files apparently

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets yaml-mode which-key web-mode web-beautify use-package undo-tree try treemacs-projectile tide rjsx-mode rainbow-delimiters pug-mode prettier-js php-mode org-bullets neotree multi-term magit lsp-ui imenu-list highlight-indent-guides gruvbox-theme go-mode flycheck-pos-tip flycheck-plantuml flycheck-color-mode-line exec-path-from-shell evil-escape evil-collection emmet-mode doom-themes doom-modeline dashboard counsel-projectile company-quickhelp cmake-mode beacon all-the-icons-ivy-rich))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
