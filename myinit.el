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
;; Treemacs:1 ends here

;; [[file:~/.emacs.d/myinit.org::*neotree][neotree:1]]
(use-package neotree
  :ensure t
  :config
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
