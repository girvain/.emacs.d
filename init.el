(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;; 	     '("melpa2" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))
;; (add-to-list 'package-archives
;;	     '("melpa3" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))



(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
<<<<<<< HEAD
    (undo-tree evil-escape evil-collection evil rainbow-demimiters rainbow-delimiters neotree treemacs-projectile treemacs multi-term counsel-projectile projectile pug-mode yasnippet-snippets yasnippet emmet-mode which-key web-mode web-beautify use-package try org-bullets magit js2-mode highlight-indent-guides git-timemachine git-gutter flycheck doom-themes counsel company-tern cmake-mode beacon))))
=======
    (flycheck-plantuml flycheck-pos-tip flycheck-color-mode-line rjsx-mode evil-escape evil-collection evil rainbow-demimiters rainbow-delimiters neotree treemacs-projectile treemacs multi-term counsel-projectile projectile pug-mode yasnippet-snippets yasnippet emmet-mode which-key web-mode web-beautify use-package try org-bullets magit js2-mode highlight-indent-guides git-timemachine git-gutter flycheck doom-themes counsel company-tern cmake-mode beacon))))
>>>>>>> 717f92d4baab52721195ee35a3b0ead6bdf93d50
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
