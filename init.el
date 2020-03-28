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



(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(custom-safe-themes
   '("34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "4e10cdf7d030fb41061cf57c74f6ddfc19db8d4af6c8e0723dc77f9922543a3d" "955426466aa729d7d32483d3b2408cf474a1332550ad364848d1dfe9eecc8a16" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default))
 '(fci-rule-color "#6272a4")
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(objed-cursor-color "#ff5555")
 '(org-agenda-files nil)
 '(package-selected-packages
   '(evil-escape evil-collection evil dashboard undo-tree neotree treemacs-magit treemacs-projectile treemacs-evil treemacs multi-term counsel-projectile projectile yasnippet-snippets yasnippet counsel ivy pug-mode tide rjsx-mode js2-mode prettier-js web-mode emmet-mode company flycheck-plantuml flycheck-pos-tip flycheck-color-mode-line flycheck cmake-mode highlight-indent-guides beacon org-bullets which-key try rainbow-delimiters doom-modeline all-the-icons doom-themes use-package))
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50fa7b")
    (cons 40 "#85fa80")
    (cons 60 "#bbf986")
    (cons 80 "#f1fa8c")
    (cons 100 "#f5e381")
    (cons 120 "#face76")
    (cons 140 "#ffb86c")
    (cons 160 "#ffa38a")
    (cons 180 "#ff8ea8")
    (cons 200 "#ff79c6")
    (cons 220 "#ff6da0")
    (cons 240 "#ff617a")
    (cons 260 "#ff5555")
    (cons 280 "#d45558")
    (cons 300 "#aa565a")
    (cons 320 "#80565d")
    (cons 340 "#6272a4")
    (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
