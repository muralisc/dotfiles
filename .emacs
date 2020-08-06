;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; Download Evil
(unless (package-installed-p 'dracula-theme)
  (package-install 'dracula-theme))
(unless (package-installed-p 'helm)
  (package-install 'helm))
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/shared_folders/minimal/Pensieve/textfiles/journal/WorkNote.org" "~/shared_folders/minimal/Pensieve/textfiles/journal/Done.org" "~/shared_folders/minimal/Pensieve/textfiles/journal/Today.org" "~/shared_folders/minimal/Pensieve/textfiles/journal/MurNote.org")))
 '(package-selected-packages (quote (evil helm dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; Personal Preferences
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key "\C-x\ \C-r" 'helm-recentf)
(global-set-key (kbd "M-x") #'helm-M-x)
(load-theme 'dracula t)
(global-linum-mode 1)
(show-paren-mode 1)
(setq vc-follow-symlinks t)  ;; Follow symlinks
(global-hl-line-mode +1)
(setq default-frame-alist '((font . "Fira Mono-14")))
;; Window Manip
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
;; Org Mode
;; From org help site:
(global-set-key "\C-ca" 'org-agenda)
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))
;; Common Settings with other frameworks like Spacemacs
(setq org-agenda-show-future-repeats nil)
(setq org-agenda-skip-scheduled-if-done t)
(setq-default evil-escape-key-sequence "jj")
(setq evil-escape-delay 0.4)
;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
(setq org-agenda-custom-commands
      `(;; match those are not scheduled, are not DONE.
        ("iu" "unscheduled TOTO tasks" tags "-SCHEDULED={.+}/+TODO|+STARTED|+WAITING")
        ;; match those are not scheduled, are not DONE.
        ("iU" "Unscheduled tasks with no TODO" tags "-SCHEDULED={.+}-TODO={.+}")
        ))
