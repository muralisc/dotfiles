(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Download Evil
(straight-use-package 'dracula-theme)
(straight-use-package 'helm)
(straight-use-package 'evil)
(straight-use-package 'evil-escape)
(straight-use-package 'which-key)

;; Enable Evil
(require 'evil)
(evil-mode 1)
(evil-escape-mode 1)
(which-key-mode)

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
(setq default-frame-alist '((font . "Fira Mono-12")))
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
(setq evil-escape-delay 0.15)
;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
(setq org-agenda-custom-commands
      `(;; match those are not scheduled, are not DONE.
        ("iu" "unscheduled TOTO tasks" tags "-SCHEDULED={.+}/+TODO|+STARTED|+WAITING")
        ;; match those are not scheduled, are not DONE.
        ("iU" "Unscheduled tasks with no TODO" tags "-SCHEDULED={.+}-TODO={.+}")
        ))

(evil-set-leader 'normal " ")

;; https://github.com/jiaoshijie/emacs.d/blob/b144b78a65127bf8e22eb3881719766bcb917b12/init.el
(evil-define-key 'normal 'global
  ;; ---- * leader-??? * ---- ;;
  (kbd "<leader>fr") 'helm-recentf
  (kbd "<leader>q") 'kill-buffer
  (kbd "<leader>w") 'save-buffer
  (kbd "M-H") 'windmove-left
  (kbd "M-J") 'windmove-down
  (kbd "M-K") 'windmove-up
  (kbd "M-L") 'windmove-right

  )
