(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-compile
	:config	(auto-compile-on-save-mode) ;; reduce risk of loading outdated bytecode
	(setq load-prefer-newer t))

(load-file "~/.emacs.d/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(defun reload ()
		"shorcut to reload init file"
		(interactive)
		(load-file "~/.emacs.d/init.el"))

	(defun cedit ()
		"shortcut to edit config.org file"
		(interactive)
		(find-file "~/.emacs.d/config.org"))

	(defun tedit ()
		"shortcut to edit org/tktodos.org"
		(interactive)
		(find-file "~/org/tktodos.org"))

	(defun pedit ()
		"shortcut to edit .profile"
		(interactive)
		(find-file "~/.zshrc"))

 (defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d")
                   ((equal prefix '(4)) "%d-%m-%Y")
                   ((equal prefix '(16)) "%A, %d. %B %Y")))
          (system-time-locale "us_US"))
      (insert (format-time-string format))))
(global-set-key (kbd "C-c d") 'insert-date)

(defun describe-in-popup (fn)
  (let* ((thing (symbol-at-point))
         (description (save-window-excursion
                        (funcall fn thing) ;; This is the yield point
                        (switch-to-buffer "*Help*")
                        (buffer-string))))
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))
(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point)))
    (cond
     ((fboundp thing) (describe-in-popup 'describe-function))
     ((boundp thing) (describe-in-popup 'describe-variable)))))

(global-set-key (kbd "C-h C-h") 'describe-thing-in-popup)

(defun hrs/rename-file (new-name)
  (interactive "FNew name: ")
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (when (buffer-modified-p)
             (save-buffer))
          (rename-file filename new-name t)
          (kill-buffer (current-buffer))
          (find-file new-name)
          (message "Renamed '%s' -> '%s'" filename new-name))
      (message "Buffer '%s' isn't backed by a file!" (buffer-name)))))

(defun hrs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun hrs/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun hrs/add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun hrs/find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun hrs/region-or-word ()
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word)))

(defun hrs/append-to-path (path)
	"Add a path both to the $PATH variable and to Emacs' exec-path."
	(setenv "PATH" (concat (getenv "PATH") ":" path))
	(add-to-list 'exec-path path))

(defun hrs/insert-password ()
	(interactive)
	(shell-command "pwgen 30 -1" t))

(defun hrs/notify-send (title message)
	"Display a desktop notification by shelling out to `notify-send'."
	(call-process-shell-command
	 (format "notify-send -t 2000 \"%s\" \"%s\"" title message)))

(use-package exec-path-from-shell
:init (exec-path-from-shell-initialize)
:config (when (memq window-system '(mac ns x)) ; sets MANPATH, PATH, exec-path-from-shell in osX/linux
(exec-path-from-shell-initialize)))

(fset 'tk-org-insert-lisp-block
   "#+begin_src emacs-lisp\C-m\C-m#+end_src\C-p")
(global-set-key (kbd "<f2>") 'tk-org-insert-lisp-block)

(fset 'tk-org-insert-rust-block
   "#+begin_src rust\C-m\C-m#+end_src\C-p")
(global-set-key (kbd "<f3>") 'tk-org-insert-rust-block)

(setq make-backup-files nil) ; none of these~
(setq auto-save-default t)

(use-package solarized-theme
 :config (load-theme 'solarized-gruvbox-dark t))
;; make src block code look nice
(add-hook 'text-mode-hook
					 (lambda ()
						(variable-pitch-mode 1)))

(use-package zoom
	:config (zoom-mode t))

(use-package centaur-tabs
	 :demand ;; don't defer load, recommended
 :config
	(setq centaur-tabs-style "bar"
	 centaur-tabs-height 32
	 centaur-tabs-set-icons t
	 centaur-tabs-set-modified-marker t
	 centaur-tabs-show-navigation-buttons t
	 centaur-tabs-set-bar 'left)
	(centaur-tabs-headline-match)
	;; (centaur-tabs-enable-buffer-reordering)
	;; (setq centaur-tabs-adjust-buffer-order t)
	(centaur-tabs-mode t)
(setq uniquify-separator "/")
	(setq uniquify-buffer-name-style 'forward)
	(defun centaur-tabs-buffer-groups () ;; straight from source
		"`centaur-tabs-buffer-groups' control buffers' group rules.
Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
		(list
		 (cond
 ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
 ;; "Remote")
 ((or (string-equal "*" (substring (buffer-name) 0 1))
			(memq major-mode '(magit-process-mode
			 magit-status-mode
			 magit-diff-mode
			 magit-log-mode
			 magit-file-mode
			 magit-blob-mode
			 magit-blame-mode
			 )))
	"Emacs")
 ((derived-mode-p 'prog-mode)
	"Editing")
 ((derived-mode-p 'dired-mode)
	"Dired")
 ((memq major-mode '(helpful-mode
				 help-mode))
	"Help")
 ((memq major-mode '(org-mode
				 org-agenda-clockreport-mode
				 org-src-mode
				 org-agenda-mode
				 org-beamer-mode
				 org-indent-mode
				 org-bullets-mode
				 org-cdlatex-mode
				 org-agenda-log-mode
				 diary-mode))
	"OrgMode")
 (t
	(centaur-tabs-get-group-name (current-buffer))))))
	:hook
	(dashboard-mode . centaur-tabs-local-mode)
	(term-mode . centaur-tabs-local-mode)
	(calendar-mode . centaur-tabs-local-mode)
	(org-agenda-mode . centaur-tabs-local-mode)
	(helpful-mode . centaur-tabs-local-mode)
	 :bind
	 (("C-M-'" . centaur-tabs-backward)
		("C-M-," . centaur-tabs-forward)
	("C-c t s" . centaur-tabs-counsel-switch-group)
	("C-c t p" . centaur-tabs-group-by-projectile-project)
	("C-c t g" . centaur-tabs-group-buffer-groups)
	))

(setq subword-mode t)

(add-hook 'text-mode-hook 'turn-on-auto-fill) ;test
(global-linum-mode 1)
(global-hl-line-mode)
(setq electric-pair-mode 1)
(use-package diff-hl
:config
(global-diff-hl-mode))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-window-scroll-bars (minibuffer-window) nil nil) ; minibuffer window has a scroll bar for some reason

;(use-package which-key)
;(which-key-mode)
