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

(fset 'tk-org-insert-lisp-block
   "#+begin_src emacs-lisp\C-m\C-m#+end_src\C-p")
(global-set-key (kbd "<f2>") 'tk-org-insert-lisp-block)

(fset 'tk-org-insert-rust-block
   "#+begin_src rust\C-m\C-m#+end_src\C-p")
(global-set-key (kbd "<f3>") 'tk-org-insert-rust-block)

(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "<f7>") 'eshell)

(setq make-backup-files nil) ; none of these~
(setq auto-save-default t)

(use-package solarized-theme
 :config (load-theme 'solarized-gruvbox-dark t))
;; make src block code look like normal text
(add-hook 'text-mode-hook
					 (lambda ()
						(variable-pitch-mode 1)))

(use-package zoom
	:config (zoom-mode t))

(setq subword-mode t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-linum-mode 1)
(global-hl-line-mode)
(setq electric-pair-mode 1)
(use-package diff-hl)
(global-diff-hl-mode)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(set-window-scroll-bars (minibuffer-window) nil nil) ; minibuffer window has a scroll bar for some reason

(global-prettify-symbols-mode t)
(setq column-number-mode t)
(setq line-number-mode t)

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (setq size-indication-mode t) ; display how long file is in modeline
)

(use-package minions
  :config
;  (setq minions-mode-line-lighter "Hey Thor ;)" ; because why not
;        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

(use-package ivy)
(use-package swiper) ; search extension to ivy
(use-package counsel) ;
(ivy-mode 1) ; globally turn on ivy
(setq ivy-use-virtual-buffers t) ; variably sized
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop) ; nicer kill ring
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h l") 'counsel-find-library)
(global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c j") 'counsel-file-jump)

(global-set-key (kbd "C-c r") 'ivy-resume)
;(global-set-key (kbd "C-c b") 'counsel-bookmark) ; weird stuff goin on
(global-set-key (kbd "C-c o") 'counsel-outline)

(use-package ivy-rich
 :config (ivy-rich-mode 1))
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line) ; recommended format
(setq ivy-rich-path-style 'abbrev) ; abbreviate paths with ~/

(use-package avy)
(global-set-key (kbd "M-t") 'avy-goto-word-1)

(use-package projectile)
(use-package counsel-projectile)
(counsel-projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package magit)
(use-package forge)

(use-package exec-path-from-shell
:init (exec-path-from-shell-initialize)
:config (when (memq window-system '(mac ns x)) ; sets MANPATH, PATH, exec-path-from-shell in osX/linux
(exec-path-from-shell-initialize)))

(use-package rg)
(hrs/append-to-path "/usr/local/bin") ; oddly wasn't globally in path, fixing that

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(use-package dumb-jump)
(global-set-key (kbd "C-M-s") 'dumb-jump-go)
(global-set-key (kbd "C-M-r") 'dumb-jump-back)
(setq dumb-jump-force-searcher 'rg)
(setq dumb-jump-selector 'ivy)

(use-package re-builder)
(setq reb-re-syntax 'string)

(use-package simpleclip)
(simpleclip-mode 1)

(use-package which-key)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-adapt-indentation nil)
;(setq org-pretty-entities nil) ; quick latex-ify in org files; annoying in codesnippets

(setq org-directory "~/org")
	(setq org-todo-keywords									; ! = timestamp, @ = create note
				'((sequence "TODO(t!)" "NOW(n!)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
(setq org-log-done 'time) ; log when finished

(use-package org-download)

(setq org-default-notes-file (concat org-directory "~/org/tktodos.org")) ; capture
	(setq org-capture-templates
				 '(
		 ("z" "Misc todo" entry (file+headline "~/org/misc.org" "Misc")
			"* TODO \t %? :MISC:\nAdded: %u:" :empty-lines 1 )
		 ("d" "Dev" entry (file+headline "~/org/dev.org" "Dev")
			"* TODO \t %? :DEV:\nAdded: %u" :empty-lines 1 )
		 ("M" "Main Dev" entry (file+headline "~/org/main.org" "Main")
			"* TODO [#A] \t %? :MAIN:DEV:\nAdded: %u" :empty-lines 1 )
		 ("R" "Main Rsch" entry (file+headline "~/org/main.org" "Main")
			"* TODO [#A] \t %? :MAIN:RSCH:\nAdded: %u" :empty-lines 1 )
		 ("e" "Emacs" entry (file+headline "~/org/emacs.org" "Emacs")
			"* TODO \t %? :EMACS:\nAdded: %u" :empty-lines 1 )
		 ("p" "Personal" entry (file+headline "~/org/pers.org" "Pers")
			"* TODO \t %? :PERS:\nAdded: %u" :empty-lines 1 )
		 ("r" "Research" entry (file+headline "~/org/rsch.org" "Rsch")
			"* TODO \t %? :RSCH:\nAdded: %u" :empty-lines 1 )
		 ("i" "Idea" entry (file "~/org/ideas.org")
			"* \t %? :IDEA:\nAdded: %u" )
))

(add-hook 'org-mode-hook
			(lambda ()
				(local-set-key (kbd "C-c C-x C-l") 'org-clock-in-last)
))

(setq org-clock-idle-time 15) ;prompt after 15 idle minutes.

(setq org-agenda-files '("~/org" ))

; tf not used heavily atm
(setq org-agenda-custom-commands ; options - todo, tags, tags-todo
			'(("d" "Dev" tags-todo "DEV")
				("e" "Emacs" tags-todo "EMACS")
				("p" "Personal" tags-todo "PERS")
				("r" "Research" tags-todo "RSCH")
				("m" "Research" tags-todo "MAIN")
				))
(setq org-agenda-start-on-weekday nil) ; start today

(setq org-tag-alist '(("dev" . d) ("personal" . ?p) ("research" . ?r) ("main" . ?m)))

(use-package company-org-roam)
	(setq org-roam-completion-system 'ivy)
	(use-package org-roam
				:hook
				(after-init . org-roam-mode)
				:custom ; adjust graph dot executable
				(org-roam-directory "~/org/roam")
				(setq org-roam-tag-sources '(prop all-directories)) ; tag all intermediate dirs
				:bind (:map org-roam-mode-map
								(("C-c n l" . org-roam)
								 ("C-c n f" . org-roam-find-file)
								 ("C-c n g" . org-roam-graph))
								:map org-mode-map
								(("C-c n i" . org-roam-insert))
								(("C-c n c" . org-roam-capture))
;								(("C-c n I" . org-roam-insert-immediate))
))

(setq org-roam-capture--file-name-default "<%Y-%m%-%d>")
(setq org-roam-capture-templates
			 ;; '(("p" "paper" plain (function org-roam--capture-get-point)
			 ;; 	 "%?"
			 ;; 	 :file-name "paper/${topic}/${subtopic}/${slug}"
			 ;; 	 :head: "#+title: ${title}\n"
			 ;; 	 :unnarrowed t)
			 '(("w" "web" plain (function org-roam--capture-get-point)
				"%?"
				:file-name "web/${topic}/${subtopic}/${slug}"
				:head "#+title: ${title}\n"
				:unnarrowed t
				)
				 ))

(setq org-roam-graph-executable "/usr/local/bin/dot")
(use-package graphviz-dot-mode
  :config
(setq graphviz-dot-indent-width 4))
;(setq org-roam-graph-viewer "/Applications/Safari.app/Contents/MacOS/safari")

(setq-default tab-width 2)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)) ; test

(setq lsp-keymap-prefix "M-n")
	(use-package lsp-mode
			:hook (rustic-mode . lsp)
;			:hook (sh-mode . lsp)
			:hook(go-mode . lsp)
			:commands lsp)
	;; optionally
	(use-package lsp-ui :commands lsp-ui-mode)
	(use-package lsp-ivy :commands lsp-ivy-workspa ce-symbol)
	(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
	(use-package dap-mode) ; debugger - no dap-rust yet
	;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package rustic)  ; many nifty convenience functions. defaults to rust-analyzer > rls
(setq rustic-lsp-server 'rust-analyzer)
(setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))
(custom-set-faces
  '(rustic-compilation-error ((t (:foreground "Red"))))
  '(rustic-compilation-warning ((t (:foreground "Red"))))
  '(rustic-compilation-message ((t (:foreground "Red"))))
  '(rustic-compilation-info ((t (:foreground "Blue"))))
  '(rustic-compilation-line ((t (:foreground "Blue")))))

(use-package rust-mode
  :config
	(hrs/append-to-path "~/.cargo/bin")
  (setq rust-format-on-save t))

(use-package cargo)
(use-package toml-mode)

; testing, possibly useful on Starchy
;(setenv "RUST_SRC_PATH" "/home/thor/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library")
(use-package racer
:config (setq company-tooltip-align-annotations t)
:hook ((rust-mode . racer-mode)
(rust-mode . rustic-mode)
(add-racer-mode . eldoc-mode) ; shows in echo area the arg list of the fn at point
(racer-mode . company-mode)) ; company autocomplete sometimes slows editor down significantly
:bind (:map rust-mode-map ("TAB" . company-indent-or-complete-common)))

(use-package flycheck-rust) ; runs on save buffer
(with-eval-after-load 'rust-mode
		(add-hook 'flycheck-mode-hook 'flycheck-rust-setup))
(add-hook 'rust-mode-hook
		(lambda ()
		(setq cargo-minor-mode t) ; Cc Cc C(b/r/t)
	))

(use-package rust-playground)

(use-package ob-rust)

(use-package go-mode)
(use-package go-errcheck)
(use-package company) ; autocompletes
(use-package company-go)
(setq company-tooltip-limit 20)     ; bigger popup window
(setq company-idle-delay .2)        ; decrease delay before autocompletion popup shows

(setenv "GOPATH" "/Users/thor/go")
(hrs/append-to-path (concat (getenv "GOPATH") "/bin")) ; user gopath
(hrs/append-to-path "/usr/local/go/bin") ; other shit that we like

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook
						(lambda ()
							(if (not (string-match "go" compile-command))
									(set (make-local-variable 'compile-command)
										 "go run ")
								)))

(add-hook 'go-mode-hook
					(lambda ()
						(set (make-local-variable 'company-backends)
								 '(company-go))
						(company-mode)
						(flycheck-mode)
						(local-set-key (kbd "C-c C-c C-r") 'compile)
	))

(use-package paredit)
(use-package rainbow-delimiters)

(setq lispy-mode-hooks
      '(emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))
(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (setq show-paren-style 'expression)
                   (paredit-mode)
                   (rainbow-delimiters-mode))))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package flycheck-package) ; should b called emacs-lisp flycheck
(eval-after-load 'flycheck
  '(flycheck-package-setup))

(use-package solidity-mode)
; optionals
(setq solidity-comment-style 'slash) ; 'star by default
; optional keybinds
(define-key solidity-mode-map (kbd "C-c C-g") 'solidity-estimate-gas-at-point)
; provide path to solc and solium binaries ;  these are overlapping checkers and can be used simultaneously
; (setq solidity-solc-path "/bin")
; (setq solidity-solium-path "/home/lefteris/.npm-global/bin/solium")
;(use-package 'solidity-flycheck)
;(setq solidity-flycheck-solc-checker-active t)
;(setq solidity-flycheck-solium-checker-active t)
; (setq flycheck-solidity-solc-addstd-contracts t) ; enable to include standard contracts
(use-package company-solidity)
(add-hook 'solidity-mode-hook
	(lambda ()
	(set (make-local-variable 'company-backends)
		(append '((company-solidity company-capf company-dabbrev-code))
			company-backends))))

(use-package company-shell)
(add-to-list 'company-backends 'company-shell)

;  (use-package auctex)
; Error (use-package): ; auctex/:catch: Loading file /Users/thor/.emacs.d/elpa/auctex-12.2.4/auctex.elc failed to provide feature auctex
	(setq TeX-auto-save t)
	(setq TeX-parse-self t)
	(setq-default TeX-master nil)
	(add-hook 'LaTeX-mode-hook 'visual-line-mode) ; an altern to auto-fill-mode
	(add-hook 'LaTeX-mode-hook 'flyspell-mode)
	(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
	(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
	(setq reftex-plug-into-AUCTeX t)
	(add-hook 'tex-mode-hook
						(lambda ()
						(latex-electric-env-pair-mode)))

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 100))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 60) ; close as I can get to full left half
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)
