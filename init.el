(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file "~/.emacs.d/config.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(ansi-color-names-vector
	 ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#93E0E3")
 '(cua-normal-cursor-color "#DCDCCC")
 '(cua-overwrite-cursor-color "#F0DFAF")
 '(cua-read-only-cursor-color "#7F9F7F")
 '(custom-safe-themes
	 (quote
		("35c096aa0975d104688a9e59e28860f5af6bb4459fd692ed47557727848e6dfe" "f490984d405f1a97418a92f478218b8e4bcc188cf353e5dd5d5acd2f8efd0790" "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "c5ad91387427abc66af38b8d6ea74cade4e3734129cbcb0c34cc90985d06dcb3" "2d835b43e2614762893dc40cbf220482d617d3d4e2c35f7100ca697f1a388a0e" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default)))
 '(electric-pair-mode t)
 '(electric-pair-pairs
	 (quote
		((96 . 96)
		 (91 . 93)
		 (123 . 125)
		 (40 . 41)
		 (34 . 34)
		 (8216 . 8217)
		 (8220 . 8221))))
 '(fci-rule-color "#dedede")
 '(highlight-changes-colors (quote ("#DC8CC3" "#bbb0cb")))
 '(highlight-symbol-colors
	 (quote
		("#680f63eb5998" "#54db645064d0" "#6097535f5322" "#5c2859a95fa1" "#4ede55f24ea4" "#64dd5979525e" "#530060d16157")))
 '(highlight-symbol-foreground-color "#FFFFEF")
 '(highlight-tail-colors
	 (quote
		(("#4F4F4F" . 0)
		 ("#488249" . 20)
		 ("#5dacaf" . 30)
		 ("#57a2a4" . 50)
		 ("#b6a576" . 60)
		 ("#ac7b5a" . 70)
		 ("#aa5790" . 85)
		 ("#4F4F4F" . 100))))
 '(hl-bg-colors
	 (quote
		("#b6a576" "#ac7b5a" "#9f5c5c" "#aa5790" "#85749c" "#57a2a4" "#5dacaf" "#488249")))
 '(hl-fg-colors
	 (quote
		("#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F" "#3F3F3F")))
 '(hl-paren-colors (quote ("#93E0E3" "#F0DFAF" "#8CD0D3" "#bbb0cb" "#7F9F7F")))
 '(line-spacing 0.2)
 '(lsp-ui-doc-border "#bdae93")
 '(notmuch-identities (quote ("thork@tuta.io")))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-logo nil)
 '(nrepl-message-colors
	 (quote
		("#CC9393" "#DFAF8F" "#F0DFAF" "#488249" "#95d291" "#57a2a4" "#93E0E3" "#DC8CC3" "#bbb0cb")))
 '(org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 2 :block today :scope agenda)))
 '(org-agenda-prefer-last-repeat nil)
 '(org-agenda-show-all-dates nil)
 '(org-agenda-show-future-repeats nil)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil t)
 '(org-agenda-start-with-clockreport-mode t)
 '(org-agenda-window-frame-fractions (quote (0.5 . 0.5)))
 '(org-roam-directory "~/org/roam")
 '(package-selected-packages
	 (quote
		(forge centaur-tabs zoom poet-theme emojify all-the-icons xkcd rustic-mode lsp-ui lsp-ivy lsp-mode workgroups2 ob-go yasnippet-snippets company-shell notmuch company-org-roam company-graphviz-dot graphviz-dot-mode auctex which-key use-package solarized-theme simpleclip rust-playground rg rainbow-delimiters racer py-autopep8 paredit ob-rust moody minions magit ivy-rich go-errcheck flycheck-rust flycheck-package elpy dumb-jump diff-hl deadgrep counsel-projectile company-jedi company-go cargo avy auto-compile)))
 '(pos-tip-background-color "#32302f")
 '(pos-tip-foreground-color "#bdae93")
 '(setq "%<%Y%m%d%H%M%S>" t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#98971a" "#32302f" 0.2))
 '(term-default-bg-color "#282828")
 '(term-default-fg-color "#a89984")
 '(user-mail-address "thork@tuta.io")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#fb4933")
		 (40 . "#eb7b77d82bd3")
		 (60 . "#e21e8997270c")
		 (80 . "#d79921")
		 (100 . "#c321997a1eab")
		 (120 . "#b8ac99341d7b")
		 (140 . "#ae1e98cb1c53")
		 (160 . "#a37098411b32")
		 (180 . "#98971a")
		 (200 . "#8bd699a03aed")
		 (220 . "#84849aa247bf")
		 (240 . "#7c5b9ba153ba")
		 (260 . "#731d9c9f5f38")
		 (280 . "#689d6a")
		 (300 . "#5cb793cf76ed")
		 (320 . "#55e88efd7cec")
		 (340 . "#4e348a3982c8")
		 (360 . "#458588"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
	 (quote
		(unspecified "#282828" "#32302f" "#b21b0a" "#fb4933" "#747400" "#98971a" "#a76e00" "#d79921" "#14676b" "#458588" "#9f4d64" "#d3869b" "#2e7d33" "#689d6a" "#a89984" "#282828")))
 '(wg-session-file "~/.emacs.d/workgroups")
 '(xterm-color-names
	 ["#32302f" "#fb4933" "#98971a" "#d79921" "#458588" "#d3869b" "#689d6a" "#a89984"])
 '(xterm-color-names-bright
	 ["#282828" "#d65d0e" "#7c6f64" "#282828" "#a89984" "#b16286" "#bdae93" "#fbf1c7"]))
 '(package-selected-packages
	 (quote
		(auctex ivy-rich which-key travis htmlize counsel-projectile projectile counsel swiper ivy ripgrep rg flycheck-inline-mode dumb-jump rust-playground ob-rust racer cargo company-rust deadgrep python-mode haskell-mode diff-hl minions solarized-theme simpleclip magit rust-mode flycheck-package rainbow-delimiters paredit flycheck use-package go-errcheck company-go auto-compile)))
 '(pos-tip-background-color "#4F4F4F")
 '(pos-tip-foreground-color "#FFFFEF")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#7F9F7F" "#4F4F4F" 0.2))
 '(term-default-bg-color "#3F3F3F")
 '(term-default-fg-color "#DCDCCC")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#CC9393")
		 (40 . "#df51b97ca1ae")
		 (60 . "#e83dcc9aa8b1")
		 (80 . "#F0DFAF")
		 (100 . "#cadbca369f51")
		 (120 . "#b7fbbf79973e")
		 (140 . "#a52cb4cc8f3f")
		 (160 . "#9260aa2d8754")
		 (180 . "#7F9F7F")
		 (200 . "#87dbb4dba003")
		 (220 . "#8b6ebfadb0a1")
		 (240 . "#8e96ca9fc17c")
		 (260 . "#914ed5b0d293")
		 (280 . "#93E0E3")
		 (300 . "#90c5da6cdd6f")
		 (320 . "#8f5dd735da39")
		 (340 . "#8df4d401d704")
		 (360 . "#8CD0D3"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
	 (quote
		(unspecified "#3F3F3F" "#4F4F4F" "#9f5c5c" "#CC9393" "#488249" "#7F9F7F" "#b6a576" "#F0DFAF" "#57a2a4" "#8CD0D3" "#aa5790" "#DC8CC3" "#5dacaf" "#93E0E3" "#DCDCCC" "#6F6F6F")))
 '(xterm-color-names
	 ["#4F4F4F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#fffff6"])
 '(xterm-color-names-bright
	 ["#3F3F3F" "#DFAF8F" "#878777" "#6F6F6F" "#DCDCCC" "#bbb0cb" "#FFFFEF" "#FFFFFD"])
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
