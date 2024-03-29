; to inspect init errors, run "emacs --debug-init"

; unset this debug line to see full stack for erros
; (setq debug-on-error t)

;;https://melpa.org/#/getting-started
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; https://github.com/CachesToCaches/getting_started_with_use_package
;;(require 'package)
;;(setq package-enable-at-startup nil)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
;;(package-initialize)

; BROKEN??
;(when (memq window-system '(mac ns))
;  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;  (setq exec-path (append exec-path '("/usr/local/bin")))
;  )

(when (require 'mac-print-mode nil t)
  (mac-print-mode 1)
  (global-set-key (kbd "M-p") 'mac-print-buffer))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;;(setenv "PATH" (concat (getenv "PATH") ":~/.virtualenvs/default/bin"))
;;(setq exec-path (append exec-path '("~/.virtualenvs/default/bin")))
(setq user-emacs-directory "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d")

(use-package pyenv-mode)
(pyenv-mode)

(use-package diminish)

(use-package flycheck
  :ensure t
  :diminish ""
  :init
  (progn
    (setq flycheck-indication-mode 'left-fringe)
    ;; disable the annoying doc checker
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
  :config
  (global-flycheck-mode 1))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;;(use-package elpy
;;  :ensure t
;;  :defer 2
;;  :init
;;    ;; enable a virtualenv so we have flake8, etc in the path
;;    (pyvenv-workon "default")
;;  :config
;;  (progn
;;   ;; Use Flycheck instead of Flymake
;;    (when (require 'flycheck nil t)
;;      (remove-hook 'elpy-modules 'elpy-module-flymake)
;;      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
;;      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
;;      (add-hook 'elpy-mode-hook 'flycheck-mode))
;;    (elpy-enable)
;;    (setq elpy-rpc-backend "jedi")))

(use-package web-mode
  :ensure t
  :defer 2
  :bind (("C-c C-v" . browse-url-of-buffer)
         ("C-c w t" . web-mode-element-wrap))
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.php$" . web-mode)))
  :config
  (progn
    ;; Set tab to 4 to play nice with plebeian editors
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)))

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" "\\.json\\'")
  :interpreter "node")

(use-package css-mode
  :init
  :mode ("\\.css\\'"))

(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching t))

;; full screen magit-status
; (defadvice magit-status (around magit-fullscreen activate)
;   (window-configuration-to-register :magit-fullscreen)
;    ad-do-it
;    (delete-other-windows)))

;; Restore windows after exiting magit
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(use-package magit
  :ensure t
;  :defer 2  ;; this breaks things. why?
  :diminish magit-auto-revert-mode
  :init
;;  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (bind-key "q" 'magit-quit-session magit-status-mode-map))

; https://github.com/bnbeckwith/writegood-mode
(use-package writegood-mode
  :ensure t
  :defer 2
  :mode "\\.txt\\'")
;; Set a global key to toggle the mode
(global-set-key "\C-cg" 'writegood-mode)

;
; in the stone ages the backspace key would generate C-h
; and making it behave required a heavy hand.
;(global-set-key "\C-h" 'backward-delete-char-untabify)

; Overload Alt-z to do the same as Command-z for rough
; keyboard equivalence between Linux and Mac keyboards.
; At least on my linux keyboard, Alt is where the Command
; key is on my mac. Sorry, no more Zap-to-char shortcut.
(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode 1) 
    (defalias 'redo 'undo-tree-redo)
    (global-set-key "\M-z" 'undo)      ; Alt-z
    (global-set-key "\M-Z" 'redo)      ; Alt-Z
    )
  )

; make the "end" key map to the end of the line, not needed on small
; keyboards or on Macs. Useful for HUGE keyboards with "End" keys.
(define-key esc-map "[" 'end-of-line)

; turn off that damn overwrite mode. This was a problem with HUGE
; keyboards with the "Insert" key right next to "Del" accidently
; triggering overwrite mode is crazy annoying.
(global-set-key [insert] 'nil)
(global-set-key [kp_insert] 'nil)

;
; for hex mode
;
(autoload 'hexl-find-file "hexl" "Edit file FILENAME in hexl-mode." t)

;
; set functions for ispell - spell ckecker
;
; http://stackoverflow.com/questions/19022015/emacs-on-mac-os-x-how-to-get-spell-check-to-work
(setq exec-path (append "/usr/local/bin" exec-path))

(setq ispell-program-name "/usr/local/bin/aspell")
;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
(autoload 'ispell "ispell" "Run ispell over buffer" t)
(autoload 'ispell-region "ispell" "Run ispell over region" t)
(autoload 'ispell-word "ispell" "Check word under cursor" t)
(autoload 'ispell-complete-word "ispell"
  "Look up current word in dictionary and try to complete it." t)
(autoload 'ispell-message "ispell"
  "Check spelling of mail message or news post.")
(define-key esc-map "$" 'ispell-word)
(define-key esc-map "#" 'ispell-buffer)
(define-key esc-map "s" 'ispell-buffer)


;
; other things
;
(put 'eval-expression 'disabled nil)

;; Make the sequence "C-x C-j" execute the `goto-line' command,
;; which prompts for a line number to jump to. J for Jump to line.
(global-set-key "\C-x\C-j" 'goto-line)
(global-set-key "\C-xj" 'goto-line)

; treat .t as perl code
(add-to-list 'auto-mode-alist '("\\.t$" . perl-cmode))
; treat .tt (perl template toolkit) as html mode
(add-to-list 'auto-mode-alist '("\\.tt$" . html-mode))

(use-package yaml-mode
  :ensure t
  :defer 2
  :mode "\\.yaml\\'")
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; octave and matlab mode
;;(autoload 'octave-mode "octave" nil t)
;;(setq auto-mode-alist
;;      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;;
;; Mecurial
;;(load "~/.emacs.d/mercurial.el")

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


; enable mouse wheel scrolling
(require 'mwheel)
; disable progressive speed scroll that is overkill for the trackpad
(setq mouse-wheel-progressive-speed nil)
(setq which-function-modes t)

;; tab fixing
;;(add-hook 'perl-mode-hook 'my-perl-mode-hook)

(defun untabify-buffer ()
    (interactive)
    (untabify (point-min) (point-max)))

;; perl hooks are hanging on close
;;(defun my-perl-mode-hook ()
;;  (interactive)
;;  (add-hook 'write-file-functions 'delete-trailing-whitespace)
;;  (add-hook 'write-file-functions 'untabify-buffer)
;;  (setq indent-tabs-mode nil))


;;(add-hook 'yaml-mode-hook 'my-yaml-mode-hook)

;;(defun my-yaml-mode-hook ()
;;  (interactive)
;;  (add-hook 'write-file-functions 'delete-trailing-whitespace)
;;  (add-hook 'write-file-functions 'untabify-buffer)
;;  (setq indent-tabs-mode nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t nil (jka-compr))
 '(case-fold-search t)
 '(cperl-brace-offset 0)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 0)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-label-offset 0)
 '(cperl-tab-always-indent t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-screen t)
 '(menu-bar-mode t)
 '(package-selected-packages
   '(pydoc pyenv-mode-auto magithub python-mode pyenv-mode ensure flycheck-pycheckers flycheck-pyflakes jedi elpygen magit-org-todos writegood-mode web-mode use-package-chords undo-tree markdown-mode magit js2-mode flycheck elpy yaml-mode))
 '(safe-local-variable-values '((c-basic-indent . 4))))


(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

;; When we're in windows mode X or Mac
(when (window-system)
  (tool-bar-mode 0)                   ;; No Toolbar
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; js-beautify.el -- beautify some js code
; Consider using https://github.com/yasuyk/web-beautify/blob/master/web-beautify.el
;
(defgroup js-beautify nil
  "Use jsbeautify to beautify some js"
  :group 'editing)

(defcustom js-beautify-args "--jslint-happy --brace-style=end-expand
--keep-array-indentation"
  "Arguments to pass to jsbeautify script"
  :type '(string)
  :group 'js-beautify)

(defcustom js-beautify-path "js-beautify"
  "Path to jsbeautifier python file"
  :type '(string)
  :group 'js-beautify)

(defun js-beautify ()
  "Beautify a region of javascript using the code from jsbeautify.org"
  (interactive)
  (let ((orig-point (point)))
    (unless (mark)
      (mark-defun))
    (shell-command-on-region (point)
                             (mark)
                             (concat "python "
                                     js-beautify-path
                                     " --stdin "
                                     js-beautify-args)
                             nil t)
    (goto-char orig-point)))

(provide 'js-beautify)
;;; js-beautify.el ends here

;; Org Mode stuff starts here
(use-package ob-core)
(use-package ox-md)
(use-package ox-latex)
(use-package ox-beamer)

(use-package org
  :ensure t
  :defer 2
  :init
  (progn
    
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
    (setq org-directory "~/Dropbox/TODO")
    (setq org-default-notes-file
	  (concat org-directory "/TODO.org"))
    (setq org-agenda-files (list "~/Dropbox/TODO"))
    (setq org-archive-location "~/Dropbox/TODO_archive")
    ;;(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    ;;(setq org-mobile-inbox-for-pull "~/DropBox/Apps/MobileOrg/mobile.org")
    (setq org-log-done 'time)

    ;; From https://github.com/howardabrams/dot-files/blob/master/emacs-org.org
    (setq org-use-speed-commands t
	  org-return-follows-link t
	  org-hide-emphasis-markers t
	  org-completion-use-ido t
	  org-outline-path-complete-in-steps nil
	  org-src-fontify-natively t   ;; Pretty code blocks
	  org-src-tab-acts-natively t
	  org-confirm-babel-evaluate nil
	  org-todo-keywords
	    '((sequence "TODO(t)" "WAITING(w)" "|" "CANCELED(c)" "DONE(d)" "NOTE(n)")))
    )
  :config
  (progn
    ;; org-mode hot keys
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-cb" 'org-iswitchb)

    ;;(defun html-org-mode-save-hook()
    ;;  "Export org-mode as HTML save hook"
    ;;  (message "Save HTML version")
    ;;  (org-html-export-to-html)
    ;;  )

    ;; I used to like saving .html versions of all my .org files. I
    ;; don't find this useful after all. Org file rendering on github
    ;; has gotten better.
    ;;(add-hook 'org-mode-hook 
    ;;          (lambda () 
    ;;             (add-hook 'after-save-hook 'html-org-mode-save-hook nil 'make-it-local)))

    ))

;;; END Org Mode stuff


;; Markdown mode
;; to preview: C-c C-c p or C-c C-c l
;; to install markdown on the mac run "brew install markdown"
;(use-package markdown-mode
;  :ensure t
;  :commands (markdown-mode gfm-mode)
;  :mode
;  (("README\\.md\\'" . gfm-mode)
;   ("\\.md\\'" . gfm-mode)
;   ("\\.markdown\\'" . markdown-mode))
;  :init
;  (if (memq window-system '(mac ns))
;    (setq markdown-command "multimarkdown")  ; mac uses multimarkdown
;    (setq markdown-command "markdown"))      ; unix uses markdown
;  )

;; Github flavored Markdown
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Reopen readonly files as root
;; from: https://github.com/howardabrams/dot-files/blob/master/emacs.org#editing-root-files
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (let* ((file-name (buffer-file-name))
           (file-root (if (string-match "/ssh:\\([^:]+\\):\\(.*\\)" file-name)
                          (concat "/ssh:"  (match-string 1 file-name)
                                  "|sudo:" (match-string 1 file-name)
                                  ":"      (match-string 2 file-name))
                        (concat "/sudo:localhost:" file-name))))
      (find-alternate-file file-root))))

;; Write backup files to their own directory, don't pollute dirs with ~ files 
;; backup files are put in ~/.emacs.d.backups
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory ".backups")))))

(setq vc-make-backup-files t)

(setq initial-scratch-message "") ;; Start with an empty scratch buffer



(use-package ob-shell
  :defer t
  :config
  (setq org-babel-sh-command "bash"))

;; HINT: if jupyter is missing, it needs to be intalled with pip
;(use-package ob-ipython
;  :ensure t)

;; https://github.com/porterjamesj/virtualenvwrapper.el
;(use-package virtualenvwrapper
;  :ensure t
;  :defer t
;)
(require 'virtualenv)
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)

;; Use M-x venv-workon to activate virtualenvs and
;; M-x venv-deactivate deactivate them.


;; list of babel programming languages to honor
(org-babel-do-load-languages 'org-babel-load-languages
			     '((shell      . t)
			       (js         . t)
			       (emacs-lisp . t)
			       (perl       . t)
;;			       (scala      . t)
			       (clojure    . t)
			       (python     . t)
;;			       (ipython    . t)
			       (ruby       . t)
			       (dot        . t)
			       (java       . t)
			       (css        . t)))

;; Have different display options based on if we are running in a terminal
;; or running in a windowing environment.
;; ACTUALLY, now I think the settings work for both.
(if (display-graphic-p)
    ;; directly logged in probably using X11
    (custom-set-faces
     '(default ((t (:stipple nil :background "#000000" :foreground "#eeeeee" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
  ;; logged in via ssh
  (custom-set-faces
   '(default ((t (:stipple nil :background nil :foreground nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal )))))
  )

;; Font Help
;; Use ` S-down-mouse-1 ' or ` M-x menu-set-font ' to see the font and fontset menu. 



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#000000" :foreground "#eeeeee" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
(put 'upcase-region 'disabled nil)
