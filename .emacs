;; https://github.com/CachesToCaches/getting_started_with_use_package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq user-emacs-directory "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d")

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

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
  :defer 2
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (elpy-enable)
    (setq elpy-rpc-backend "jedi")))

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
  :defer 2
  :diminish magit-auto-revert-mode
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
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

;; I used to use C-u for undo, now I have mac Command-Z
;(global-set-key "\C-u" 'undo)

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
(setq ispell-program-name "/usr/local/bin/aspell")
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

;; octave and matlab mode
(autoload 'octave-mode "octave" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;;
;; Mecurial
(load "~/.emacs.d/mercurial.el")

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
(add-hook 'perl-mode-hook 'my-perl-mode-hook)

(defun untabify-buffer ()
    (interactive)
    (untabify (point-min) (point-max)))

(defun my-perl-mode-hook ()
  (interactive)
  (add-hook 'local-write-file-hooks 'delete-trailing-whitespace)
  (add-hook 'local-write-file-hooks 'untabify-buffer)
  (setq indent-tabs-mode nil))

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
 '(safe-local-variable-values (quote ((c-basic-indent . 4)))))


(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; js-beautify.el -- beautify some js code
; pip install jsbeautifier
; ~/anaconda/bin/js-beautify
;
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

(defcustom js-beautify-path "~/anaconda/bin/js-beautify"
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

;; Org mode
;;(setq org-todo-keywords '((type "TODO" "WAITING" "|" "CANCELED" "DONE")))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-directory "~/Dropbox/TODO")
(setq org-agenda-files (list "~/Dropbox/TODO" "~/Box Sync/TODO"))
(setq org-archive-location "~/Dropbox/TODO_archive")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/DropBox/Apps/MobileOrg/mobile.org")
(setq org-log-done 'time)

; org-mode hot keys
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)

(defun html-org-mode-save-hook()
  "Export org-mode as HTML save hook"
  (message "Save HTML version")
  (org-html-export-to-html)
  )

(add-hook 'org-mode-hook 
          (lambda () 
             (add-hook 'after-save-hook 'html-org-mode-save-hook nil 'make-it-local)))

;; Markdown mode
;; to preview: C-c C-c p or C-c C-c l
;; to install markdown on the mac run "brew install markdown"
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

;; Github flavored Markdown
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#000000" :foreground "#eeeeee" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
