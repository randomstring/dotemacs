(setq user-emacs-directory "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d")
;
; set backspace key to do backwards delete
;
(global-set-key "\C-h" 'backward-delete-char-untabify)
;(global-set-key "\C-q" 'help-for-help)
(global-set-key "\C-u" 'undo)

; make the "end" key map to the end of the line
(define-key esc-map "[" 'end-of-line)

; turn off that damn overwrite mode
(global-set-key [insert] 'nil)
(global-set-key [kp_insert] 'nil)

;
; for hex mode
;
(autoload 'hexl-find-file "hexl" "Edit file FILENAME in hexl-mode." t)

;
; set functions for ispell - spell ckecker
;
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
;; which prompts for a line number to jump to.
(global-set-key "\C-x\C-j" 'goto-line)
(global-set-key "\C-xj" 'goto-line)

;; javascript mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; CSS mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;;
;; Mecurial
(load "~/.emacs.d/mercurial.el")

;; Start emacsserver
;; (server-start)

(require 'mwheel)
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

(add-to-list 'auto-mode-alist '("\\.t$" . perl-cmode))
(add-to-list 'auto-mode-alist '("\\.tt$" . html-mode))

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

;;; js-beautify.el -- beautify some js code

(defgroup js-beautify nil
  "Use jsbeautify to beautify some js"
  :group 'editing)

(defcustom js-beautify-args "--jslint-happy --brace-style=end-expand
--keep-array-indentation"
  "Arguments to pass to jsbeautify script"
  :type '(string)
  :group 'js-beautify)

(defcustom js-beautify-path "~/bin/js-beautify"
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


;; (print (getenv "SSH_CONNECTION"))

;; old way
;;(if (equal (getenv "SSH_CONNECTION") nil)
(print (display-graphic-p))

(if (display-graphic-p)
    ;; directly logged in probably using X11
    (custom-set-faces
     '(default ((t (:stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Inconsolata")))))
    ;; logged in via ssh
    (custom-set-faces
     '(default ((t (:stipple nil :background nil :foreground nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal )))))
    )


