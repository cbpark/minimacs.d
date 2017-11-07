;;; init.el --- Emacs init file

;;; Commentary:
;;;   Init file for using Emacs

;;; Code:

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; for custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; to fix a security vulnerability
(when (version< emacs-version "25.3")
  (eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end))))

;; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; if indent-tabs-mode is off, untabify before saving
(add-hook 'write-file-hooks (lambda () (when (not indent-tabs-mode)
                                         (untabify (point-min) (point-max))) nil))

(fset 'yes-or-no-p 'y-or-n-p)

;; dired
(require 'dired-x)
(setq-default dired-omit-files-p t)
(put 'dired-find-alternate-file 'disabled nil)

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^")
  (lambda () (interactive) (find-alternate-file "..")))
(define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)

;; ido
(require 'ido)
(ido-mode 1)

;; recentf
(require 'recentf)
(run-at-time nil (* 10 60) 'recentf-save-list)
(recentf-mode 1)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file (find-file file))))
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

;; winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (global-set-key (kbd "C-x 0") 'winner-undo)
  (global-set-key (kbd "C-x 9") 'winner-redo))

;; shortcut of commands
(defalias 'qrr 'query-replace-regexp)
(defalias 'srr 'replace-string)
(defalias 'dw  'delete-trailing-whitespace)

;; key bindings
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-c o")   'occur)
(global-set-key (kbd "M-o")     'other-window)
(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-x w")   'delete-frame)
(global-set-key (kbd "M-SPC")   'cycle-spacing)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c e")   'eshell)

(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-up   4)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-down 4)))
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))

;; start server
(require 'server)
(unless (server-running-p) (server-start))

;;; init.el ends here
