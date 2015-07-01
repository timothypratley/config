;; Turn off the toolbar, scroll bar, and menu bar. Add line numbers.

(progn
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (global-linum-mode 1))

(set-frame-font "Source Code Pro-16" nil t)

(electric-indent-mode 1)
(define-key global-map (kbd "RET") 'newline-and-indent)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Definition of a word includes symbol characters.
(dolist (c (string-to-list ":_-?!#*^/"))
  (modify-syntax-entry c "w" emacs-lisp-mode-syntax-table))

;; Allow emacsclient to launch faster.
(server-start)

(fset 'compile-and-goto-repl "\C-x\C-s\C-c\C-k\C-c\C-z")
(global-set-key [f5] 'compile-and-goto-repl)

(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t
      linum-format "%d "
      use-dialog-box nil
      visible-bell nil
      scroll-step 1
      scroll-conservatively 10000
      mouse-yank-at-point t
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil
      apropos-do-all t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t       ; use versioned backups
      frame-title-format
      '((:eval (if (buffer-file-name)
                 (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(show-paren-mode 1)
(column-number-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(when (fboundp 'winner-mode)
  (winner-mode 1))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(require 'packages)

(require 'cl)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
           "Prevent annoying \"Active processes exist\" query when you quit Emacs."
           (flet ((process-list ())) ad-do-it))

(defun clj-scratch ()
  "Create/retrieve a Clojure scratch buffer and switch to it.."
  (interactive)
  (let ((buf (get-buffer-create "*clj-scratch*")))
    (switch-to-buffer buf)
    (clojure-mode)))

(defun cljs-scratch ()
  "Create/retrieve a ClojureScript scratch buffer and switch to it.."
  (interactive)
  (let ((buf (get-buffer-create "*cljs-scratch*")))
    (switch-to-buffer buf)
    (clojurescript-mode)))

(defun cider-eval-expression-at-point-in-repl ()
  (interactive)
  (let ((form (cider-defun-at-point)))
    ;; Strip excess whitespace
    (while (string-match "\\`\s+\\|\n+\\'" form)
           (setq form (replace-match "" t t form)))
    (set-buffer (cider-get-repl-buffer))
    (goto-char (point-max))
    (insert form)
    (cider-repl-return)))

(require 'cider-mode)
(define-key cider-mode-map
            (kbd "C-;") 'cider-eval-expression-at-point-in-repl)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-max-face-count 1)
 '(safe-local-variable-values (quote ((ffip-project-file . "project.clj")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(idle-highlight ((t (:underline t))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "gray50"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "gray50")))))
