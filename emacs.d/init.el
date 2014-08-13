;; Turn off the toolbar, scroll bar, and menu bar. Add line numbers.
(if (display-graphic-p)
  (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode -1)
    (global-linum-mode 1)))

(set-frame-font "Source Code Pro-16" nil t)

(electric-indent-mode 1)

;; Definition of a word includes symbol characters.
(dolist (c (string-to-list ":_-?!#*"))
  (modify-syntax-entry c "w" emacs-lisp-mode-syntax-table))

;; Allow emacsclient to launch faster.
(server-start)

(fset 'compile-and-goto-repl "\C-x\C-s\C-c\C-k\C-c\C-z")
(global-set-key [f5] 'compile-and-goto-repl)

(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t
      use-dialog-box nil
      visible-bell nil
      scroll-step 1
      scroll-conservatively 10000
      mouse-yank-at-point t
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil
      apropos-do-all t
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups")))
      frame-title-format
      '((:eval (if (buffer-file-name)
		 (abbreviate-file-name (buffer-file-name))
		 "%b"))))

(show-paren-mode 1)
(column-number-mode 1)
(defalias 'yes-or-no-p 'y-or-n-p)
(when (fboundp 'winner-mode)
  (winner-mode 1))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(require 'packages)
(require 'cljdoc)

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
