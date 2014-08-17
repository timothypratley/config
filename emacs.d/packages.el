(require 'package)

(setq-default save-place t)
(setq evil-shift-width 2
      evil-want-C-u-scroll t
      evil-want-change-word-to-end t
      evil-default-cursor t
      evil-want-fine-undo t
      save-place-file (concat user-emacs-directory "places")
      ido-enable-flex-matching t
      ido-everywhere t
      ido-use-virtual-buffers t
      uniquify-buffer-name-style 'forward
      nrepl-hide-special-buffers t
      cider-auto-select-error-buffer nil
      cider-prompt-save-file-on-load nil
      cider-repl-result-prefix ";; => ")

(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
		  ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))

(package-initialize)

(dolist (package '(ace-jump-mode
                   dash
                   evil
                   paredit
                   column-marker
		   clj-refactor
		   cljsbuild-mode
                   clojure-mode
		   clojure-mode-extra-font-locking
                   cider
                   exec-path-from-shell
                   ibuffer
                   idle-highlight-mode
                   ido
                   ido-better-flex
                   ido-ubiquitous
                   key-chord
                   magit
                   rainbow-delimiters
                   saveplace
                   smex
		   yasnippet
		   ; themes
		   ample-theme
		   color-theme-sanityinc-tomorrow
		   gruber-darker-theme
		   noctilux-theme
		   spacegray-theme))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

;(load-theme 'sanityinc-tomorrow-night t)
;(load-theme 'gruber-darker t)
(load-theme 'noctilux t)

(exec-path-from-shell-initialize)

(key-chord-mode 1)

(evil-mode t)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "M-.") 'nrepl-jump)
(define-key evil-normal-state-map (kbd "M-,") 'nrepl-jump-back)

;; Escape quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(key-chord-define evil-normal-state-map "jk" 'evil-force-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-change-to-previous-state)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)

(key-chord-define evil-normal-state-map "bb" 'ido-switch-buffer)
(key-chord-define evil-normal-state-map "bj" 'evil-prev-buffer)
(key-chord-define evil-normal-state-map "bk" 'evil-next-buffer)
(key-chord-define evil-normal-state-map "xk" 'kill-this-buffer)
(key-chord-define-global "jx" 'smex)

(ido-mode t)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'eldoc)
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'prog-mode-hook 'enable-paredit-mode)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))
(add-hook 'prog-mode-hook 'idle-highlight-mode)
(add-hook 'prog-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)
			       (cljr-add-keybindings-with-prefix "C-c C-j")))
(eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
(global-rainbow-delimiters-mode)

(add-to-list 'load-path "~/.emacs.d/snippets")
(yas-global-mode 1)

(provide 'packages)
