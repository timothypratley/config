(require 'package)

(add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))

;;(eval-after-load 'flycheck '(flycheck-clojure-setup))
;;(add-hook 'after-init-hook #'global-flycheck-mode)
;;(eval-after-load 'flycheck '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(setq-default save-place t)
(setq blink-cursor-blinks -1
      ;;cider-auto-select-error-buffer nil
      ;;cider-prompt-save-file-on-load nil
      cider-repl-result-prefix ";; => "
      cider-prefer-local-resources t
      ;;cider-repl-pop-to-buffer-on-connect nil
      ;;cider-show-error-buffer nil
      cider-known-endpoints '(("te" "trick" "22345"))
      cljr-magic-require-namespaces
      '(("edn" . "clojure.edn")
        ("io" . "clojure.java.io")
        ("log" . "clojure.tools.logging")
        ("set" . "clojure.set")
        ("str" . "clojure.string")
        ("string" . "clojure.string")
        ("pprint" . "clojure.pprint")
        ("time" . "clj-time.core")
        ("walk" . "clojure.walk")
        ("zip" . "clojure.zip"))
      evil-shift-width 2
      evil-want-C-u-scroll t
      evil-want-change-word-to-end t
      evil-default-cursor t
      evil-want-fine-undo t
      gc-cons-threshold 20000000
      ido-enable-flex-matching t
      ido-everywhere t
      ido-use-virtual-buffers t
      magit-last-seen-setup-instructions "1.4.0"
      nrepl-hide-special-buffers t
      nrepl-log-messages t
      save-place-file (concat user-emacs-directory "places")
      uniquify-buffer-name-style 'forward
      package-pinned-packages '((cider . "melpa-stable")))

(when (>= emacs-major-version 24)
    (setq package-archives '(("ELPA" .  "http://tromey.com/elpa/")
                             ("gnu" .  "http://elpa.gnu.org/packages/")
                             ("melpa" .  "http://melpa.org/packages/")
                             ("melpa-stable" .  "http://stable.melpa.org/packages/")
                             ("marmalade" .  "http://marmalade-repo.org/packages/"))))

(package-initialize)

(dolist (package '(ace-jump-mode
                   ag
                   dash
                   evil
                   ;;flycheck
                   ;;flycheck-clojure
                   ;;flycheck-pos-tip
                   flx-ido
                   helm
                   helm-ag
                   paredit
                   projectile
                   column-marker
                   company
                   cider
                   ;;cider-eval-sexp-fu
                   ;;clj-refactor
                   cljsbuild-mode
                   clojure-mode
                   dirtree
                   elm-mode
                   exec-path-from-shell
                   ibuffer
                   idle-highlight-mode
                   ido
                   ido-better-flex
                   ido-ubiquitous
                   json-mode
                   coffee-mode
                   ;;key-chord
                   magit
                   rainbow-delimiters
                   saveplace
                   smex
                   yasnippet
                   yaml-mode
                   restclient
                   ;; themes
                   ;ample-theme
                   ;base16-theme
                   ;color-theme-sanityinc-tomorrow
                   ;gruber-darker-theme
                   noctilux-theme
                   ;spacegray-theme
                   ))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(global-company-mode)

(load-theme 'noctilux t)

(exec-path-from-shell-initialize)

;;(key-chord-mode 0)

(evil-mode t)
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "M-.") 'cider-jump-to-var)
(define-key evil-normal-state-map (kbd "M-,") 'cider-jump-back)

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
(key-chord-define-global "jx" 'smex)
(key-chord-define-global "js" 'save-buffer)

(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
;(define-key evil-normal-state-map "\C-d" 'evil-delete-char)
;(define-key evil-insert-state-map "\C-d" 'evil-delete-char)
;(define-key evil-visual-state-map "\C-d" 'evil-delete-char)
(define-key evil-normal-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)
;(define-key evil-normal-state-map "\C-w" 'evil-delete)
;(define-key evil-insert-state-map "\C-w" 'evil-delete)
;(define-key evil-visual-state-map "\C-w" 'evil-delete)
(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-visual-state-map "\C-y" 'yank)
(define-key evil-normal-state-map "\C-k" 'paredit-kill)
(define-key evil-insert-state-map "\C-k" 'paredit-kill)
(define-key evil-visual-state-map "\C-k" 'paredit-kill)
(define-key evil-normal-state-map "Q" 'call-last-kbd-macro)
(define-key evil-visual-state-map "Q" 'call-last-kbd-macro)
(define-key evil-normal-state-map (kbd "TAB") 'evil-undefine)

;;(key-chord-define-global "bb" 'ido-switch-buffer)
(key-chord-define-global "bp" 'evil-prev-buffer)
(key-chord-define-global "bn" 'evil-next-buffer)
(key-chord-define-global "bk" 'quit-window)
(key-chord-define-global "zh" 'evil-window-left)
(key-chord-define-global "zj" 'evil-window-down)
(key-chord-define-global "zk" 'evil-window-up)
(key-chord-define-global "zl" 'evil-window-right)

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
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))
(add-hook 'prog-mode-hook 'idle-highlight-mode)
(add-hook 'prog-mode-hook 'turn-on-eldoc-mode)
;(add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)
                               ;(cljr-add-keybindings-with-prefix "C-c C-r")))
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
  (clojure.tools.namespace.repl/refresh)")
  (cider-test-run-tests))
(require 'clojure-mode)
(define-key clojure-mode-map (kbd "C-c C-r") 'cider-namespace-refresh)
;(add-hook 'cider-mode-hook
          ;'(lambda () (add-hook 'after-save-hook
                                ;'(lambda () (if (and (boundp 'cider-mode) cider-mode)
                                              ;(cider-namespace-refresh))))))
(eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
;(require 'cider-eval-sexp-fu)
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
                        (setq js-indent-level 2)))

(add-to-list 'load-path "~/.emacs.d/snippets")
(yas-global-mode 1)

(defun noprompt/forward-transpose-sexps ()
  (interactive)
  (paredit-forward)
  (transpose-sexps 1)
  (paredit-backward))

(defun noprompt/backward-transpose-sexps ()
  (interactive)
  (transpose-sexps 1)
  (paredit-backward)
  (paredit-backward))

(key-chord-define-global "tk" 'noprompt/forward-transpose-sexps)
(key-chord-define-global "tj" 'noprompt/backward-transpose-sexps)

(require 'dirtree)
(provide 'packages)

(require 'rainbow-delimiters)
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error)

(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)
;;(defun my-save () (if (buffer-file-name) (evil-save)))
;;(add-hook 'evil-insert-state-exit-hook 'my-save)
