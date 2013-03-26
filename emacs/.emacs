(setq load-path  (cons (expand-file-name "~/emacs") load-path))

(modify-frame-parameters nil '((wait-for-wm . nil)))

; Set the font
(if 'nil
(set-default-font "-schumacher-clean-medium-r-normal--12-120-75-75-c-60-iso8859-7")
)

(load "site-defaults.el")
(load-defaults)

; Delete trailing whitespace on save
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

; No ~ backup files
(setq make-backup-files nil)
(setq find-file-suppress-same-file-warnings t)

(require 'smooth-scrolling)

; Turn off the toolbar
(tool-bar-mode 0)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-warning-face ((t (:foreground "red2" :weight bold)))))

(global-set-key "\C-\M-f" 'forward-to-indentation)
(global-set-key "\C-\M-b" 'backward-to-indentation)

(global-set-key (kbd "C-x M-.") 'tags-search)

(global-set-key (kbd "C-M-v") 'revert-buffer)
(global-set-key "\M-=" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))

        (t (error "%s" "Not on a paren, brace, or bracket"))

        )
)

(defun sort-words-in-lines (start end)
   (interactive "r")
   (goto-char start)
   (beginning-of-line)
   (while (< (setq start (point)) end)
      (let ((words (sort (split-string (buffer-substring start (line-end-position)))
                         (function string-lessp))))
        (delete-region start (line-end-position))
        (dolist (word words ) (insert word " ")))
      (beginning-of-line) (forward-line 1)))

(setq kill-whole-line t)

(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "OH") 'beginning-of-line)
(global-set-key (kbd "OF") 'end-of-line)
(global-set-key (kbd "DEL") 'backward-delete-char-untabify)
(global-set-key (kbd "RET") 'newline-and-indent)

;(setq indent-line-function 'indent-for-tab-command)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(global-set-key [(meta f12)] 'recentf-open-files)
;(setq recentf-max-menu-items 60)

; Enable case sensitivity
(setq ido-case-fold t)

(setq ido-ignore-files '("\\`CVS/" "\\`#" "\\`.#" "\\.pyc" "\\`\\.svn/" "\\.egg-info" "\\.scons"))

(setq ido-enable-last-directory-history nil)
(setq ido-auto-merge-work-directories-length -1) ; Disable the ido auto-merge
(setq ido-create-new-buffer 'always)

(global-set-key (kbd "C-c e") '(lambda () (interactive) (eval-last-sexp 'universal-argument) (backward-sexp) (backward-kill-sexp) (forward-sexp)))

(defun find-tag-current-word()
  "find the tag for the word currently under the cursor.
if none is found, call etags"
  (interactive)
  (find-tag (current-word)))

(define-key global-map [(control f12)] 'find-tag-current-word)
(define-key global-map [(f12)] '(lambda () "find next tag" (interactive) (execute-kbd-macro "\C-u\256")))

(setq fixme-modes '(python-mode vera-mode java-mode c-mode c++-mode emacs-lisp-mode scheme-mode))
(make-face 'font-lock-fixme-face)
(modify-face 'font-lock-fixme-face "Red" "Yellow" nil t nil t nil nil)
(make-face 'font-lock-todo-face)
(modify-face 'font-lock-todo-face "Blue" "Yellow" nil t nil t nil nil)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(FIXME\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
           )
        ))
      fixme-modes)

; Run 'occur' from interactive search
(define-key isearch-mode-map (kbd "C-,")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(require 'bscope)
(require 'iswitchb)
(iswitchb-mode 1)

(add-to-list 'iswitchb-buffer-ignore "^ ")
(add-to-list 'iswitchb-buffer-ignore "*Messages*")
(add-to-list 'iswitchb-buffer-ignore "*ECB")
(add-to-list 'iswitchb-buffer-ignore "*Buffer")
(add-to-list 'iswitchb-buffer-ignore "*Completions")
(add-to-list 'iswitchb-buffer-ignore "*Compile")
(add-to-list 'iswitchb-buffer-ignore "TAGS")

(setq ido-ignore-buffers '("\\*Completions" "\\TAGS" "\\*Compile" "\\Echo Area" "\\Minibuf"))

; No startup screen
(setq inhibit-startup-message t)

(autoload 'verilog-mode "verilog-mode-2013.el" "Verilog mode" t )
(setq auto-mode-alist (cons  '("\\.vp\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.svp\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.sv\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.svap\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.vg\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.svg\\'" . verilog-mode) auto-mode-alist))

(autoload 'go-mode "go-mode.el" "GO mode" t )
(setq auto-mode-alist (cons  '("\\.go\\'" . go-mode) auto-mode-alist))

(put 'downcase-region 'disabled nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(column-number-mode t)
 '(indent-tabs-mode nil)
 '(load-home-init-file t t)
 '(make-backup-files nil)
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(nxml-slash-auto-complete-flag t)
 '(show-paren-mode t nil (paren))
 '(tags-case-fold-search nil)
 '(transient-mark-mode t)
 '(vc-follow-symlinks t)
 '(verilog-align-ifelse t)
 '(verilog-auto-endcomments nil)
 '(verilog-auto-indent-on-newline nil)
 '(verilog-auto-newline nil)
 '(verilog-auto-read-includes nil)
 '(verilog-case-indent 4)
 '(verilog-cexp-indent 4)
 '(verilog-compiler (quote compile-command))
 '(verilog-indent-begin-after-if nil)
 '(verilog-indent-level 4)
 '(verilog-indent-level-behavioral 4)
 '(verilog-indent-level-declaration 4)
 '(verilog-indent-level-module 4))

;(defun get-string-from-file (file-path)
;  "Return FILEPATH's file content."
;  (with-temp-buffer
;    (insert-file-contents file-path)
;    (buffer-string)))
;
;(defun find-tec-autos ()
;  (setq local-env-root (strip (shell-command-to-string "runtool env_root")))
;  (if (not (string-match "ERROR" local-env-root))
;    (progn
;      (setq autos-path (concat local-env-root "/build/autos.filelist"))
;      (if (file-exists-p autos-path)
;          (setq verilog-library-directories (split-string (get-string-from-file autos-path) " "))
;        )
;      )
;    )
;  )
;
;(find-tec-autos)
