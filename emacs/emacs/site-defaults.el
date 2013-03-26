(defun configure-c-mode()
  (c-set-offset 'case-label 4)
)

(defun insert-pdb ()
  (interactive)
  (insert "import pdb; pdb.set_trace()\n")
  (indent-for-tab-command)
  )

(defun setup-tab-completion()
  ;(setq hippie-expand-dabbrev-as-symbol 1)
  (setq hippie-expand-verbose nil)

  ;(require 'thingatpt)

  (defun indent-or-complete ()
    "Complete if point is at end of a word, otherwise indent line."
    (interactive)
    (if (and (looking-at "\\>") (not (looking-at "^")))
        (dabbrev-expand nil)
  ;      (hippie-expand nil)
      (indent-for-tab-command)
      )
  )

  ;(global-set-key (kbd "TAB") 'indent-or-complete)

  (add-hook 'find-file-hooks
            (function (lambda ()
                        (local-set-key (kbd "TAB") 'indent-or-complete)
                        (light-symbol-mode)
                        ))
            )

  (add-hook 'python-mode-hook
            (function (lambda ()
                        (local-set-key (kbd "M-p") 'insert-pdb)
                        (modify-syntax-entry ?_ "_")
                        )
                      )
            )

  (defun org-or-complete ()
    "Complete if point is at end of a word, otherwise indent line."
    (interactive)
    (if (and (looking-at "\\>") (not (looking-at "^")))
        (org-cycle)
  ;      (hippie-expand nil)
      (indent-for-tab-command)
      )
  )

  ;(add-hook 'org-mode-hook
  ;          (lambda () (local-set-key (kbd "TAB") 'org-or-complete)
  ;            )
  ;          )
  (add-hook 'c-mode-hook
            (lambda ()
              (define-key c-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
              ;(define-key c-mode-map (kbd "M-`") 'ff-find-other-file)
              )
            )

  (add-hook 'c++-mode-hook
            (lambda ()
              (define-key c++-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
              ;(define-key c++-mode-map (kbd "M-`") 'ff-find-other-file)
              )
            )

  (add-hook 'c-mode-hook 'configure-c-mode)
  (add-hook 'c++-mode-hook 'configure-c-mode)


  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (define-key emacs-lisp-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
              ;(define-key emacs-lisp-mode-map (kbd "tab") 'dabbrev-expand)
              )
            )

  (add-hook 'verilog-mode-hook
            (function (lambda ()
                        (local-set-key (kbd ";") 'self-insert-command)
                        (define-key verilog-mode-map (kbd "RET") 'newline-and-indent)
                        )
                      )
            )

  (global-set-key (kbd "M-`") 'ff-find-other-file)
  (global-set-key (kbd "S-TAB") 'dabbrev-completion)

  (setq ff-search-directories (append '(".")
                                      (list
                                       (concat env-root "/../../common/*/include")
                                       (concat env-root "/*/tb")
                                       (concat env-root "/*/rtl")
                                       (concat env-root "/*/tests/lib")
                                       (concat env-root "/build/*")
                                       )
                                      )
        )

  (require 'find-file)

  (setq cc-other-file-alist
        (quote (
                ("\\.v$" (".vi" ".vh"))
                ("\\.v[ih]" (".v"))

                ("\\.vr[hi]$" (".vr"))
                ("\\.vr$" (".vrh"))

                ("\\.sv$" (".svh"))
                ("\\.sv[hi]$" (".sv"))

                ("\\.cc$" (".hh" ".h"))
                ("\\.hh$" (".cc" ".C"))
                ("\\.c$" (".h"))
                ("\\.h$" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))
                ("\\.C$" (".H" ".hh" ".h"))
                ("\\.H$" (".C" ".CC"))
                ("\\.CC$" (".HH" ".H" ".hh" ".h"))
                ("\\.HH$" (".CC"))
                ("\\.cxx$" (".h" ".hh"))
                ("\\.cpp$" (".h" ".hh" ".hpp"))
                ("\\.hpp$" (".cpp"))

                )
               )
        )

  (setq ff-special-constructs (append
                               '(
                                 ("^ *\[#`]\\s *\\(include\\|import\\)\\s +[<\"]\\(.*\\)[>\"]" .
                                  (lambda ()
                                    (setq fname (buffer-substring (match-beginning 2) (match-end 2))))))
                               ff-special-constructs
                               )
        )

)

(setq build-command "build")
(defun setup-model-compilation()
  (setq verilog-set-compile-command build-command)
  (setq compilation-scroll-output 1)

  ; Find file at point
  (require 'find-this-file)

  ; Read-only build/default hook
  (add-hook 'find-file-hooks
            (lambda ()
              (if (and
                   (string-match "/build/" buffer-file-name)
                   (not (string-match "/src/build/" buffer-file-name)) ; tools/src/build is OK
                   (not (file-symlink-p buffer-file-name))
                   )
                       (toggle-read-only 1))
              )
  )

  ;; Some code that will make it so the background color of the lines
  ;; that gcc found errors on, should be in another color.

  (require 'custom)

  (defvar all-overlays ())

  (defun delete-this-overlay(overlay is-after begin end &optional len)
    (delete-overlay overlay)
    )

  (defun highlight-current-line()
    (interactive)
    (setq current-point (point))
    (beginning-of-line)
    (setq beg (point))
    (forward-line 1)
    (setq end (point))
    ;; Create and place the overlay
    (setq error-line-overlay (make-overlay 1 1))

    ;; Append to list of all overlays
    (setq all-overlays (cons error-line-overlay all-overlays))

    (overlay-put error-line-overlay
                 'face '(background-color . "firebrick"))
    (overlay-put error-line-overlay
                 'modification-hooks (list 'delete-this-overlay))
    (move-overlay error-line-overlay beg end)
    (goto-char current-point)
    )

  (defun delete-all-overlays()
    (while all-overlays
      (delete-overlay (car all-overlays))
      (setq all-overlays (cdr all-overlays))
      )
    )

  (defun highlight-error-lines(compilation-buffer, process-result)
    (interactive)
    (delete-all-overlays)
    (if (string-match "finished" process-result)

        (let ((buf (get-buffer "*compilation*")))
          (when buf
            (delete-window (get-buffer-window buf))
            (kill-buffer buf)
            )

          (message "Success")

          )

      (progn
        (condition-case nil
            (while t
              (next-error)
              (highlight-current-line)
              )
          (error nil))
        ; Select the first error
        (first-error)
        )
      )
    )

  (setq compilation-finish-function 'highlight-error-lines)
)

(defun match-paren (arg)
  "Go to the matching paren"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))

        (t (error "%s" "Not on a paren, brace, or bracket"))

        )
)

(defun switch-to-other-buffer ()
  "Switch to other-buffer in current window"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun backward-kill-line ()
  "Kill from the beginning of current line to point."
  (interactive)
  (kill-line 0))

(defun strip (s)
  "Return a new string derived by stripping whitespace surrounding string
S."
  (interactive "sString: ")
  (let ((r s))
    (when (string-match "^[ \t\n]+" r)
      (setq r (substring r (match-end 0))))
    (when (string-match "[ \t\n]+$" r)
      (setq r (substring r 0 (match-beginning 0))))
    r))

(defun compile-model ()
  (interactive)
  (setq local-env-root (strip (shell-command-to-string "env_root")))
  ; TODO: exit if model root not found
  (if (string-match "ERROR" local-env-root)
      (error local-env-root)
    (progn
      (setq compile-command build-command)
      (setq compilation-search-path (list local-env-root))
      )
    )
  ;    (progn
;  (setq compilation-finish-function 'highlight-error-lines)

  (recompile)
)

(defun lint ()
  "Run the linter (duh!)"
  (interactive)
  (if (null buffer-file-name)
      (error "Null buffer")
    (progn
      (setq compile-command
            (concat "lint " buffer-file-name)
            )
      (recompile)

      )
    )
  )

(defun load-defaults ()

  ; Disable auto-saving
  (setq auto-save-default nil)

  (setq load-path (cons "~/emacs/org/lisp" load-path))

  (require 'org-install)

  ;; The following lines are always needed.  Choose your own keys.
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)

  ;(require 'org-install)
; Session support
;(require 'session)
;(add-hook 'after-init-hook 'session-initialize)

; Make "yes or no" => "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

; Redo support
(require 'redo)
(global-set-key "\M-?" 'redo)

(global-set-key (kbd "C-x p") 'bury-buffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

; Fix retarded page-up/down in emacs
(require 'pager)
(global-set-key "\C-v"   'pager-page-down)
(global-set-key [next]    'pager-page-down)
(global-set-key "\ev"   'pager-page-up)
(global-set-key [prior]   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)
(global-set-key '[M-kp-2]  'pager-row-down)

(global-set-key "\M-n" 'backward-kill-line)

(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(font-lock-warning-face ((t (:foreground "red2" :weight bold)))))

(global-set-key "\M-=" 'match-paren)
(setq auto-revert-verbose nil)
(setq tags-revert-without-query t)

(global-set-key (kbd "<C-tab>") 'switch-to-other-buffer)

; ASM configuration
;(setq asm-comment-char ?\#)
;(setq tab-stop-list '(4 8 12 16 20 24 28 32))
(setq-default tab-width 4)

(setq c-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))
(setq c++-mode-hook
    (function (lambda ()
                (setq indent-tabs-mode nil)
                (setq c-indent-level 4))))

(setq c-basic-offset 4)

(setq c-default-style "bsd"
      c-basic-offset 4)

(setq auto-mode-alist (cons '("\\.B$" . asm-mode) auto-mode-alist))

(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))


; Set the window title bar to the current buffer's name (ie. the filename)
(setq initial-frame-alist '((name . nil) (minibuffer . t))
      frame-title-format '("%b"))

(setq frame-title-format "Emacs: %b")
(setq icon-title-format "%b")

(setq default-frame-alist
      '(
         (width . 120) (height . 68)
       )
)

(global-set-key "\M-;" 'replace-regexp)

;(global-set-key "\C-h" 'delete-backward-char)
;(global-set-key (kbd "DEL") 'delete-char)

; Fix the Putty home/end mappings
(global-set-key "\e[h" 'beginning-of-line)
(global-set-key "\e[1~" 'beginning-of-line)
(global-set-key [kp-7] 'end-of-line)
(global-set-key "\eOw" 'end-of-line)
(global-set-key [select] 'end-of-line)

; Turn on auto-reverting
(global-auto-revert-mode 1)

; Turn on column numbers
(column-number-mode 1)

; Set the foreground and background colors (hacker-style baby!)
;(require 'font-lock)
(set-foreground-color "LightYellow2")
(set-background-color "black")

(global-set-key "\M-g" 'goto-line)

;(fast-lock-mode t)
(global-font-lock-mode t)


; Highlight changes mode
(global-set-key "\M-'" 'highlight-changes-mode)

; Kill other windows with meta -
(global-set-key [(meta -)] 'delete-other-windows)

;(setq printer-name "//big_brother/binary_p1")
(fset 'detab
   [?\C-x ?h ?\M-x ?u ?n ?t ?a ?b ?i ?f ?y return])

(put 'narrow-to-region 'disabled nil)

; Python
;(autoload 'python-mode "python-mode" "Python editing mode." t)
;(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
;(setq file-coding-system-alist
;       (append (list (cons "\\.py$" (cons 'emacs-mule-unix 'emacs-mule-unix)))
;              file-coding-system-alist))
;(setq interpreter-mode-alist
;      (cons '("python" . python-mode)
;            interpreter-mode-alist))
;(setq py-python-command "python")

;;;;;(load "nxml-mode/rng-auto.el")

;(setq auto-mode-alist
;      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
;            auto-mode-alist))
;
;(add-hook 'nxml-mode-hook
;          (function (lambda ()
;                      (local-set-key (kbd "RET") 'newline-and-indent)))
;)

; Makefile
;;;;(setq auto-mode-alist
;;;;      (cons '("\\.\\(make\\|rules\\)$" . makefile-mode) auto-mode-alist))

;(defun accurev-history ()
;  "Get the history of the current file"
;  (interactive "*")
;  (shell-command (concat "accurev hist -k keep " (buffer-name (current-buffer))))
;)
;
;(defun accurev-annotate ()
;  "Annotate the current file"
;  (interactive "*")
;  (shell-command (concat "accurev annotate " (buffer-name (current-buffer))))
;)
;
;(defun accurev-keep ()
;  "Keep the current file"
;  (interactive)
;  (shell-command (concat "accurev keep " (buffer-name (current-buffer))))
;)
;
;(defun accurev-stat ()
;  "Get the status of the current file"
;  (interactive)
;  (shell-command (concat "accurev stat " (buffer-name (current-buffer))))
;)
;
;(defun accurev-diff ()
;  "Get the diff of the current file from Accurev"
;  (interactive)
;  (shell-command (concat "accurev diff -b " (buffer-name (current-buffer))))
;)
;
;(defun accurev-cat ()
;  "Display the backed version of the current file"
;  (interactive)
;  (shell-command (concat "accurev cat " (buffer-name (current-buffer))))
;)

(setq env-root (strip (shell-command-to-string "env_root")))
;(setq tags-path (strip (shell-command-to-string "tags_path")))
(setq tags-path (concat env-root "/build/TAGS"))

(if (file-exists-p tags-path)
  (visit-tags-table tags-path)
)

; Log files
(autoload 'log-mode "log-mode" "Log Mode" t)
(setq auto-mode-alist (cons '("\\.log\\'" . log-mode) auto-mode-alist))

(autoload 'verilog-mode "verilog-mode" "Verilog Mode" t)
(setq auto-mode-alist (cons '("\\.v\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.x\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vpp\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.v.em\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vi\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.svi" . verilog-mode) auto-mode-alist))

(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(verilog-align-ifelse t)
 '(verilog-indent-begin-after-if nil)
 '(verilog-auto-endcomments nil)
 '(verilog-auto-indent-on-newline nil)
 '(verilog-auto-newline nil)
 '(verilog-auto-read-includes nil)
 '(verilog-case-indent 4)
 '(verilog-cexp-indent 4)
 '(verilog-indent-level 4)
 '(verilog-indent-level-behavioral 4)
 '(verilog-indent-level-declaration 4)
 '(verilog-indent-level-module 4)
 '(verilog-compiler 'compile-command)
 '(case-fold-search t)
 '(tags-case-fold-search nil)
 '(nxml-child-indent 4)
 '(nxml-outline-child-indent 4)
 '(nxml-slash-auto-complete-flag t)
)

(global-set-key (kbd "M-<return>") 'complete-tag)

(compilation-minor-mode)

(global-set-key [f6] 'compile-model)
(global-set-key [f7] 'previous-error)
(global-set-key [f8] 'next-error)
;; to add a path to the ltf-find-this-file-list
;(setq ltf-find-this-file-list (cons ".." ltf-find-this-file-list ))

(setup-tab-completion)
;(setup-model-compilation)

(require 'ido)
(ido-mode t)

(load "light-symbol")

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

); end of load-defaults()
