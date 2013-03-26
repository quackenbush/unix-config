(provide 'log)

(defgroup log nil
  "Major mode for editing log files."
  :prefix "log-"
  :group 'languages)

(defconst log-version "0.0.1"
  "Version number of log file mode.")

(defun log-version ()
  "Returns the value of the variable `log-version'."
  log-version)

(defconst log-font-lock-keywords
  (list
        ; Label
        '( "^:[^ :\t\n]+[ \t]*$" (0 font-lock-reference-face t))

        ; Numbers
        '( "0x[0-9a-f]+" (0 font-lock-constant-face t))
        '( " [0-9]+" (0 font-lock-constant-face t))

        ; user defining a variable set LIKE_THIS=something
        '( "^[Ss][Ee][Tt] \\([^ %=]+\\)" (1 font-lock-constant-face t))

        '( "^TRACE [^\n]*$" (0 font-lock-variable-name-face t))

        '( "^ERROR [^\n]*$" (0 font-lock-warning-face t))
        '( "^WARN [^\n]*$" (0 font-lock-warning-face t))
        )
  "Additional expressions to highlight in log mode.")


;;;###autoload
(defun log-mode ()
  "Major mode for log files.

Special commands:
Turning on log-mode calls the value of the variable `log-mode-hook',
if that value is non-nil.

Font lock mode:

Turning on font lock mode causes various log syntactic structures to be
highlighted. To turn this on whenever you visit a log file, add
the following to your .emacs file:
  \(add-hook 'log-mode-hook 'turn-on-font-lock\)
"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Log")
  (setq major-mode 'log-mode)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(log-font-lock-keywords t))
  (turn-on-font-lock)
  (run-hooks 'log-mode-hook))

;;; log.el ends here
