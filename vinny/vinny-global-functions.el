
(defun vinny-dont-kill-emacs ()
  "Prevent key combinations from killing emacs"
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))

(defun vinny-show-buffers-and-focus-buffers ()
  (interactive)
  (buffer-menu))

(setq vinny-current-theme-index 0)

(defun vinny-theme-rotator()
  "Disable the current theme(s) and load the next one in (custom-available-themes)"
  (interactive)
  (mapcar 'disable-theme custom-enabled-themes)
  (load-theme (nth vinny-current-theme-index (custom-available-themes)) 1)
  (message "Loading theme %S" (nth vinny-current-theme-index (custom-available-themes)))
  (setq vinny-current-theme-index (+ vinny-current-theme-index 1))
  (when (equal vinny-current-theme-index (length (custom-available-themes)))
	(setq vinny-current-theme-index 0)
  )
)

(defun vinny-disable-all-themes ()
  (interactive)
  (mapcar 'disable-theme (custom-available-themes))
)

(provide 'vinny-global-functions)
