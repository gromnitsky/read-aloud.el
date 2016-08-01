;; read-aloud.el -- An interface to TTS engines -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)

(defconst read-aloud-engine 'spd-say)
(defconst read-aloud-engines
  '(spd-say				; Linux/FreeBSD only
    (cmd "spd-say" args ("-e" "-w"))
    flite				; Cygwin?
    (cmd "flite" args nil)
    jampal				; Windows
    (cmd "cscript" args "C:\\Program Files\\Jampal\\ptts.vbs")
    ))

(defconst read-aloud--logbufname "*Read-Aloud Log*")
(defconst read-aloud--prname "read-aloud")

(defconst read-aloud--c-pr nil)
(defconst read-aloud--c-buf nil)
(defconst read-aloud--c-bufpos nil)
(defconst read-aloud--c-locked nil)

(defun read-aloud--log(msg &optional args)
  (let ((buf (get-buffer-create read-aloud--logbufname)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert-before-markers (format (concat msg "\n") args))
      )))

(defun read-aloud-test ()
  "Opens a new tmp buffer, inserts a string, tries to read it."
  (interactive)
  (let ((buf (get-buffer-create "*Read-Aloud Test*")))

    (with-current-buffer buf
      (erase-buffer)
      (insert "Here lies the body of William Jay,
Who died maintaining his right of way--
He was right, dead right, as he speed along,
But he's just as dead as if he were wrong."))

    ;; show our logs
    (switch-to-buffer read-aloud--logbufname)
    (goto-char (point-max))

    (wordnut-u-switch-to-buffer buf)
    (goto-char (point-min))

    (setq read-aloud--c-buf buf)
    (setq read-aloud--c-bufpos 1)
    (read-aloud-buf)))

(defun read-aloud--cmd ()
  (plist-get (plist-get read-aloud-engines read-aloud-engine) 'cmd))

(defun read-aloud--args ()
  (plist-get (plist-get read-aloud-engines read-aloud-engine) 'args))

(defun read-aloud--valid-str-p (str)
  (and str (not (equal "" (string-trim str)))))

(defun read-aloud--reset()
  "Reset internal state."
  (setq read-aloud--c-pr nil)
  (setq read-aloud--c-buf nil)
  (setq read-aloud--c-bufpos nil)
  (setq read-aloud--c-locked nil)

  (read-aloud--log "RESET"))

(cl-defun read-aloud--string(str)
  "Open an async process, feed its stdin with STR."
  (unless (read-aloud--valid-str-p str) (cl-return-from read-aloud--string nil))

  (let ((process-connection-type nil)) ; (start-process) requires this

    (if read-aloud--c-locked (error "read-aloud is LOCKED"))

    (setq read-aloud--c-locked "buffer")
    (condition-case err
	(setq read-aloud--c-pr
	      (apply 'start-process read-aloud--prname nil
		     (read-aloud--cmd) (read-aloud--args)))
      (error
       (read-aloud--reset)
       (user-error "External TTS engine failed to start: %s"
		   (error-message-string err))
       (cl-return-from read-aloud--string nil)))

    (set-process-sentinel read-aloud--c-pr 'read-aloud--sentinel)
    (setq str (string-trim str))
    (read-aloud--log "Sending: %s" str)
    (process-send-string read-aloud--c-pr str)
    (process-send-eof read-aloud--c-pr)
    t
    ))

(defun read-aloud--sentinel (process event)
  (setq event (string-trim event))
  (if (equal event "finished")
      (progn
	(setq read-aloud--c-locked nil)
	;; FIXME
	(read-aloud-buf))

    ;; else
    (read-aloud--reset)
    (user-error "%s ended w/ the event: %s" process event)
    ))

(defun read-aloud-stop ()
  (interactive)
  (kill-process read-aloud--c-pr)
  (read-aloud--log "INTERRUPTED BY USER")
  )

(cl-defun read-aloud-buf()
  (interactive)

  (if read-aloud--c-locked
      (progn
	(read-aloud-stop)
	(cl-return-from read-aloud-buf)))

  (unless read-aloud--c-buf (setq read-aloud--c-buf (current-buffer)))
  (unless read-aloud--c-bufpos (setq read-aloud--c-bufpos (point)))

  (let (line)
    (with-current-buffer read-aloud--c-buf
      (if (eobp)
	  (progn
	    (read-aloud--log "END OF BUFFER")
	    (read-aloud--reset)
	    (cl-return-from read-aloud-buf)))

      (goto-char read-aloud--c-bufpos)
      (setq line (wordnut-u-line-cur))

      (read-aloud--string line)

      (ignore-errors (line-move 1))
      (setq read-aloud--c-bufpos (point))
      )))

(provide 'read-aloud)
