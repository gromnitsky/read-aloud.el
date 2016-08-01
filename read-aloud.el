;; read-aloud.el -- An interface to TTS engines -*- lexical-binding: t -*-

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

(defconst read-aloud--bufname "*Read-Aloud Log*")
(defconst read-aloud--prname "read-aloud")
(defconst read-aloud--pr nil)
(defconst read-aloud--queue '())
(defconst read-aloud--locked nil)

(defun read-aloud--log(msg &optional args)
  (let ((buf (get-buffer-create read-aloud--bufname)))
    (with-current-buffer buf
      (insert (format msg args))
      (insert "\n")
      )))

(defun read-aloud-test ()
  "Opens a new tmp buffer, inserts a string, tries to read it."
  (interactive)
  (let ((buf (get-buffer-create "*Read-Aloud Test*"))
	)

    (with-current-buffer buf
      (erase-buffer)
      (insert "Here lies the body of William Jay,
Who died maintaining his right of way--
He was right, dead right, as he speed along,
But he's just as dead as if he were wrong.")
      )
    (wordnut-u-switch-to-buffer buf)

    (goto-char (point-min))
    (while t
      (read-aloud-string (wordnut-u-line-cur))
      (line-move 1))

    ))

(defun read-aloud--cmd ()
  (plist-get (plist-get read-aloud-engines read-aloud-engine) 'cmd))

(defun read-aloud--args ()
  (plist-get (plist-get read-aloud-engines read-aloud-engine) 'args))

(defun read-aloud--valid-str-p (str)
  (and str (not (equal "" (string-trim str)))))

(cl-defun read-aloud-string (str)
  "Open an async process, feed its stdin with STR, wait for it to close."
  (unless (read-aloud--valid-str-p str) (cl-return-from read-aloud-string))

  (setq str (string-trim str))
  (let ((process-connection-type nil) ; (start-process) creates a pipe w/ this
	)

    (if read-aloud--locked
	(progn
	  (read-aloud--log "sentinel ADD=%s" str)
	  (setq read-aloud--queue
		(append read-aloud--queue (list str)))
	  )

      ;; else
      (setq read-aloud--locked t)
      (condition-case err
	  (setq read-aloud--pr
		(apply 'start-process read-aloud--prname nil
		       (read-aloud--cmd) (read-aloud-args)))
	(error
	 (setq read-aloud--locked nil)
	 (user-error "%s" (error-message-string err))
	 (cl-return-from read-aloud-string))
	)
      (set-process-sentinel read-aloud--pr 'read-aloud-sentinel)
      (process-send-string read-aloud--pr str)
      (process-send-eof read-aloud--pr)
      )))

(defun read-aloud-sentinel (process event)
  (let (str)

    (setq event (string-trim event))
    (if (equal event "finished")
	(progn
	  (setq str (pop read-aloud--queue))
	  (read-aloud--log "sentinel NEXT=%s" str)
	  (setq read-aloud--locked nil)
	  (read-aloud-string str))

      ;; else
      (setq read-aloud--queue '())
      (setq read-aloud--locked nil)
      (user-error "%s ended w/ the event: %s" process event)
      )))

(defun read-aloud-stop ()
  (interactive)
  (kill-process read-aloud--pr)
  )
