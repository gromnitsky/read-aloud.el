;; read-aloud.el -- An interface to TTS engines -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)

(defconst read-aloud-engine 'flite)
(defconst read-aloud-engines
  '(spd-say				; Linux/FreeBSD only
    (cmd "spd-say" args ("-e" "-w"))
    flite				; Cygwin?
    (cmd "flite" args nil)
    jampal				; Windows
    (cmd "cscript" args "C:\\Program Files\\Jampal\\ptts.vbs")
    ))

(defconst read-aloud-max 160)		; chars
(defface read-aloud-text-face '((t :inverse-video t))
  "For highlighting the text that is being read")




(defconst read-aloud--logbufname "*Read-Aloud Log*")
(defconst read-aloud--prname "read-aloud")

(defconst read-aloud--c-pr nil)
(defconst read-aloud--c-buf nil)
(defconst read-aloud--c-bufpos nil)
(defconst read-aloud--c-locked nil)
(defconst read-aloud--c-overlay nil)

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

(defun read-aloud--overlay-rm()
  (if read-aloud--c-overlay
      (progn
	(delete-overlay read-aloud--c-overlay)
	(setq read-aloud--c-overlay nil))))

(defun read-aloud--reset()
  "Reset internal state."
  (setq read-aloud--c-pr nil)
  (setq read-aloud--c-buf nil)
  (setq read-aloud--c-bufpos nil)
  (setq read-aloud--c-locked nil)

  (read-aloud--overlay-rm)
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
    (setq str (concat (string-trim str) "\n"))
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
	(read-aloud--overlay-rm)
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

  (let (tb)
    (with-current-buffer read-aloud--c-buf
      (if (eobp)
	  (progn
	    (read-aloud--log "END OF BUFFER")
	    (read-aloud--reset)
	    (cl-return-from read-aloud-buf)))

      (goto-char read-aloud--c-bufpos)
      (setq tb (read-aloud--grab-text read-aloud--c-buf (point)))
      (unless tb
	  (progn
	    (read-aloud--log "SPACES AT THE END OF BUFFER")
	    (read-aloud--reset)
	    (cl-return-from read-aloud-buf)))

      ;; highlight text
      (setq read-aloud--c-overlay
	    (make-overlay (plist-get tb 'beg) (plist-get tb 'end)))
      (overlay-put read-aloud--c-overlay 'face 'read-aloud-text-face)

      (read-aloud--string (plist-get tb 'text))

      (setq read-aloud--c-bufpos (plist-get tb 'end))
      )))

(cl-defun read-aloud--grab-text(buf point)
  "Return (text \"omglol\" beg 10 end 20) plist or nil on
eof. BUF & POINT are the starting location for the job."
  (let ((sep-re "[,.:!;]\\|-\\{2,\\}")
	t2 raw max p)

    (with-current-buffer buf
      (goto-char point)
      ;; move to the first non-space char
      (skip-chars-forward "[[:space:]\n]")

      (setq max (+ (point) read-aloud-max 1))
      (if (> max (point-max)) (setq max (point-max)))
      (setq t2 (buffer-substring-no-properties (point) max))

      (if (string-empty-p (string-trim-right t2))
	  ;; we have spaces at the end of buffer, there is nothing to grab
	  (cl-return-from read-aloud--grab-text nil))

      (if (= max (point-max))
	  (progn
	    (read-aloud--log "text grab: `%s'" t2)
	    (cl-return-from read-aloud--grab-text
	      `(text ,t2
		     beg ,(point)
		     end ,max) )))

      ;; look for the 1st non-space in `t` from the end & cut off that part
      (setq p (string-match "[[:space:]\n]" (reverse t2)) )
      (if p (setq t2 (substring t2 0 (- (length t2) p 1))))
      (setq raw t2)
      (read-aloud--log "text grab raw: `%s'" raw)

      ;; cut off everything after the punctuation
      (setq p (string-match sep-re (reverse t2) ))
      (if p (setq t2 (substring t2 0 (- (length t2) p 1))))

      ;; in case we have something like --------------------
      (if (string-match (concat "^\\(" sep-re "\\)+$") t2) (setq t2 raw))

      (read-aloud--log "text grab: `%s'" t2)
      `(text ,t2
	     beg ,(point)
	     end ,(+ (point) (length t2) 1))
      )))

(provide 'read-aloud)
