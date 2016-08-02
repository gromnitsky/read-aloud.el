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




(defvar read-aloud-word-hist '())	; (*-current-word) uses it
(defconst read-aloud--logbufname "*Read-Aloud Log*")
(defconst read-aloud--prname "read-aloud")

;; this should be in cl-defstruct
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

(cl-defun read-aloud--string(str source)
  "Open an async process, feed its stdin with STR. SOURCE is an
arbitual string like 'buffer', 'word' or 'selection'."
  (unless (read-aloud--valid-str-p str) (cl-return-from read-aloud--string nil))

  (let ((process-connection-type nil)) ; (start-process) requires this

    (if read-aloud--c-locked (error "read-aloud is LOCKED"))

    (setq read-aloud--c-locked source)
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
    (read-aloud--log "Sending: `%s`" str)
    (process-send-string read-aloud--c-pr str)
    (process-send-eof read-aloud--c-pr)
    t
    ))

(defun read-aloud--sentinel (process event)
  (let ((source read-aloud--c-locked))

    (setq event (string-trim event))
    (if (equal event "finished")
	(progn
	  (read-aloud--overlay-rm)
	  (setq read-aloud--c-locked nil)
	  (cond
	   ((equal source "buffer") (read-aloud-buf))
	   ((equal source "word") t)	  ; do nothing
	   ((equal source "selection") t) ; do nothing
	   (t (error "unknown source: %s" source))) )

      ;; else
      (read-aloud--reset)
      (user-error "%s ended w/ the event: %s" process event)
      )))

(defun read-aloud-stop ()
  (interactive)
  (kill-process read-aloud--c-pr)
  (read-aloud--log "INTERRUPTED BY USER")
  )

(cl-defun read-aloud-buf()
  "Read the current buffer, highlighting words along the
read. Run it again to stop reading."
  (interactive)

  (when read-aloud--c-locked
    (read-aloud-stop)
    (cl-return-from read-aloud-buf))

  (unless read-aloud--c-buf (setq read-aloud--c-buf (current-buffer)))
  (unless read-aloud--c-bufpos (setq read-aloud--c-bufpos (point)))

  (let (tb)
    (with-current-buffer read-aloud--c-buf
      (when (eobp)
	(read-aloud--log "END OF BUFFER")
	(read-aloud--reset)
	(cl-return-from read-aloud-buf))

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

      (read-aloud--string (plist-get tb 'text) "buffer")

      (setq read-aloud--c-bufpos (plist-get tb 'end))
      )))

(cl-defun read-aloud--grab-text(buf point)
  "Return (text \"omglol\" beg 10 end 20) plist or nil on
eof. BUF & POINT are the starting location for the job."
  (let (max t2 p pstart chunks pchunk)

    (with-current-buffer buf
      (save-excursion
	(goto-char point)
	(skip-chars-forward "[\\-,.:!;[:space:]\r\n]")

	(setq max (+ (point) read-aloud-max))
	(if (> max (point-max)) (setq max (point-max)))
	(setq t2 (buffer-substring-no-properties (point) max))

	(if (string-empty-p (string-trim-right t2))
	    ;; we have spaces at the end of buffer, there is nothing to grab
	    (cl-return-from read-aloud--grab-text nil))

	(setq pstart (point))

	(unless (= max (point-max))
	  (progn
	    ;; look for the 1st non-space in `t` from the end & cut
	    ;; off that part
	    (setq p (string-match "[[:space:]\r\n]" (reverse t2)) )
	    (if p (setq t2 (substring t2 0 (- (length t2) p 1))) )))

	(setq chunks (split-string t2 "[,.:!;]\\|-\\{2,\\}\\|\n\\{2,\\}" t))
	(if chunks
	    (progn
	      (search-forward (car chunks))
	      (setq pchunk (point))
	      (search-backward (car chunks))
	      (setq pstart (point))
	      (setq t2 (buffer-substring-no-properties pstart pchunk)) ))

	(read-aloud--log "text grab: `%s`" t2)
	`(text ,t2
	       beg ,pstart
	       end ,(+ pstart (length t2)))
	))))

(cl-defun read-aloud-current-word()
  "Pronounce a word under the pointer. If under there is rubbish,
ask user for an additional input."
  (interactive)
  (when read-aloud--c-locked
    (read-aloud-stop)
    (cl-return-from read-aloud-current-word))

  (let ((word (current-word)) )

    (unless (string-match "[[:alnum:]]" word)
      ;; maybe we should share the hist list w/ `wordnut-completion-hist`?
      (setq word (read-string "read aloud: " word 'read-aloud-word-hist)) )

    (read-aloud--string word "word")
    ))

(cl-defun read-aloud-selection(beg end)
  "Pronounce all that is selected."
  (interactive "r")

  (when read-aloud--c-locked
    (read-aloud-stop)
    (cl-return-from read-aloud-selection))

  (unless (use-region-p) (user-error "No selection"))
  (read-aloud--string (buffer-substring-no-properties beg end) "selection"))



(provide 'read-aloud)
