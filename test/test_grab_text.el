:; exec emacs -Q --script "$0" -- "$@" # -*- lexical-binding: t -*-

(setq
 vc-handled-backends nil
 argv (cdr argv))		       ; remove '--' from CL arguments

(push (concat (file-name-directory load-file-name) "/..") load-path)

(require 'read-aloud)

(ert-deftest empty-buffer()
  (with-temp-buffer
    (should-not (read-aloud--grab-text (buffer-name) 1) )
    ))

(ert-deftest buffer-with-spaces()
  (with-temp-buffer
    (insert " \n\t\n\n\n\n")
    (should-not (read-aloud--grab-text (buffer-name) 1) )
    ))

(ert-deftest a-long-line()
  (with-temp-buffer
    (let ((read-aloud-max 10))
      (insert (make-string 10 ?a))
      (insert (make-string 10 ?b))
      (should (equal '(text "aaaaaaaaaa" beg 1 end 11)
		     (read-aloud--grab-text (buffer-name) 1) ))
      )))

(ert-deftest find-a-space()
  (with-temp-buffer
    (let ((read-aloud-max 7))
      (insert (make-string 5 ?a))
      (insert " ")
      (insert (make-string 5 ?b))
;      (print (buffer-string))
      (should (equal '(text "aaaaa" beg 1 end 6)
		     (read-aloud--grab-text (buffer-name) 1) ))
      (should (equal '(text "bbbbb" beg 7 end 12)
		     (read-aloud--grab-text (buffer-name) 6) ))
      (should-not (read-aloud--grab-text (buffer-name) 12) )
      )))

(ert-deftest split1()
  (with-temp-buffer
    (let ((read-aloud-max 100))
      (insert "-aaaaa bbbbb\n\n12345")
      (should (equal '(text "aaaaa bbbbb" beg 2 end 13)
		     (read-aloud--grab-text (buffer-name) 1) ))
      (should (equal '(text "12345" beg 15 end 20)
		     (read-aloud--grab-text (buffer-name) 13) ))
      (should-not (read-aloud--grab-text (buffer-name) 20) )
      )))

(ert-deftest split1-win()
  (with-temp-buffer
    (let ((read-aloud-max 100))
      (insert "-aaaaa bbbbb\r\n\r\n12345")
      (should (equal '(text "aaaaa bbbbb" beg 2 end 13)
		     (read-aloud--grab-text (buffer-name) 1) ))
      (should (equal '(text "12345" beg 17 end 22)
		     (read-aloud--grab-text (buffer-name) 13) ))
      (should-not (read-aloud--grab-text (buffer-name) 22) )
      )))

(ert-deftest split2()
  (with-temp-buffer
    (let ((read-aloud-max 100))
      (insert ",aaaaa bbbbb, ccccc,")
      (should (equal '(text "aaaaa bbbbb" beg 2 end 13)
		     (read-aloud--grab-text (buffer-name) 1) ))
      (should (equal '(text "ccccc" beg 15 end 20)
		     (read-aloud--grab-text (buffer-name) 13) ))
      (should-not (read-aloud--grab-text (buffer-name) 20) )
      )))

(ert-deftest split3()
  (with-temp-buffer
    (let ((read-aloud-max 100))
      (insert " next.  First")
      (should (equal '(text "next" beg 2 end 6)
		     (read-aloud--grab-text (buffer-name) 1) ))
      (should (equal '(text "First" beg 9 end 14)
		     (read-aloud--grab-text (buffer-name) 6) ))
      (should-not (read-aloud--grab-text (buffer-name) 14) )
      )))



(ert-run-tests-batch-and-exit (car argv))
