(ql:quickload :cl-utilities)
(ql:quickload :chirp)
(use-package :cl-utilities)

(defparameter *settings* (make-hash-table :test 'eq))
(defparameter *settings-path* #P"/etc/storm/settings")
(defun settings (&optional path)
  (if (not (null path))
    (setf *settings-path* path))
  (setf *settings* (make-hash-table :test 'eq))
  (if (probe-file *settings-path*)
    (with-open-file (in *settings-path*)
      (mapcar (lambda (v) (setf (gethash (car v) *settings*) (cdr v)))
              (read in)))))

(defun setting (key)
  (gethash key *settings*))

(defun (setf setting) (new-value key)
  (setf (gethash key *settings*) new-value)
  (if *settings-path*
      (with-open-file (out *settings-path*
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :overwrite)
        (format out "~S~%"
                (loop for k being each hash-key of *settings*
                      using (hash-value v)
                      collect (cons k v)))))
  new-value)

(defun save-chirp-auth (path)
  (settings path)
  (setf (setting 'api-key)       chirp:*oauth-api-key* )
  (setf (setting 'api-secret)    chirp:*oauth-api-secret*)
  (setf (setting 'access-token)  chirp:*oauth-access-token* )
  (setf (setting 'access-secret) chirp:*oauth-access-secret*))

(defun restore-chirp-auth (&optional path)
  (settings path)
  (setf chirp:*oauth-api-key*       (setting 'api-key))
  (setf chirp:*oauth-api-secret*    (setting 'api-secret))
  (setf chirp:*oauth-access-token*  (setting 'access-token))
  (setf chirp:*oauth-access-secret* (setting 'access-secret)))

(defun cull/cdr (lst)
  "culls returns the first cdr of lst that doesn't start with nil"
  (cond ((null lst) nil)
        ((null (car lst))
         (cull/cdr (cdr lst)))
        (t lst)))

(defun cull (lst)
  "culls nil values from a list, replacing contiguous runs of more than one nil with a single nil"
  (cond ((null lst) nil)
        ((null (car lst))
         (cons nil
               (cull (cull/cdr lst))))
        (t
         (cons (car lst)
               (cull (cdr lst))))))

(defun drop-empty (w)
  "returns w, unless w is the empty string, in which case it returns nil"
  (if (eq (length w) 0)
    nil
    w))

(defun tweet-files (in-dir)
  "find and return list of pathnames of files that contain tweets"
  (directory (format nil "~A/*.tweet" in-dir)))

(defun read-file (path)
  "read all of a tweet file into memory, replacing newlines with spaces"
  (with-open-file (in path)
    (format nil "~{~A ~}"
            (loop for line = (read-line in nil)
                  while line
                  collect line))))

(defun watch (dir handler)
  "watch a directory for tweet files, and fire handler for each such file found"
  (loop
    (format t "sleeping for 1s~%")
    (sleep 1)
    (loop for file in (tweet-files dir)
          do
          (progn
            (apply handler (list file (read-file file)))
            (delete-file file)))))


(defun disassemble-tweet (full)
  "parse a string into a list of space-separated words, suitable for re-assembling into tweets"
  (cull
    (mapcar #'drop-empty
            (split-sequence #\Space full))))

(defun assemble-tweet (words &optional image)
  "assemble a list of words and an optional media (or media list) into a tweet cons"
  (cons (format nil "~{~A~^ ~}" words)
        image))

(defvar *tweet-length* 280)
(defun tweet-too-long? (tweet)
  "determines if a string is too long to tweet, according to *tweet-length*"
  (or (> (length tweet) *tweet-length*)
      nil))

(defun media-reference? (s)
  "determines if a string word is a reference to media that should be uploaded to Twitter"
  (uiop:string-enclosed-p "<" s ">"))

(defun media-reference (s)
  "turns media references (like '<file.jpg>') into pathnames suitable for uploading to Twitter"
  (pathname (subseq s 1 (- (length s) 1))))

(defun fit-tweet (candidate words)
  "try to fit all of the words into a single tweet, using candidate as a starting list"
  (cond ((null words) (assemble-tweet (reverse candidate)))
        ((null (car words))
         (values (assemble-tweet (reverse candidate))
                 (cdr words)))
        ((media-reference? (car words))
         (values (assemble-tweet (reverse candidate)
                                 (media-reference (car words)))
                 (cdr words)))
        ((tweet-too-long? (assemble-tweet (cons (car words) candidate)))
         (values (assemble-tweet (reverse candidate))
                 words))
        (t (fit-tweet (cons (car words) candidate)
                            (cdr words)))))

(defun fit-tweets (tweets words)
  "convert a list of words into a smaller list of tweets, pursuant to length limitations"
  (cond ((null words) (reverse tweets))
        (t (multiple-value-bind (tweet remaining)
             (fit-tweet nil words)
             (fit-tweets (cons tweet tweets)
                         remaining)))))

(defun storm (text)
  "turn a source text into a list of tweets and their associated media references (if any)"
  (let* ((*tweet-length* 200)
         (tweets (fit-tweets nil
                    (disassemble-tweet text)))
         (n 0)
         (m (length tweets)))
    (if (eq m 1)
      tweets
      (mapcar #'(lambda (tweet)
                  (incf n)
                  (cons
                    (format nil "~A~%~%~D/~D" (car tweet) n m)
                    (cdr tweet)))
              tweets))))

(defun raw-words (text)
  "split text into component words, ignoring all spaces"
  (remove "" (split-sequence #\Space text)))

(defun seconds-to-type (wpm text)
  "determine how many seconds it would take to type out text, given a wpm typing speed"
  (floor (* 60 (/ (length (raw-words text))
               wpm))))

(defun dont-tweet-it (status media reply-to)
  "pretend to tweet, but don't -- useful for testing locally"
  (format t "[dry run] ~S~%" `(status ,status media ,media reply-to ,reply-to)))

(defun tweet-it (status media reply-to)
  "tweet status updates, as a thread, with attached media (if any)"
  (format t "tweeting: ~A~%" status)
  (if media (format t "... with attached media ~A~%" media))
  (cond ((and media reply-to) (chirp:statuses/update-with-media status media :reply-to (chirp:id reply-to)))
        (media                (chirp:statuses/update-with-media status media))
        (reply-to             (chirp:statuses/update status :reply-to (chirp:id reply-to)))
        (t                    (chirp:statuses/update status))))

(defun nap (n)
  "loudly take a nap for n seconds"
  (format t "sleeping for ~ds...~%" n)
  (sleep 0))

(defun tweetstorm (text &key (wpm 80))
  "tweet out text, as a tweet storm, at the given wpm typing speed"
  (let ((prev-status nil))
    (loop for tweet in (storm text)
          do (let* ((status  (car tweet))
                    (media   (cdr tweet))
                    (naptime (seconds-to-type wpm status)))
               (nap naptime)
               (setf prev-status
                     (tweet-it status media prev-status))))))


;(defun watch-demo (dir)
;  (watch dir #'(lambda (file contents)
;                 (format t "... processing ~A...~%" file)
;                 (format t "---~%~A~%~%~%===~%" contents))))

(defun watch-and-tweet (&optional (dir "/tweets"))
  "main event loop - watch tweets/* for tweets and tweet them out"
  (watch dir
         #'(lambda (file tweet)
             (declare (ignore file))
             (tweetstorm tweet))))
