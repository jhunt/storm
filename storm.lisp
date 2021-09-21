(ql:quickload :cl-utilities)
(ql:quickload :chirp)
(use-package :cl-utilities)
(load "image.lisp")

;; tweets are represented by a single cons cell,
;; with the car populated with the tweet text and
;; the cdr populated with the media to attach

(defun make-tweet (text &optional media)
  "build a tweet object"
  (cons text media))

(defun build-tweet (words &optional media)
  (make-tweet
    (format nil "~{~A~^ ~}" words)
    media))

(defun is-tweet (tweet)
  (and (consp tweet)
       (stringp (car tweet))
       (or (null (cdr tweet))
           (pathnamep (cdr tweet)))))

(defun tweet-text (tweet)
  (car tweet))

(defun tweet-length (tweet)
  (length (tweet-text tweet)))

(defparameter *tweet-length* 280)
(defun tweet-too-long? (tweet &optional (max-len *tweet-length*))
  (or (> (tweet-length tweet) max-len)
      nil))


(defun tweet-media (tweet)
  (cdr tweet))

(defun tweet-has-media? (tweet)
  (not (null (tweet-media tweet))))

(defparameter *wpm* 10)

(defparameter *stat* (make-hash-table :test 'eq))
(defun stat (key)
  (multiple-value-bind (n ok)
    (gethash key *stat*)
    (if ok n 0)))

(defun (setf stat) (new-value key)
  (setf (gethash key *stat*) new-value))

(defun whats-up ()
  (format t "~d storms.~%" (stat 'storms))
  (format t "~d tweets sent.~%" (stat 'tweets))
  (format t "~d media uploaded.~%" (stat 'media)))

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

(defun read-file-as-lines (path)
  (with-open-file (in path)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defparameter *lisp-n* 1)
(defun stow-image (dir text)
  (let ((path (format nil "~A/lisp~D.png" dir *lisp-n*))
        (font (format nil "~A/mono.ttf" dir)))
    (incf *lisp-n*)
    (image::lispify path text :font font)
    path))

(defun expand-lisp-code (lines images &optional acc)
  (cond ((null lines)
         (values nil nil))
       ((equal (car lines) "---")
         (values (reverse acc)
                 (cdr lines)))
        (t
         (expand-lisp-code (cdr lines)
                           images
                           (cons (car lines) acc)))))

(defun expand-input (lines images)
  (cond ((null lines) nil)
        ((equal (car lines) "---(lisp)")
         (multiple-value-bind (img rest-of-tweet)
           (expand-lisp-code (cdr lines) images)
           (cons (format nil "<~A>" (stow-image images img))
                 (expand-input rest-of-tweet images))))
        (t (cons (car lines)
                 (expand-input (cdr lines) images)))))

(defun parse-input (path images)
  (let ((lines (read-file-as-lines path)))
    (format nil "~{~A~^ ~}"
            (expand-input lines images))))

(defun watch (dir images handler)
  "watch a directory for tweet files, and fire handler for each such file found"
  (loop
    (sleep 1)
    (loop for file in (tweet-files dir)
          do
          (progn
            (apply handler (list file (parse-input file images)))
            (delete-file file)))))


(defun disassemble-tweet (full)
  "parse a string into a list of space-separated words, suitable for re-assembling into tweets"
  (cull
    (mapcar #'drop-empty
            (split-sequence #\Space full))))

(defun media-reference? (s)
  "determines if a string word is a reference to media that should be uploaded to Twitter"
  (uiop:string-enclosed-p "<" s ">"))

(defun media-reference (s)
  "turns media references (like '<file.jpg>') into pathnames suitable for uploading to Twitter"
  (pathname (subseq s 1 (- (length s) 1))))

(defun fit-tweet (candidate words)
  "try to fit all of the words into a single tweet, using candidate as a starting list"
  (cond ((null words) (build-tweet (reverse candidate)))
        ((null (car words))
         (values (build-tweet (reverse candidate))
                 (cdr words)))
        ((media-reference? (car words))
         (values (build-tweet (reverse candidate)
                                 (media-reference (car words)))
                 (cull/cdr (cdr words))))
        ((tweet-too-long? (build-tweet (cons (car words) candidate)))
         (values (build-tweet (reverse candidate))
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
  (incf (stat 'tweets))
  (if (not reply-to)
      (incf (stat 'storms)))
  (cond (media (format t "... with attached media ~A~%" media)
               (incf (stat 'media))))
  (cond ((and media reply-to) (chirp:statuses/update-with-media status media :reply-to (chirp:id reply-to)))
        (media                (chirp:statuses/update-with-media status media))
        (reply-to             (chirp:statuses/update status :reply-to (chirp:id reply-to)))
        (t                    (chirp:statuses/update status))))

(defun nap (n)
  "loudly take a nap for n seconds"
  (format t "sleeping for ~ds...~%" n)
  (sleep n))

(defun tweet (text &key (wpm *wpm*))
  "tweet out text, as a tweet storm, at the given wpm typing speed"
  (let ((prev-status nil))
    (loop for the-tweet in (storm text)
          do (let* ((status  (tweet-text  the-tweet))
                    (media   (tweet-media the-tweet))
                    (naptime (seconds-to-type wpm status)))
               (nap naptime)
               (setf prev-status
                     (tweet-it status media prev-status))))))

(defun watch-and-tweet (&optional (dir "/tweets") (images "/img"))
  "main event loop - watch tweets/* for tweets and tweet them out"
  (watch dir images
         #'(lambda (file the-tweet)
             (declare (ignore file))
             (tweet the-tweet))))
