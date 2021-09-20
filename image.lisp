(defpackage :image
  (:use :cl))
(in-package image)

(ql:quickload :vecto)
(use-package :vecto)


(defun /hex (n)
  (if (eq n 255)
      1
      (multiple-value-bind (_ rem)
        (ftruncate (/ n 255) 1.0)
        (declare (ignore _))
        rem)))
(defun hex-to-rgba (r g b &optional (a 1))
  (list
    (/hex r)
    (/hex g)
    (/hex b)
    a))

(defun rest-of-string (s from &optional to)
  (subseq s from to))

(defun split-off-token (str from to &optional (offset 0))
  (let ((to (if (null to)
                (length str)
                to)))
    (values
      (list (rest-of-string str from to)
            (+ offset from)
            (+ offset to))
      (- to from)
      (rest-of-string str to))))

(defun find-next-boundary (str fn)
  (loop for i from 0
        for c across str
        when (not (apply fn (list c i)))
             return i))

(defun isspace (c i)
  (declare (ignore i))
  (or (eq #\space c)
      (eq #\linefeed c)))

(defun isprint (c i)
  (declare (ignore i))
  (not (or (eq #\space c)
           (eq #\( c)
           (eq #\) c)
           (eq #\' c))))

(defun find-next-token (str offset)
  (cond ((equal "" str) nil)
        (t
         (let ((lead (char str 0)))
           (cond ((or (eq lead #\()
                      (eq lead #\))
                      (eq lead #\')
                      (eq lead #\`)
                      (eq lead #\,)
                      (eq lead #\@))
                  (values (list (make-string 1 :initial-element lead) offset (1+ offset))
                          1
                          (rest-of-string str 1)))
                 ((eq lead #\space)
                  (split-off-token str 0
                                   (find-next-boundary str #'isspace)
                                   offset))
                 ((eq lead #\;)
                  (values (list str offset (- (length str) offset))
                          (length str)
                          ""))
                 (t
                  (split-off-token str 0
                                   (find-next-boundary str #'isprint)
                                   offset)))))))

(defun find-tokens (str &optional (offset 0))
  (cond ((equal "" str) nil)
        (t (multiple-value-bind (token len new-str)
             (find-next-token str offset)
             (cons token
                   (find-tokens new-str (+ offset len)))))))

(defparameter *keywords* (make-hash-table))
(do-external-symbols (s :common-lisp)
  (setf (gethash s *keywords*) t))

(defun is-keyword? (sym)
  (multiple-value-bind (value found)
    (gethash sym *keywords*)
    (declare (ignore value))
    found))

(defun is-keyword-string? (str)
  (is-keyword? (intern (string-upcase str)))) 

(let ((i 0)
      (colors (list :red :cyan :green :violet))
      (in-qq nil)
      (in-q nil)
      (stack nil))
  (defun qq-color ()
    (setf in-qq t)
    :blue)
  (defun q-color ()
    (setf in-q t)
    :red)
  (defun open-paren-color ()
    (cond (in-qq
           (setf in-qq nil)
           (push :blue stack)
           :blue)
          (t
           (let ((c (nth i colors)))
             (setf i (mod (1+ i) (length colors)))
             (push c stack)
             c))))
  (defun close-paren-color ()
    (let ((c (pop stack)))
      (if (not (eq :blue c))
        (setf i (mod (1- i) (length colors))))
      c))
  (defun identifier-color (c)
    (if in-q :cyan c))
  (defun space-color (c)
    (cond (in-q
            (setf in-q nil)
            :cyan)
          (t c))))

(defun classify-token (token)
  (cond ((equal "(" token) 'open)
        ((equal ")" token) 'close)
        ((equal "`" token) 'qq)
        ((eq (char token 0) #\,)     'comma)
        ((eq (char token 0) #\@)     'at)
        ((eq (char token 0) #\#)     'sharp)
        ((eq (char token 0) #\')     'q)
        ((eq (char token 0) #\space) 'space)
        ((eq (char token 0) #\;)     'comment)
        ((is-keyword-string? token)  'keyword)
        (t 'identifier)))

(defun colorize-token (token)
  (case (classify-token token)
    (open       (open-paren-color))
    (close      (close-paren-color))
    (qq         (qq-color))
    (comma      (identifier-color :red))
    (at         :red)
    (q          (q-color))
    (space      (space-color :white))
    (sharp      :green)
    (comment    :cyan)
    (keyword    (identifier-color :yellow))
    (identifier (identifier-color :white))))

(defun make-grid (w h)
  (make-array (list h w)
              :initial-element (cons :white #\space)))

(defun gridify (grid row line)
  (let ((tokens (find-tokens line)))
    (loop for token in tokens
          do (let ((lexeme (car token))
                   (start  (cadr token))
                   (end    (caddr token)))
               (loop for i from start to end
                     for c across lexeme
                     do (setf (aref grid row i)
                              (cons (colorize-token lexeme) c)))))))

(defparameter *colors* (make-hash-table))
(defun set-color (key color)
  (setf (gethash key *colors*) color))

(set-color :white  (hex-to-rgba #xff #xff #xff 0.9))
(set-color :black  (hex-to-rgba #x11 #x11 #x11))
(set-color :red    (hex-to-rgba #xec #x2f #x2f))
(set-color :cyan   (hex-to-rgba #x5f #xfd #xff))
(set-color :yellow (hex-to-rgba #xff #xfc #x67))
(set-color :green  (hex-to-rgba #x5f #xfa #x68))
(set-color :blue   (hex-to-rgba #x68 #x71 #xff))
(set-color :violet (hex-to-rgba #xff #x77 #xff))


(defparameter *glyph-width* 14)
(defparameter *glyph-height* 35)
(defparameter *pad-x* 40)
(defparameter *pad-y* 60)
(defmacro with-grid-canvas (grid &body body)
  (let ((w (gensym))
        (h (gensym)))
    `(destructuring-bind (,h ,w)
       (array-dimensions ,grid)
       (with-canvas (:width  (+ (* 2 *pad-x*) (* *glyph-width*  ,w))
                     :height (+ (* 2 *pad-y*) (* *glyph-height* ,h)))
         ,@body))))

(defun grid-w (g)
  (destructuring-bind (h w)
    (array-dimensions g)
    (declare (ignore h))
    w))

(defun grid-h (g)
  (destructuring-bind (h w)
    (array-dimensions g)
    (declare (ignore w))
    h))

(defun grid-y (g y)
  (- (grid-h g) 0.6 y))
(defun grid-x (g x)
  (declare (ignore g))
  x)

(defun scale-x (x)
  (+ *pad-x* (* *glyph-width* x)))
(defun scale-y (y)
  (+ *pad-y* (* *glyph-height* y)))

(defun draw-glyph (x y glyph)
  (let ((x (scale-x x))
        (y (scale-y y))
        (c (gethash (car glyph) *colors*)))
    (when c
      (apply #'set-rgba-fill (gethash (car glyph) *colors*)))
    (draw-string x y (list (cdr glyph)))))

(defun make-grid-for (lines)
  (let ((n 0))
    (loop for line in lines do
          (let ((m (length line)))
            (setf n (if (> m n) m n))))
    (make-grid n (length lines))))

(defun lispify (out lines &key (font "mono.ttf"))
  (let* ((grid (make-grid-for lines)))
    (with-grid-canvas grid
       (let ((font (get-font font)))
         (set-rgb-fill 0 0 0)
         (clear-canvas)
         (set-font font 24)
         (loop for line in lines
               for row from 0
               do (gridify grid row line))
         (loop for x from 0 to (1- (grid-w grid)) do
          (loop for y from 0 to (1- (grid-h grid)) do
                (draw-glyph (grid-x grid x)
                            (grid-y grid y)
                            (aref grid y x))))
         (save-png out)))))
