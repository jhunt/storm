(load "tap.lisp")
(load "storm.lisp")

(defun should-equal (msg a b)
  (should msg (equal a b))
  (when (not (equal a b))
        (diag "expected ~S~%" a)
        (diag " but got ~S~%" b)))

(let ((the-tweet (make-tweet "a tweet"))
      (*test-prefix* "a tweet without media should "))
  (should "still be a tweet"
          (is-tweet the-tweet))

  (should "have status text"
          (equal "a tweet" (tweet-text the-tweet)))

  (should "not have media"
          (null (tweet-media the-tweet)))

  (should "not have media"
          (not (tweet-has-media? the-tweet))))

(let ((the-tweet (make-tweet "see?" #p"img.png"))
      (*test-prefix* "a tweet with media should "))
  (should "still be a tweet"
          (is-tweet the-tweet))

  (should "have media"
          (tweet-has-media? the-tweet)))

(let ((the-tweet (build-tweet (list "from" "some" "words")))
      (*test-prefix* "a tweet built up from a list of words "))
  (should "still be a tweet"
          (is-tweet the-tweet))

  (should "have the right status text"
          (equal "from some words"
                 (tweet-text the-tweet))))

(let ((the-tweet (make-tweet "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way—in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only."))

      (*test-prefix* "a really long tweet "))
  (should "be too long"
          (tweet-too-long? the-tweet))
  (should "not be too long if we allow 2k tweets..."
          (not (tweet-too-long? the-tweet 2048)))

  (let ((*tweet-length* 2048))

    (should "honor *tweet-length* dynamic var"
            (not (tweet-too-long? the-tweet)))))

(defun tw (test)
  (parse-input (format nil "t/data/~A.tweet" test)
               #p"t/tmp"))

(should-equal
  "parse a single text-only tweet properly"
  "just setting up my twttr"
  (tw "single"))

(should-equal
  "parse a multi-line text-only tweet properly"
  ; note that the [LF LF] turns into [SP SP]
  "the first.  the second."
  (tw "double"))

(should-equal
  "expand lisp code into image media references"
  "here's some lisp: <t/tmp/lisp1.png>"
  (tw "lisp1"))

(should-equal
  "properly disassemble tweets"
  `("this" "is" "a" "set" "of" "tokens")
  (disassemble-tweet "this is a set of tokens"))

(should-equal
  "properly assemble tweets"
  (cons "this is a set of tokens" nil)
  (build-tweet (list "this" "is" "a" "set" "of" "tokens")))

(should-equal
  "fit tweets based on desired length"
  (list (make-tweet "the first tweet")
        (make-tweet "the second tweet"))
  (let ((*tweet-length* 17))
    (fit-tweets nil `("the" "first" "tweet"
                      "the" "second" "tweet"))))

(should-equal
  "treat a single tweet as an unthreaded 'storm'"
  (list (make-tweet "just setting up my twttr"))
  (storm (tw "single")))

(should-equal
  "handle particularly long tweets"
  "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way—in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only."
  (tw "threaded"))

(should-equal
  "properly split tweets based on word length"
  (list (make-tweet "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light,

1/4")

       (make-tweet "it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going

2/4")
      (make-tweet "direct the other way—in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of

3/4")
      (make-tweet "comparison only.

4/4"))
  (storm (tw "threaded")))

(should-equal
  "split right after a media reference"
  (list (make-tweet "here's a picture

1/2" #p"img.png")
        (make-tweet "wasn't that neat?

2/2"))
  (storm (tw "media1")))

(should-equal
  "ignore lines between a media ref and the next text block"
  (list (make-tweet "here's a picture

1/2" #p"img.png")
        (make-tweet "wasn't that neat?

2/2"))
  (storm (tw "media2")))

(done-testing)
