(ql:quickload :swank)
(let ((swank::*loopback-interface* "0.0.0.0"))
  (swank:create-server :dont-close t))

(restore-chirp-auth)
(watch-and-tweet)
