(ql:quickload :swank)
(let ((swank::*loopback-interface* "0.0.0.0"))
  (swank:create-server :dont-close t))

(loop do (sleep 3600))
