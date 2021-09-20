FROM ubuntu:20.04

RUN apt-get update \
 && apt-get install -y sbcl libssl-dev
RUN mkdir /opt/lisp
ADD https://beta.quicklisp.org/quicklisp.lisp /opt/lisp/quicklisp.lisp
RUN sbcl --load /opt/lisp/quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --quit
RUN sbcl --load /root/quicklisp/setup.lisp \
         --eval '(progn (ql:quickload :cl-utilities) (ql:quickload :chirp))' \
         --quit
RUN sbcl --load /root/quicklisp/setup.lisp \
         --eval '(progn (ql:quickload :swank))' \
         --quit

COPY *.lisp /opt/lisp/
COPY entrypoint.sh /usr/bin/entrypoint

ENTRYPOINT ["/usr/bin/entrypoint"]
WORKDIR /opt/lisp
CMD ["--load", "/opt/lisp/storm.lisp", "--script", "/opt/lisp/init.lisp"]

EXPOSE 4005
