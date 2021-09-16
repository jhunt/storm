#!/bin/bash
set -eu

if [[ ${1-} == "" ]]; then
	exec sbcl --load "/root/quicklisp/setup.lisp"
fi

if [[ -f "$1" && ! -x "$1" ]]; then
	exec sbcl --noinform --noprint \
	          --load "/root/quicklisp/setup.lisp" \
	          --script "$1"
fi

case "$1" in
-*)
	exec sbcl --noinform --noprint \
	          --load "/root/quicklisp/setup.lisp" \
	          "$@"
	;;
*)
	exec "$@"
	;;
esac
