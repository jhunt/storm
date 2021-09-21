IMAGE ?= iamjameshunt/storm:latest

PROVE := prove -v --exec 'sbcl --noinform --load $(HOME)/quicklisp/setup.lisp --script'

test:
	$(PROVE) t/*.lisp

build:
	docker build -t storm .

run: build
	@docker stop storm || true
	@docker rm storm || true
	docker run -d --name storm -p 40050:4005 -v $(PWD)/_/tweets:/tweets -v $(PWD)/_/img:/img -v $(PWD)/_/etc:/etc/storm storm

push: build
	docker tag storm $(IMAGE)
	docker push $(IMAGE)
