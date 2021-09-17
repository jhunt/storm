build:
	docker build -t storm .

run: build
	@docker stop storm || true
	@docker rm storm || true
	docker run -d --name storm -p 40050:4005 -v $(PWD)/_/tweets:/tweets -v $(PWD)/_/img:/img -v $(PWD)/_/etc:/etc/storm storm
