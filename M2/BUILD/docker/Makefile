TAG = m2
STORAGE = `pwd`/storage
M2REPO = `pwd`/../../../

always:
	@cat README.md

build: build-minimal

run: run-graphical

all: build run

build-minimal:
	docker build --tag $(TAG) .

run-graphical:
	docker run --volume $(STORAGE):/home --volume $(M2REPO):/home/M2 --net=host --env="DISPLAY" $(TAG)

run-terminal:
	docker run --volume $(STORAGE):/home --volume $(M2REPO):/home/M2 --interactive --tty $(TAG)
