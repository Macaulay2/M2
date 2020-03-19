TAG = m2-cmake-build
STORAGE = `pwd`/storage
M2HOME = /home/macaulay
M2REPO = `pwd`/../../..
VOLUME = --volume $(STORAGE):$(M2HOME) --volume $(M2REPO):$(M2HOME)/M2

always:
	@cat README.md

build: build-image build-M2

run: run-graphical

all: build run

build-image:
	docker build --tag $(TAG) .

build-M2:
	mkdir -p build
	docker run --volume $(STORAGE):$(M2HOME) --volume $(M2REPO):$(M2HOME)/M2 --interactive --tty --entrypoint="" $(TAG) bash /home/macaulay/build-M2.sh

run-graphical:
	docker run $(VOLUME) --net=host --env="DISPLAY" $(TAG)

run-terminal:
	docker run $(VOLUME) --interactive --tty $(TAG)

M2:
	docker run $(VOLUME) --interactive --tty --entrypoint M2 $(TAG)

bash:
	docker run $(VOLUME) --interactive --tty --entrypoint bash $(TAG)

# TODO: need --int --no-readline --no-randomize in args?
