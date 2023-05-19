# Macaulay2 testbot for GitHub Actions

The `Dockerfile` in this directory creates a container image based on Debian, intended for use as a testbot for Macaulay2 workshops powered by GitHub Actions.

## Getting Started
0. Install Docker and start the daemon.

1. Build a testbot container with Macaulay2
```
docker build --tag macaulay2 .
```

2. Tag and push the container to GitHub Packages

Once the image is working as intended (you can test it with `make shell`), you can publish it via GitHub Packages:
```
docker tag macaulay2 ghcr.io/mahrud/macaulay2:latest
docker push ghcr.io/mahrud/macaulay2:latest
```

## Create a Macaulay2 Repository Testbot

In a repository where you wish to develop Macaulay2, create a file `.github/workflows/testbot.yml`:
```yaml
name: Project Testbot
on: [push]

jobs:
  testbot:
    name: Testing ${{ github.head_ref || github.ref_name }}
    runs-on: ubuntu-latest
    container:
      image: 'ghcr.io/mahrud/macaulay2:latest'
      volumes:
        - '${{ github.workspace }}:/home/macaulay'

    steps:
      - name: Checkout the project repository
        uses: actions/checkout@v2

      - name: Run testbot on project branch
        run: |
          M2 --script tests/testbot.m2
```
This is a simplified version of the [workflow](https://github.com/Macaulay2/Workshop-2023-Minneapolis/blob/main/.github/workflows/) running tests for the 2023 Macaulay2 workshop in Minneapolis. The very last line can be modified, or extended to multiple lines, to run any scripts inside the repository every time you push a new commit to GitHub. For instance, an example used for the workshop can be found [here](https://github.com/Macaulay2/Workshop-2023-Minneapolis/blob/main/tests/testbot.m2).

## Running Tests Locally

You can use the following command to locally run the same tests that GitHub Actions runs:
```
docker run -v "REPO_DIR":"/home/macaulay" ghcr.io/mahrud/macaulay2 --script tests/testbot.m2
```
Note that you need to replace `REPO_DIR` with the address to a local clone of your project repository, set to the correct branch, and you may need to edit the `--script tests/testbot.m2` appropriate to your workflow action above.

Tip: if issues persist, you can create a shell inside the container and manually run M2 and test your scripts.
```
docker run -v "REPO_DIR":"/home/macaulay" -it --entrypoint bash ghcr.io/mahrud/macaulay2:latest
```
