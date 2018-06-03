# Polyhedra
This project is about maintaining and extending the `Polyhedra.m2` package. Polyhedra is a package for computations with combinatorial objects, such as cones, fans, polyhedra and polyhedral complexes. It was originally developed by [René Birkner](http://www.osa.fu-berlin.de/mathematik/perspektiven/it-referent/index.html), here is an [article about Polyhedra](https://msp.org/jsag/2009/1-1/p03.xhtml).

Polyhedra can be loaded and checked with
```
loadPackage "Polyhedra"
check "Polyhedra"
```
It still has many parts without tests. Some older todos can be found in the file `TODO_file_from_Polyhedra` in this folder.

Furthermore in a second part of this project we want to build an interface to the [polymake](https://polymake.org/doku.php) library. This is not the same as the existing polymake interface that writes and reads files.

## Setup
In order to work on Polyhedra, you need a recent version of M2 and a clone or fork of [my fork of M2](https://github.com/lkastner/M2).

If you want to participate in attaching polymake, you need an installed version of polymake as well. To circumvent issues with different operating systems, I have prepared a [Dockerfile](https://github.com/lkastner/dockerfiles/blob/master/build_env/M2_pm/Dockerfile) (see [Docker](https://www.docker.com/)). You should download it, add a user with the same uid and gid as yours inside the container, and build it with
```
docker build -t m2env .
```
Then you can mount your clone or fork inside with
```
docker run -it -v /path/to/M2/source/:/home/USER/M2 --name m2build m2env /bin/bash
```
Once inside you can build M2 according to the [instructions](http://www2.macaulay2.com/Macaulay2/Downloads/SourceCode/index.html).

## Tasks
### Add tests for PolyhedraComplex
### Triangulations
What should the datatype of a triangulation be? Refactor the method computing a triangulation.
### Eliminate deprecation warnings in documentation.
### Normaliz interface
There is a Normaliz package, can we use it to compute Hilbert bases of cones? Especially cones with lineality.
