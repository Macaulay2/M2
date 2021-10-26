# Packaging Macaulay2 for Ubuntu and Debian

1. Compile Macaulay2 in a container:
```
make build
```

2. Create a `.deb` package:
```
make deb
```

3. Run `lintian` on the package:
```
make lintian
```
