# Packaging Macaulay2 for Fedora et al.

1. Compile Macaulay2 in a container:
```
make build
```

2. Create a `.rpm` package:
```
make rpm
```

3. Run `rpmlint` on the package:
```
make rpmlint
```
