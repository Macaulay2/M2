Macaulay2 lexer for Pygments
============================

[Pygments](https://pygments.org/) is a Python syntax highlighter.  It is available by running:

```
pip install Pygments
```

Or, in Debian-based Linux distributions:

```
sudo apt install python3-pygments
```

Beginning with Pygments 2.12.0, Macaulay2 syntax highlighting is available by
default.  For example:

```
pygmentize <path-to-m2-file>
```

This will print the syntax-highlighted contents of the Macaulay2 file to
`stdout`.  For html output:

```
pygmentize -O full -o foo.html <path-to-m2-file>
```

If you have an older version of Pygments, or would like to generate the
Macaulay2 lexer yourself, run:


```
M2 --script <path-to-M2-source>/M2/Macaulay2/editors/make-M2-symbols.m2
```

The file `macaulay2.py` should appear in the `pygments` subdirectory of your
current directory.  Then, if you want syntax highlighting for a Macaulay2 file,
you would run:

```
cd pygments
pygmentize -x -l macaulay2.py:Macaulay2Lexer <path-to-m2-file>
```

For more information, please refer to the [Pygments
documentation](https://pygments.org/docs/).
