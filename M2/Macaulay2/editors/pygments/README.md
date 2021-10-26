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

To use Pygments for syntax highlighting Macaulay2 code, generate the
file `macaulay2.py` by running:

```
M2 --script <path-to-M2-source>/M2/Macaulay2/editors/make-M2-symbols.m2
```

The file should appear in the `pygments` subdirectory of your current
directory.  Then, if you want syntax highlighting for a Macaulay2 file,
you would run:

```
cd pygments
pygmentize -x -l macaulay2.py:Macaulay2Lexer <path-to-m2-file>
```

This will print the syntax-highlighted contents of the Macaulay2 file to
`stdout`.  For html output:

```
pygmentize -x -l macaulay2.py:Macaulay2Lexer -O full -o foo.html <path-to-m2-file>
```

For more information, please refer to the [Pygments
documentation](https://pygments.org/docs/).
