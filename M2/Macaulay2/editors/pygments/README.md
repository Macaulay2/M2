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
Macaulay2 lexer yourself, change to `M2/Macaulay2/editors` and run:

```
M2 --script make-M2-symbols.m2
```

This writes the file `pygments/macaulay2.py`. Then, if you want syntax
highlighting for a Macaulay2 file with the generated lexer, run:

```
cd /path/to/M2/Macaulay2/editors/pygments
pygmentize -x -l macaulay2.py:Macaulay2Lexer <path-to-m2-file>
```

For more information, please refer to the [Pygments
documentation](https://pygments.org/docs/).
