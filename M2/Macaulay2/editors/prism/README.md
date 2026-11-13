Macaulay2 in Prism
==================

[Prism](https://prismjs.com/) is a lightweight, extensible JavaScript
syntax highlighter.  It has been used for syntax highlighting examples
in the Macaulay2 html documentation since version 1.22, and is also
used in [Macaulay2Web](https://github.com/pzinn/Macaulay2Web).

Building
--------

To regenerate the Macaulay2 grammar from this repository, change to
`M2/Macaulay2/editors` and run:

```
M2 --script make-M2-symbols.m2
```

This writes the file `prism/macaulay2.js`.

Using
-----

Download `prism.js` and `prism.css` from
[prismjs.com](https://prismjs.com/) and then include the following in
your html document:

```html
<script src="/path/to/prism.js"></script>
<script src="/path/to/macaulay2.js"></script>
<link rel="stylesheet" href="/path/to/prism.css">
```

Then set the `class` of any `<code>` elements you would like syntax
highlighted to `"language-macaulay2"`.  For example:

```html
<pre>
<code class="language-macaulay2">
R = QQ[x, y, z, w]
monomialCurveIdeal(R, {1, 2, 3})
</code>
</pre>
```

Style package
-------------

The file `prism.js` in Macaulay2's `Style` package bundles the Prism
JavaScript and CSS files together with the Macaulay2 grammar using
[node](https://nodejs.org/) and [webpack](https://webpack.js.org/).

To build it using autotools:

```
make -C Macaulay2/editors prism/prism.js
```

And cmake:

```
ninja M2-prism
```
