Macaulay2 in highlight.js
=========================

[highlight.js](https://highlightjs.org/) is a JavaScript syntax highlighter
with language auto-detection and zero dependencies.  It is used for syntax
highlighting examples in the Macaulay2 html documentation.

Downloading
-----------

See https://highlightjs.org/download/ to obtain the main highlight.js
library.

To get to the Macaulay2 language module, do one of the following:

* Run [`npm install highlightjs-macaulay2`](
  https://www.npmjs.com/package/highlightjs-macaulay2).
* Run [`git clone https://github.com/d-torrance/highlightjs-macaulay2`](
  https://github.com/d-torrance/highlightjs-macaulay2).
* Build it yourself (see below).

Using
-----

In your html file, include the following, adjusting the paths to the css and
js files as necessary:

```html
<link rel="stylesheet" href="/path/to/default.min.css">
<script type="text/javascript" src="/path/to/highlight.min.js"></script>
<script type="text/javascript" src="/path/to/macaulay2.min.js"></script>
<script type="text/javascript">
  hljs.highlightAll();
</script>
```

Note that `default.min.css` may be replaced with any highlight.js
theme file.  See all of the available themes at
https://highlightjs.org/static/demo/.

Then include your Macaulay2 code:

```html
<pre>
<code class="language-macaulay2">
-- Macaulay2 code goes here
</code>
</pre>
```

Alternatively, you can use the copy of `highlight.js` that is shipped with Macaulay2 in the `Style` package.  It bundles all of the above into a single file, and so you need only one line:

```html
<script type="text/javascript" src="/path/to/Macaulay2/Style/highlight.js"></script>
```

Note that doing it this way will remove the background color from
the theme and highlight inline code.

Building
--------

You will need [node](https://nodejs.org/) and [git](https://git-scm.com/)
installed.  You should also have the Macaulay2 source repository cloned, say
at `/path/to/M2/source`.

First, clone the highlight.js repository and install the dependencies:

```
git clone https://github.com/highlightjs/highlight.js
cd highlight.js
npm install
```

Next, generate the Macaulay2 language file:

```
mkdir -p extra/highlightjs-macaulay2/src/languages
M2 --script /path/to/M2/source/M2/Macaulay2/editors/make-M2-symbols.m2
cp highlightjs/macaulay2.js extra/highlightjs-macaulay2/src/languages
```

Finally, build:

```
node ./tools/build.js -t cdn
```

The generated files should now be in the `build` directory.  Copy the
following files to your project:

* `build/highlight.min.js`
* `build/languages/macaulay2.min.js`
* `build/styles/default.min.css` (or any of the other styles in `build/styles`)
