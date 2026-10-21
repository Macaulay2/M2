(function () {
    'use strict';

    var PAGE = 10;

    window.addEventListener('DOMContentLoaded', function () {
        var input = document.getElementById('search-input');
        var results = document.getElementById('search-results');
        if (!input || !results) return;
        if (typeof lunr === 'undefined' || !window.M2SearchData) return;

        var docs = window.M2SearchData.docs;

        var accentFold = function (token) {
            return token.update(function (str) {
                return str.normalize('NFD').replace(/[̀-ͯ]/g, '');
            });
        };
        lunr.Pipeline.registerFunction(accentFold, 'accentFold');

        var idx = lunr(function () {
            this.pipeline.before(lunr.stemmer, accentFold);
            this.field('name',    { boost: 10 });
            this.field('package', { boost:  5 });
            this.field('tagline', { boost:  3 });
            this.ref('id');
            var self = this;
            Object.keys(docs).forEach(function (id) {
                var doc = docs[id];
                doc.id = id;
                self.add(doc);
            });
        });

        // Compute the Style directory URL from the search-data.js script tag so that
        // doc paths (stored relative to Style/) resolve correctly from any page depth.
        var scriptEl = document.querySelector('script[src*="search-data.js"]');
        var styleBase = scriptEl ? new URL('.', scriptEl.src).href : '';

        var currentHits = [];
        var shown = 0;

        function makeResultEl(hit) {
            var doc = docs[hit.ref];
            if (!doc) return null;

            var a = document.createElement('a');
            try {
                a.href = styleBase ? new URL(doc.path, styleBase).href : doc.path;
            } catch (e) {
                a.href = doc.path;
            }
            a.className = 'search-result';

            var nameEl = document.createElement('span');
            nameEl.className = 'search-name';
            nameEl.textContent = doc.name;
            a.appendChild(nameEl);

            var pkgEl = document.createElement('span');
            pkgEl.className = 'search-pkg';
            pkgEl.textContent = doc.package;
            a.appendChild(pkgEl);

            if (doc.tagline) {
                var tlEl = document.createElement('span');
                tlEl.className = 'search-tagline';
                tlEl.textContent = doc.tagline;
                a.appendChild(tlEl);
            }
            return a;
        }

        function showMore() {
            var btn = results.querySelector('.search-more');
            if (btn) btn.remove();

            var next = Math.min(shown + PAGE, currentHits.length);
            var firstNew = null;
            for (var i = shown; i < next; i++) {
                var el = makeResultEl(currentHits[i]);
                if (el) {
                    if (!firstNew) firstNew = el;
                    results.appendChild(el);
                }
            }
            shown = next;

            if (shown < currentHits.length) {
                btn = document.createElement('button');
                btn.className = 'search-more';
                btn.textContent = 'Show ' + Math.min(PAGE, currentHits.length - shown) + ' more…';
                btn.addEventListener('click', function (e) {
                    e.preventDefault();
                    e.stopPropagation();
                    showMore();
                });
                results.appendChild(btn);
            }

            if (firstNew) firstNew.scrollIntoView({ behavior: 'smooth', block: 'nearest' });

            results.style.display = 'block';
            return firstNew;
        }

        function show(hits) {
            results.innerHTML = '';
            currentHits = hits;
            shown = 0;
            if (hits.length === 0) {
                var msg = document.createElement('div');
                msg.className = 'search-noresults';
                msg.textContent = 'No results found.';
                results.appendChild(msg);
                results.style.display = 'block';
                return;
            }
            showMore();
        }

        input.addEventListener('input', function () {
            var q = input.value.trim();
            if (q.length > 1) {
                var terms = q.toLowerCase().normalize('NFD').replace(/[̀-ͯ]/g, '').split(/\s+/).filter(Boolean);
                var hits = [];
                try {
                    hits = idx.query(function (query) {
                        terms.forEach(function (term) {
                            query.term(term, { boost: 10 });
                            query.term(term, { wildcard: lunr.Query.wildcard.TRAILING, boost: 1 });
                            query.term(term, { editDistance: 1, boost: 3 });
                        });
                    });
                } catch (e) {}
                show(hits);
            } else {
                results.innerHTML = '';
                results.style.display = 'none';
            }
        });

        input.addEventListener('keydown', function (e) {
            if (e.key === 'ArrowDown' && results.style.display !== 'none') {
                var first = results.querySelector('.search-result, .search-more');
                if (first) { e.preventDefault(); first.focus(); }
            } else if (e.key === 'Escape') {
                results.style.display = 'none';
            }
        });

        results.addEventListener('keydown', function (e) {
            var items = Array.from(results.querySelectorAll('.search-result, .search-more'));
            var idx = items.indexOf(document.activeElement);
            if (e.key === 'ArrowDown') {
                e.preventDefault();
                if (idx < items.length - 1) items[idx + 1].focus();
            } else if (e.key === 'ArrowUp') {
                e.preventDefault();
                if (idx > 0) items[idx - 1].focus();
                else input.focus();
            } else if ((e.key === 'Enter' || e.key === ' ') && document.activeElement.classList.contains('search-more')) {
                e.preventDefault();
                var firstNew = showMore();
                if (firstNew) firstNew.focus();
            } else if (e.key === 'Escape') {
                results.style.display = 'none';
                input.focus();
            }
        });

        input.addEventListener('focus', function () {
            if (input.value.trim().length > 1 && results.innerHTML !== '') {
                results.style.display = 'block';
            }
        });

        document.addEventListener('click', function (e) {
            if (!results.contains(e.target) && e.target !== input) {
                results.style.display = 'none';
            }
        });
    });
}());
