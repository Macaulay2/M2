import hljs from 'highlight.js/lib/core';
import macaulay2 from './macaulay2.js';
import 'highlight.js/styles/default.css';
import './highlight-override.css';

hljs.registerLanguage('macaulay2', macaulay2);
hljs.configure({ cssSelector: 'code' });
hljs.highlightAll();
