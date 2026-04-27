import 'bootstrap/dist/css/bootstrap.min.css';
import 'bootstrap/js/dist/collapse';
import '@fortawesome/fontawesome-free/css/fontawesome.css';
import '@fortawesome/fontawesome-free/css/solid.css';
import './fa-glyphicons-trick.css';
import ClipboardJS from 'clipboard';
import 'jquery';
import noUiSlider from 'nouislider/dist/nouislider.js';
import 'nouislider/dist/nouislider.css';

// trick Bootstrap 5 into supporting Bootstrap 3 attributes so we can
// keep using BootSideMenu
document.addEventListener("DOMContentLoaded", () => {
  document.querySelectorAll('[data-toggle="collapse"]').forEach(el => {
    if (!el.hasAttribute("data-bs-toggle")) {
      el.setAttribute("data-bs-toggle", "collapse");
    }
  });
});
