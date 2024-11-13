// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded "><a href="Index.html">들어가면서</a></li><li class="chapter-item expanded affix "><li class="part-title">On Lisp</li><li class="chapter-item expanded "><a href="onlisp/01-extensible-language.html">01. 확장 가능한 언어</a></li><li class="chapter-item expanded "><a href="onlisp/02-functions.html">02. 함수</a></li><li class="chapter-item expanded "><a href="onlisp/03-functional-programming.html">03. 함수형 프로그래밍</a></li><li class="chapter-item expanded "><a href="onlisp/04-utility-functions.html">04. 유틸리티 함수</a></li><li class="chapter-item expanded "><a href="onlisp/05-returning-functions.html">05. 함수를 반환하기</a></li><li class="chapter-item expanded "><a href="onlisp/06-functions-as-representation.html">06. 표현으로서의 함수</a></li><li class="chapter-item expanded "><a href="onlisp/07-macros.html">07. 매크로</a></li><li class="chapter-item expanded "><a href="onlisp/08-when-to-use-macros.html">08. When to Use Macros</a></li><li class="chapter-item expanded "><a href="onlisp/09-variable-capture.html">09. Variable Capture</a></li><li class="chapter-item expanded "><a href="onlisp/10-other-macro-pitfalls.html">10. Other Macro Pitfalls</a></li><li class="chapter-item expanded "><a href="onlisp/11-classic-macros.html">11. Classic Macros</a></li><li class="chapter-item expanded "><a href="onlisp/12-generalized-variables.html">12. Generalized Variables</a></li><li class="chapter-item expanded "><a href="onlisp/13-computation-at-compile-time.html">13. Computation at Compile-Time</a></li><li class="chapter-item expanded "><a href="onlisp/14-anaphoric-macros.html">14. Anaphoric Macros</a></li><li class="chapter-item expanded "><a href="onlisp/15-macros-returning-functions.html">15. Macros Returning Functions</a></li><li class="chapter-item expanded "><a href="onlisp/16-macro-defining-macros.html">16. Macro-Defining Macros</a></li><li class="chapter-item expanded "><a href="onlisp/17-read-macros.html">17. Read-Macros</a></li><li class="chapter-item expanded "><a href="onlisp/18-destructuring.html">18. Destructuring</a></li><li class="chapter-item expanded "><a href="onlisp/19-a-query-compiler.html">19. A Query Compiler</a></li><li class="chapter-item expanded "><a href="onlisp/20-continuations.html">20. Continuations</a></li><li class="chapter-item expanded "><a href="onlisp/21-multiple-processes.html">21. Multiple Processes</a></li><li class="chapter-item expanded "><a href="onlisp/22-nondeterminism.html">22. Nondeterminism</a></li><li class="chapter-item expanded "><a href="onlisp/23-parsing-with-atns.html">23. Parsing with ATNs</a></li><li class="chapter-item expanded "><a href="onlisp/24-prolog.html">24. Prolog</a></li><li class="chapter-item expanded "><a href="onlisp/25-object-oriented-lisp.html">25. Object-Oriented Lisp</a></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString();
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
