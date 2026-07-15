// Suave Website — theme, nav, code copy, docs TOC

const ThemeManager = (() => {
  const THEME_KEY = 'suave-theme';
  const LIGHT = 'light';
  const DARK = 'dark';

  const init = () => {
    const saved = localStorage.getItem(THEME_KEY);
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
    applyTheme(saved || (prefersDark ? DARK : LIGHT));
  };

  const applyTheme = (theme) => {
    const html = document.documentElement;
    html.classList.toggle('dark', theme === DARK);
    html.classList.toggle('light', theme !== DARK);
    localStorage.setItem(THEME_KEY, theme);
    updateThemeButton(theme);
  };

  const toggle = () => {
    const next = document.documentElement.classList.contains('dark') ? LIGHT : DARK;
    applyTheme(next);
  };

  const updateThemeButton = (theme) => {
    const button = document.getElementById('theme-toggle');
    if (!button) return;
    button.textContent = theme === DARK ? '☀' : '☾';
    button.setAttribute('aria-label', `Switch to ${theme === DARK ? 'light' : 'dark'} mode`);
  };

  const setupToggleButton = () => {
    const button = document.getElementById('theme-toggle');
    if (!button) return;
    button.addEventListener('click', (e) => {
      e.preventDefault();
      toggle();
    });
  };

  return { init, setupToggleButton };
})();

const MobileMenu = (() => {
  const init = () => {
    const hamburger = document.getElementById('hamburger');
    const navLinks = document.getElementById('nav-links');
    if (!hamburger || !navLinks) return;

    hamburger.addEventListener('click', () => {
      navLinks.classList.toggle('active');
    });

    navLinks.querySelectorAll('a').forEach((link) => {
      link.addEventListener('click', () => navLinks.classList.remove('active'));
    });
  };

  return { init };
})();

const CodeCopy = (() => {
  const init = () => {
    document.querySelectorAll('.code-block').forEach((block) => {
      if (block.querySelector('.copy-button')) return;
      const button = document.createElement('button');
      button.className = 'copy-button';
      button.type = 'button';
      button.textContent = 'Copy';
      button.setAttribute('aria-label', 'Copy code');
      block.appendChild(button);

      button.addEventListener('click', () => {
        const code = block.querySelector('code') || block.querySelector('pre');
        if (!code) return;
        navigator.clipboard.writeText(code.textContent || '').then(() => {
          button.textContent = 'Copied';
          button.classList.add('copied');
          setTimeout(() => {
            button.textContent = 'Copy';
            button.classList.remove('copied');
          }, 1600);
        });
      });
    });
  };

  return { init };
})();

const TableOfContents = (() => {
  const init = () => {
    if (document.body.dataset.noToc === 'true' || document.body.classList.contains('api-docs')) {
      return;
    }

    const content = document.querySelector('.docs-content') || document.querySelector('main .container');
    if (!content || content.dataset.noToc === 'true' || content.classList.contains('fsdocs-content')) {
      return;
    }

    const headings = content.querySelectorAll('h2, h3');
    if (headings.length < 3) return;

    const toc = document.createElement('nav');
    toc.className = 'table-of-contents';
    toc.setAttribute('aria-label', 'On this page');
    toc.innerHTML = '<h3>On this page</h3><ul></ul>';
    const list = toc.querySelector('ul');

    headings.forEach((heading, index) => {
      const id = heading.id || `section-${index}`;
      heading.id = id;
      const li = document.createElement('li');
      const a = document.createElement('a');
      a.href = `#${id}`;
      a.textContent = heading.textContent;
      if (heading.tagName === 'H3') {
        li.style.marginLeft = '0.75rem';
        li.style.fontSize = '0.92em';
      }
      li.appendChild(a);
      list.appendChild(li);
    });

    const first = content.querySelector('h1, h2');
    if (first && first.nextSibling) {
      first.parentNode.insertBefore(toc, first.nextSibling);
    } else {
      content.insertBefore(toc, content.firstChild);
    }
  };

  return { init };
})();

const DocsNavHighlight = (() => {
  const init = () => {
    const path = window.location.pathname.replace(/\/$/, '') || '/';
    document.querySelectorAll('.docs-sidebar a').forEach((a) => {
      const href = a.getAttribute('href') || '';
      try {
        const url = new URL(href, window.location.origin);
        const linkPath = url.pathname.replace(/\/$/, '');
        if (linkPath === path || (path.endsWith(linkPath) && linkPath.length > 1)) {
          a.classList.add('active');
        }
      } catch (_) { /* ignore */ }
    });
  };

  return { init };
})();

document.addEventListener('DOMContentLoaded', () => {
  ThemeManager.setupToggleButton();
  MobileMenu.init();
  CodeCopy.init();
  TableOfContents.init();
  DocsNavHighlight.init();
});

ThemeManager.init();
