// Suave Website - Theme Toggle and Interactivity

// Theme management
const ThemeManager = (() => {
  const THEME_KEY = 'suave-theme';
  const LIGHT_THEME = 'light';
  const DARK_THEME = 'dark';
  
  const init = () => {
    const savedTheme = localStorage.getItem(THEME_KEY);
    const prefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
    
    const theme = savedTheme || (prefersDark ? DARK_THEME : LIGHT_THEME);
    applyTheme(theme);
  };
  
  const applyTheme = (theme) => {
    console.log('Applying theme:', theme);
    const html = document.documentElement;
    const body = document.body;
    console.log('Before - dark class:', html.classList.contains('dark'), 'light class:', html.classList.contains('light'));
    
    if (theme === DARK_THEME) {
      html.classList.add('dark');
      html.classList.remove('light');
      // Fallback: set inline styles if CSS doesn't apply
      body.style.backgroundColor = '#1a1a1a';
      body.style.color = '#e0e0e0';
    } else {
      html.classList.add('light');
      html.classList.remove('dark');
      // Fallback: set inline styles if CSS doesn't apply
      body.style.backgroundColor = '#f2f2f2';
      body.style.color = '#373737';
    }
    console.log('After - dark class:', html.classList.contains('dark'), 'light class:', html.classList.contains('light'));
    console.log('HTML element classes:', html.className);
    
    // Check computed styles
    const computedStyle = window.getComputedStyle(body);
    console.log('Body background-color:', computedStyle.backgroundColor);
    console.log('Body color:', computedStyle.color);
    
    // Force a repaint to ensure CSS rules are applied
    void html.offsetHeight;
    
    localStorage.setItem(THEME_KEY, theme);
    updateThemeButton(theme);
  };
  
  const toggle = () => {
    console.log('Toggle called');
    const html = document.documentElement;
    const newTheme = html.classList.contains('dark') ? LIGHT_THEME : DARK_THEME;
    console.log('Current theme has dark class:', html.classList.contains('dark'), 'switching to:', newTheme);
    applyTheme(newTheme);
  };
  
  const updateThemeButton = (theme) => {
    const button = document.getElementById('theme-toggle');
    if (button) {
      button.textContent = theme === DARK_THEME ? '☀️' : '🌙';
      button.setAttribute('aria-label', `Switch to ${theme === DARK_THEME ? 'light' : 'dark'} mode`);
    }
  };
  
  const setupToggleButton = () => {
    const button = document.getElementById('theme-toggle');
    console.log('setupToggleButton called, button:', button);
    if (button) {
      console.log('Adding click listener to theme toggle button');
      button.addEventListener('click', (e) => {
        console.log('Theme toggle clicked!');
        e.preventDefault();
        toggle();
      });
    } else {
      console.log('Theme toggle button not found');
    }
  };
  
  return { init, toggle, setupToggleButton };
})();

// Mobile menu toggle
const MobileMenu = (() => {
  const init = () => {
    const hamburger = document.getElementById('hamburger');
    const navLinks = document.getElementById('nav-links');
    
    if (hamburger) {
      hamburger.addEventListener('click', () => {
        navLinks.classList.toggle('active');
      });
      
      // Close menu when clicking a link
      navLinks.querySelectorAll('a').forEach(link => {
        link.addEventListener('click', () => {
          navLinks.classList.remove('active');
        });
      });
    }
  };
  
  return { init };
})();

// Copy code to clipboard
const CodeCopy = (() => {
  const init = () => {
    const codeBlocks = document.querySelectorAll('.code-block');
    
    codeBlocks.forEach(block => {
      const button = document.createElement('button');
      button.className = 'copy-button';
      button.textContent = 'Copy';
      button.setAttribute('aria-label', 'Copy code');
      
      block.style.position = 'relative';
      block.appendChild(button);
      
      button.addEventListener('click', () => {
        const code = block.querySelector('code') || block.querySelector('pre');
        if (code) {
          const text = code.textContent;
          navigator.clipboard.writeText(text).then(() => {
            button.textContent = '✓ Copied!';
            button.classList.add('copied');
            setTimeout(() => {
              button.textContent = 'Copy';
              button.classList.remove('copied');
            }, 2000);
          }).catch(err => {
            console.error('Copy failed:', err);
          });
        }
      });
    });
  };
  
  return { init };
})();

// Table of contents generator
const TableOfContents = (() => {
  const init = () => {
    const main = document.querySelector('main');
    if (!main) return;
    
    const headings = main.querySelectorAll('h2, h3');
    if (headings.length < 2) return;
    
    const toc = document.createElement('nav');
    toc.className = 'table-of-contents';
    toc.innerHTML = '<h3>On this page</h3><ul></ul>';
    
    const list = toc.querySelector('ul');
    let currentLevel = 2;
    let currentList = list;
    const stack = [{ level: 2, list }];
    
    headings.forEach((heading, index) => {
      const level = parseInt(heading.tagName[1]);
      const id = heading.id || `heading-${index}`;
      heading.id = id;
      
      const li = document.createElement('li');
      li.innerHTML = `<a href="#${id}">${heading.textContent}</a>`;
      
      if (level > currentLevel) {
        const newList = document.createElement('ul');
        currentList.lastElementChild.appendChild(newList);
        stack.push({ level, list: newList });
        newList.appendChild(li);
        currentList = newList;
      } else if (level < currentLevel) {
        while (stack.length > 1 && stack[stack.length - 1].level > level) {
          stack.pop();
          currentList = stack[stack.length - 1].list;
        }
        currentList.appendChild(li);
      } else {
        currentList.appendChild(li);
      }
      
      currentLevel = level;
    });
    
    const firstHeading = main.querySelector('h2, h3');
    if (firstHeading) {
      firstHeading.parentNode.insertBefore(toc, firstHeading);
    }
  };
  
  return { init };
})();

// Smooth scroll offset for sticky header
const SmoothScroll = (() => {
  const init = () => {
    document.querySelectorAll('a[href^="#"]').forEach(link => {
      link.addEventListener('click', (e) => {
        const href = link.getAttribute('href');
        if (href === '#') return;
        
        const target = document.querySelector(href);
        if (!target) return;
        
        e.preventDefault();
        const headerOffset = 80;
        const elementPosition = target.getBoundingClientRect().top;
        const offsetPosition = elementPosition + window.pageYOffset - headerOffset;
        
        window.scrollTo({
          top: offsetPosition,
          behavior: 'smooth'
        });
      });
    });
  };
  
  return { init };
})();

// Initialize everything on page load
document.addEventListener('DOMContentLoaded', () => {
  console.log('DOMContentLoaded event fired');
  ThemeManager.setupToggleButton();
  MobileMenu.init();
  CodeCopy.init();
  TableOfContents.init();
  SmoothScroll.init();
});

// Initialize theme ASAP to avoid flash
console.log('Script loaded, initializing theme');
ThemeManager.init();
