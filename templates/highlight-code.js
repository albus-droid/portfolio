// highlight-code.js
document.addEventListener('DOMContentLoaded', function() {
    
    // Change title from h1 to h2
    const title = document.querySelector('h1.title');
    if (title) {
    const h2 = document.createElement('h2');
    h2.className = title.className;
    h2.textContent = title.textContent;
    title.parentNode.replaceChild(h2, title);
    }

  // Convert org-mode src blocks to highlight.js format
  document.querySelectorAll('pre[class*="src-"]').forEach(pre => {
    const match = pre.className.match(/src-(\w+)/);
    if (match) {
      const lang = match[1];
      
      // Wrap content in <code> tag if not already wrapped
      let code = pre.querySelector('code');
      if (!code) {
        code = document.createElement('code');
        code.textContent = pre.textContent;
        pre.textContent = '';
        pre.appendChild(code);
      }
      
      // Add language class to code element
      code.className = `language-${lang}`;
    }
  });
  
  // Initialize highlight.js
  hljs.highlightAll();
  
  // Add copy buttons to all code blocks
  document.querySelectorAll('pre code').forEach(block => {
    // Create copy button
    const button = document.createElement('button');
    button.className = 'copy-button';
    button.textContent = 'Copy';
    button.setAttribute('aria-label', 'Copy code to clipboard');
    
    // Copy functionality
    button.onclick = function() {
      navigator.clipboard.writeText(block.textContent).then(() => {
        button.textContent = 'Copied!';
        button.classList.add('copied');
        setTimeout(() => {
          button.textContent = 'Copy';
          button.classList.remove('copied');
        }, 2000);
      }).catch(err => {
        console.error('Failed to copy:', err);
        button.textContent = 'Failed';
        setTimeout(() => {
          button.textContent = 'Copy';
        }, 2000);
      });
    };
    
    // Make pre position relative for absolute positioning of button
    block.parentElement.style.position = 'relative';
    block.parentElement.appendChild(button);
  });
});