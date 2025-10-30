// highlight-code.js
document.addEventListener('DOMContentLoaded', function() {
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
});