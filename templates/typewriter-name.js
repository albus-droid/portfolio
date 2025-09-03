document.addEventListener('DOMContentLoaded', function() {
    const name = 'Albin Babu Varghese';
    const speed = 50;
    const targetElement = document.getElementById('name-typewriter');
    
    if (!targetElement) return;

    let displayedText = '';
    let currentIndex = 0;
    
    const nameSpan = document.createElement('span');
    targetElement.appendChild(nameSpan);

    const cursorSpan = document.createElement('span');
    cursorSpan.className = 'cursor';
    targetElement.appendChild(cursorSpan);

    function type() {
        if (currentIndex < name.length) {
            displayedText += name[currentIndex];
            nameSpan.textContent = displayedText;
            currentIndex++;
            setTimeout(type, speed);
        } else {
            setTimeout(() => {
                cursorSpan.style.display = 'none';
            }, 800);
        }
    }

    type();
});
