<script src="https://cdnjs.cloudflare.com/ajax/libs/reveal.js/4.3.1/reveal.min.js"></script>
<script>
        // Overlay iframe system
        const overlayIframe = document.getElementById('overlayIframe');
        const overlayClose = document.getElementById('overlayClose');

        // Show overlay function
        function showOverlay(src) {
            overlayIframe.src = src;
            overlayIframe.classList.add('visible');
            overlayClose.classList.add('visible');
            
            // Pause RevealJS navigation
            Reveal.configure({ keyboard: false });
        }

        // Hide overlay function
        function hideOverlay() {
            overlayIframe.classList.remove('visible');
            overlayClose.classList.remove('visible');
            
            // Re-enable RevealJS navigation
            Reveal.configure({ keyboard: true });
            
            // Clear iframe src after animation
            setTimeout(() => {
                if (!overlayIframe.classList.contains('visible')) {
                    overlayIframe.src = 'about:blank';
                }
            }, 300);
        }

        // Add click handlers to all iframe triggers
        document.addEventListener('click', function(e) {
            if (e.target.classList.contains('iframe-trigger')) {
                const src = e.target.getAttribute('data-src');
                if (src) {
                    showOverlay(src);
                }
            }
        });

        // Close button handler
        overlayClose.addEventListener('click', hideOverlay);

        // ESC key to close overlay
        document.addEventListener('keydown', function(e) {
            if (e.key === 'Escape' && overlayIframe.classList.contains('visible')) {
                hideOverlay();
            }
        });

        // Prevent RevealJS from handling ESC when overlay is open
        document.addEventListener('keydown', function(e) {
            if (e.key === 'Escape' && overlayIframe.classList.contains('visible')) {
                e.stopPropagation();
            }
        }, true);
</script>