// Function to update the logo's size and position based on current slide
function updateLogoSizePosition() {
    // Find the logo element in the DOM using its CSS class
    const logo = document.querySelector(".slide-logo");
    
    // If logo doesn't exist, exit the function early to prevent errors
    if (!logo) return;
    
    // Check if we're on the title slide (first slide)
    // Reveal.getIndices() returns an object with h (horizontal) and v (vertical) indices
    // h: 0 and v: 0 means we're on the very first slide (title slide)
    const isTitleSlide = Reveal.getIndices().h === 0 && Reveal.getIndices().v === 0;
    
    if (isTitleSlide) {
        // TITLE SLIDE STYLING: Top left corner with larger size
        logo.style.cssText = `
            display: block !important;           /* Ensure logo is visible */
            position: fixed !important;          /* Fixed positioning relative to viewport */
            top: 5px !important;                 /* 5px from top of screen */
            left: 12px !important;               /* 12px from left edge (top left corner) */
            right: auto !important;              /* Override any right positioning */
            height: 80px !important;            /* Set height to 100px for larger size */
            width: auto !important;              /* Let width adjust automatically to maintain aspect ratio */
            max-width: none !important;          /* Remove any maximum width constraints */
            object-fit: contain !important;      /* Ensure image maintains aspect ratio without distortion */
        `;
    } else {
        // REGULAR SLIDES STYLING: Top right corner with smaller size
        logo.style.cssText = `
            display: block !important;           /* Ensure logo is visible */
            position: fixed !important;          /* Fixed positioning relative to viewport */
            top: 3px !important;                 /* 5px from top of screen (same vertical position) */
            left: auto !important;               /* Override any left positioning */
            right: 12px !important;              /* 12px from right edge (top right corner) */
            height: 40px !important;             /* Set height to 60px for smaller size */
            width: auto !important;              /* Let width adjust automatically to maintain aspect ratio */
            max-width: none !important;          /* Remove any maximum width constraints */
            object-fit: contain !important;      /* Ensure image maintains aspect ratio without distortion */
        `;
    }
}

// Wait for the page to fully load before executing the code
window.addEventListener("load", function() {
    // Set up an interval to check if Reveal.js is loaded and ready
    // This is necessary because Reveal.js might load after our script
    const checkReveal = setInterval(function() {
        // Check if the Reveal object exists in the global window object
        if (window.Reveal) {
            // Clear the interval once Reveal.js is found
            clearInterval(checkReveal);
            
            // Set initial logo position when page loads
            updateLogoSizePosition();
            
            // Add event listener for slide changes - update logo when user navigates
            Reveal.on('slidechanged', updateLogoSizePosition);
            
            // Add event listener for when Reveal.js is fully ready
            Reveal.on('ready', updateLogoSizePosition);
        }
    }, 100); // Check every 100 milliseconds (10 times per second)
});