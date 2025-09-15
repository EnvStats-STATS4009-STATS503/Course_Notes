// The script below will ensure the logo is displayed top left
// in his full size (probably on the first slide; title-slide)
// then as soon as a different slide is displayed, the logo will
// be displayed bottom right with a smaller size.

function updateLogoSizePosition(event) {
    if (event.currentSlide.matches('#title-slide')) {
    var elements = document.querySelectorAll(".slide-logo");
    [].forEach.call(elements, function(elem) {
        elem.classList.remove("slide-logo-bottom-right");
        elem.classList.add("slide-logo-max-size");
    });
    } else {
    var elements = document.querySelectorAll(".slide-logo");
    [].forEach.call(elements, function(elem) {
        elem.classList.add("slide-logo-bottom-right");
        elem.classList.remove("slide-logo-max-size");
        elem.style.display = "none";  // Hide the logo on other slides
    });
    }
};

window.addEventListener("load", function(event) {
    // Make sure the logo has its full size when the slideshow is loaded (i.e., apply the slide-logo-max-size css class)
    var elements = document.querySelectorAll(".slide-logo");
    [].forEach.call(elements, function(elem) {
        elem.classList.remove("slide-logo-bottom-right");
        elem.classList.add("slide-logo-max-size");
        elem.style.display = "block";  // Show the logo on the first load (title slide)
    });

    // Ensure the logo visibility is correct for the first slide on initial load
    Reveal.on("slidechanged", function(event) {
        updateLogoSizePosition(event);
    });

    // To handle cases where the user might navigate back to the first slide
    Reveal.on("ready", function(event) {
        // Make sure the logo visibility is correct when the presentation starts or after navigating back
        updateLogoSizePosition(event);
    });
});