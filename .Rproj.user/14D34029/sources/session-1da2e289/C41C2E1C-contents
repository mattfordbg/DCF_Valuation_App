$(document).ready(function() {
  // Function to fix scrolling in Handsontable instances
  function fixHandsontableScrolling() {
    // Find all scrollable containers in Handsontable instances
    $('.ht_master .wtHolder').each(function() {
      var $holder = $(this);

      // Only apply once to each holder
      if (!$holder.data('scroll-fixed')) {
        $holder.data('scroll-fixed', true);

        // Add wheel event listener with capture phase (before Handsontable's handlers)
        this.addEventListener('wheel', function(e) {
          // Get scroll position information
          var maxScroll = this.scrollHeight - this.clientHeight;
          var currentScroll = this.scrollTop;

          // If at top and scrolling up OR at bottom and scrolling down
          if ((e.deltaY < 0 && currentScroll <= 0) ||
              (e.deltaY > 0 && currentScroll >= maxScroll)) {

            // Let the parent scroll instead
            e.stopPropagation();
            return true;
          }
        }, { capture: true, passive: false }); // Use options object for capture & prevent default if needed
      }
    });
  }

  // Run initially after short delay
  setTimeout(fixHandsontableScrolling, 500);

  // Run when Shiny updates any table or output that might contain a table
  $(document).on('shiny:value shiny:outputinvalidated', function(event) {
    // Use a short delay to allow DOM updates
    setTimeout(fixHandsontableScrolling, 150);
  });

  // Use mutation observer more targetedly if possible, or keep observing body
  // Observing body is simpler but less performant if DOM changes frequently unrelated to tables
  new MutationObserver(function(mutations) {
    // Check if added nodes contain a handsontable instance? More complex.
    // Simpler: just rerun on any subtree change.
    setTimeout(fixHandsontableScrolling, 150);
  }).observe(document.body, { childList: true, subtree: true });
});
