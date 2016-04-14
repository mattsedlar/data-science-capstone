$(document).ready(function() {
  $("#submitPhrase").on('click', function() {
      setTimeout(function() { jQuery("#dialog").dialog() }, 8000);
  })
});
