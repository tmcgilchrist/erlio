$(document).ready(function() {
  Erlio.init();
  var currentYear = (new Date).getFullYear();
  $("#year").text( currentYear );

});

var Erlio = {

  init: function() {
    Erlio.observeFormSubmissions();
    Erlio.observeRestarts();
  },

  observeFormSubmissions: function() {
    $('form').submit(function(e) {
      var form = $(e.target);
      e.preventDefault();
      $.ajax(
        { type: form.attr('method')
        , contentType: 'application/json'
        , url: form.attr('action')
        , data:
          JSON.stringify({ url: $('#input-url').val() })
        , success: function(data, textStatus, request) {
            var outputURL = request.getResponseHeader('Location');
            $('#output-url').val(outputURL).select();
            $('#input-url').removeClass('error');
            $('.arrow_box').hide();
            $('#bar').addClass('flipped');
          }
        , error: function(xhr, ajaxOptions, thrownError) {
            $('#error').text(xhr.responseText);
            $('#input-url').addClass('error');
            $('.arrow_box').show();
          }
        }
      );
    });
  },

  observeRestarts: function() {
    $('a#restart').click(function(e) {
      e.preventDefault();
      $('#input-url').val('').focus();
      $('#output-url').val('');
      $('#copied-msg').hide();
      $('#bar').removeClass('flipped');
    });
  }
};
