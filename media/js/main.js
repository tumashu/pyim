$(document).ready(function() {
    /*******************************************************************
     * 1. replace css class "src" and "example" with "prettyprint", for
     *    prettify.js to render
     * 2. replace detail language css class, e.g. "src-scheme" to
     *    "lang-scheme" per the description of prettify.js
     ******************************************************************/
    var $blocks = $('pre.src');
    $blocks.each(function(index) {
        var self = $(this);
        var classes = self.removeClass('src').attr('class').split(/\s+/);
        $.each(classes, function(idx, cls) {
            if (cls.substring(0, 4) === 'src-') {
                var lang = cls.substring(4);
                self.removeClass(cls).addClass('lang-' + lang);
            }
        });
        self.addClass('prettyprint');
    });
    $('pre.example').removeClass('example').addClass('prettyprint');

    /*******************************************************************
     * 1. remove all org exported line number spans
     * 2. add css class "linenums" to code block per the description of
     *    prettify.js
     ******************************************************************/
    var $lines = $('span.linenr');
    var $linedBlocks = $lines.parent();
    $lines.remove();
    $linedBlocks.each(function(index) {
        $(this).addClass('linenums');
    });

    /*******************************************************************
     * pretty print all code blocks
     ******************************************************************/
    prettyPrint();
});
