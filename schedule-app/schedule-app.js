futu.onload(function () {
    console.info("Initialising schedule js");
    // imports
    var $ = futu.$;
    var $_ = futu.$_;
    var $$ = futu.$$;
    var assert = futu.assert;
    var buttonOnClick = futu.buttonOnClick;

    $('select[data-futu-id=schedule-employees]').$el = jQuery($('select[data-futu-id=schedule-employees]')).select2();
});
