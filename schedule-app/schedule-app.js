futu.onload(function () {
    console.info("Initialising schedule js");
    // imports
    var $ = futu.$;
    var $_ = futu.$_;
    var $$ = futu.$$;
    var assert = futu.assert;
    var buttonOnClick = futu.buttonOnClick;

    $$('select[data-futu-id=schedule-employees]').$el = jQuery($$('select[data-futu-id=schedule-employees]')).select2();
    $$('select[data-futu-id=schedule-locations]').$el = jQuery($$('select[data-futu-id=schedule-locations]')).select2();

    $$('button[data-futu-id=delete-template]').forEach(function (btn) {
        buttonOnClick(btn, function () {
            btn.disabled = true;
            var templateId = btn.dataset.scheduleTemplate;
            fetch("/schedule-template",
                  { method: "DELETE",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify(templateId)})
                .then(function (response) { console.log("Success"); location=location.href; })
                .catch(function (error) { console.log("Got error " + error); });
        });
    });
    jQuery(document).foundation();
});
