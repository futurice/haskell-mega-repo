futu.onload(function () {
  // imports
  var $  = futu.$;
  var $_ = futu.$_;
  var $$ = futu.$$;
  var assert = futu.assert;
  var buttonOnClick = futu.buttonOnClick;

  $$("button[data-futu-post-button]").forEach(updateDepartDate);

  function updateDepartDate(btn) {
    var url = btn.dataset.futuPostButton;
    btn.disabled = false;

    buttonOnClick(btn, function () {
      if (btn.disabled) return;
      btn.disabled = true;

      console.log("Posting to URL", url);
      futu.commandFetchJSON(url, {})
        .then(function (res) {
          btn.className = "button success";
        })
        .catch(function (exc) {
          btn.className = "button alert";
        });
    });
  };
});
