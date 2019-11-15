futu.onload(function () {
  console.log("Initialising prox-mgmt");

  var $ = futu.$;
  var $$ = futu.$$;
  var $_ = futu.$_;
  var buttonOnClick = futu.buttonOnClick;

  var regenButton = $("button#futu-regenerate-token");
  if (regenButton) {
    regenButton.disabled = false;
    buttonOnClick(regenButton, function () {
      regenButton.disabled = true;

      futu.fetchJSON("/regenerate-own-token", {})
        .then(function (token) {
          $_("#futu-token-code").innerText = token;
          $_("#futu-token-info").style.display = "block";
        })
        .catch(function (exc) {
          alert("" + exc);
        });
    });
  }
    var serviceRegenButtons = $$("button#futu-regenerate-service-token");
    serviceRegenButtons.forEach(function (btn) {
        btn.disabled = false;
        buttonOnClick(btn, function () {
            btn.disabled = true;
            var serviceUser = btn.dataset.serviceUsername;

            futu.fetchJSON("/regenerate-service-token", serviceUser)
                .then(function (token) {
                    $_("#futu-service-token-user").innerText = serviceUser;
                    $_("#futu-service-token-code").innerText = token;
                    $_("#futu-service-token-info").style.display = "block";
                })
                .catch(function (exc) {
                    alert("" + exc);
                });
        });
    });
});
