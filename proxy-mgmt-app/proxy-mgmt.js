futu.onload(function () {
  console.log("Initialising prox-mgmt");

  var $ = futu.$;
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
});
