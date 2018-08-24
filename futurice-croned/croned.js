futu.onload(function () {
  // imports
  var $  = futu.$;
  var $_ = futu.$_;
  var $$ = futu.$$;
  var assert = futu.assert;
  var buttonOnClick = futu.buttonOnClick;

  var btn = $("#futu-trigger");
  if (btn) {
    futu.buttonOnClick(btn, function () {
      if (btn.disabled) return;
      btn.disabled = true;
       
      futu.fetchJSON("/trigger", {}).then(function () {
        location.reload();
      });
    });
  }

  var stdout = $("#futu-stdout");
  var stderr = $("#futu-stderr");
  function updateStreams() {
    console.log("update...");
    futu.fetchJSON("/outputs").then(function (out) {
      stdout.innerText = out.outStdout;
      stderr.innerText = out.outStderr;
    });
  }
  window.setInterval(updateStreams, 1000);
});
