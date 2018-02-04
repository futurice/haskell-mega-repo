document.addEventListener("DOMContentLoaded", function () {
  console.log("Initialising early caring");

  var buttons = $$("button[data-futu-early-caring-mail]");
  buttons.forEach(function (btn) {
      buttonOnClick(btn, function () {
          btn.disabled = true;

          var url = "/early-caring-submit";

          var headers = new Headers();
          headers.append("Accept", "application/json");
          headers.append("Content-Type", "application/json");

          var opts = {
            method: "POST",
            headers: headers,
            credentials: "same-origin",
            body: btn.dataset.futuEarlyCaringMail,
          };

          fetch(url, opts)
              .then(function (res) {
                  return res.text();
              })
              .then(function (res) {
                  if (res === "true") {
                      btn.className = "button success";
                  } else {
                      throw new Error("Didn't get 'true': " + res);
                  }
              })
              .catch(function (exc) {
                    console.error(exc);
                    btn.className = "button alert";
              });
      });
  });

  function $$(selector, el) {
    el = el || document;
    var res = el.querySelectorAll(selector, el);
    return Array.prototype.slice.call(res);
  }

  function assert(cond, msg) {
    if (!cond) {
      console.error(msg);
      throw new Error(msg);
    }
  }

  function buttonOnClick(btn, callback) {
    btn.addEventListener("click", function (e) {
      e.preventDefault();
      callback(e);
      return false;
    });
  }
});
