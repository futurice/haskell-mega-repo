futu.onload(function () {
  // imports
  var $  = futu.$;
  var $_ = futu.$_;
  var $$ = futu.$$;
  var assert = futu.assert;
  var buttonOnClick = futu.buttonOnClick;

  var btnAll = $("button#futu-early-caring-send-all")
  if (btnAll) {
    console.log("Initialising early caring");

    // individual buttons
    var buttons = $$("button[data-futu-early-caring-mail]");
    var callbacks = [];
    buttons.forEach(function (btn) {
      btn.disabled = false;
      var callback = function () {
        if (btn.disabled) return;

        btn.disabled = true;

        console.info("Sending email to " + btn.dataset.futuEarlyCaringName);
        futu.fetchJSON("/early-caring-submit", JSON.parse(btn.dataset.futuEarlyCaringMail))
          .then(function (res) {
            if (res === true) {
                btn.className = "button success";
            } else {
                throw new Error("Didn't get 'true': " + res);
            }
          })
          .catch(function (exc) {
            alert("" + exc);
            console.error(exc);
            btn.className = "button alert";
            throw exc;
          });
      };

      buttonOnClick(btn, callback);
      callbacks.push(callback);
    });

    // send all list
    var allList = $_("ul#futu-early-caring-all-supervisors");
    buttons.forEach(function (btn) {
      var li = document.createElement("LI");
      li.innerText = btn.dataset.futuEarlyCaringName;
      allList.appendChild(li);
    });
    
    // send all button
    btnAll.disabled = false;
    buttonOnClick(btnAll, function () {
      btnAll.disabled = true;
      callbacks.forEach(function (cb) { cb (); });
    });
  }

    var ignoreUsersSrc$ = menrva.source([]);
    var ignoreUsers$ = ignoreUsersSrc$.map(function (xs) {
        return _.sortedUniq(_.sortBy(xs));
    }, _.isEqual);

    var checkboxes = $$("input[data-futu-vacation-report-ignore-user]");
    checkboxes.forEach(function (chk) {
        chk.addEventListener("change", function () {
        var personId = chk.dataset.futuVacationReportIgnoreUser;
        if (chk.checked) {
          menrva.transaction([
            ignoreUsersSrc$, function (xs) {
              return xs.concat([personId]);
            }]).commit();
            chk.parentNode.parentNode.style = "color:#ccc";
        } else {
          menrva.transaction([
            ignoreUsersSrc$, function (xs) {
              return xs.filter(function (x) {
                return x !== personId;
              });
            }]).commit();
            chk.parentNode.parentNode.style = "";
        }
        console.log(ignoreUsers$.value());
      });
    });

  $$("button[data-futu-vacation-report-send-all]").forEach(sendVacationReport);

  function sendVacationReport(btn) {
    var url = btn.dataset.futuVacationReportSendAll;
    btn.disabled = false;

    buttonOnClick(btn, function () {
      if (btn.disabled || !confirm("Send vacation email to all?")) return;
      btn.disabled = true;

      console.log("Posting to URL", url);
        futu.commandFetchJSON(url, ignoreUsers$.value().map(x => parseInt(x)))
        .then(function (res) {
            btn.className = "button success";
            checkboxes.forEach(function (chk) { chk.checked = false;});
            menrva.transaction([
                ignoreUsersSrc$, function (xs) {
                    return [];
                }]).commit();
        })
        .catch(function (exc) {
          btn.className = "button alert";
        });
    });
  };

});
