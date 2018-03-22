futu.onload(function () {
  // imports
  var $  = futu.$;
  var $_ = futu.$_;
  var $$ = futu.$$;
  var assert = futu.assert;
  var buttonOnClick = futu.buttonOnClick;

  var btnRemove = $("button#remove-users");
  if (btnRemove) {
    console.log("Initialising github-sync");

    // Remove users
    var removeUsersSrc$ = menrva.source([])
    var removeUsers$ = removeUsersSrc$.map(function (xs) {
      return _.sortedUniq(_.sortBy(xs));
    }, _.isEqual)

    var checkboxes = $$("input[data-futu-remove-user]");
    checkboxes.forEach(function (chk) {
      var ghLogin = chk.dataset.futuRemoveUser;
      chk.addEventListener("change", function () {
        if (chk.checked) {
          menrva.transaction([
            removeUsersSrc$, function (xs) {
              return xs.concat([ghLogin]);
            }]).commit();
        } else {
          menrva.transaction([
            removeUsersSrc$, function (xs) {
              return xs.filter(function (x) {
                return x !== ghLogin;
              });
            }]).commit();
        }
      });
    });
    //dbtnRemove.disabled = false;

    removeUsers$.onValue(function (xs) {
      btnRemove.disabled = xs.length === 0;
    });

    buttonOnClick(btnRemove, function () {
      if (btnRemove.disabled) return;
      btnRemove.disabled = true;
       
      var removeUsers = removeUsers$.value();
      console.log("Removing users", removeUsers);
      futu.fetchJSON("/command/remove-users", removeUsers)
          .then(function (res) {
            if (res === true) {
                btnRemove.className = "button success";
            } else {
                throw new Error("Didn't get 'true': " + res);
            }
          })
          .catch(function (exc) {
            alert("" + exc);
            console.error(exc);
            btnRemove.className = "button alert";
            throw exc;
          });
    });

    // Invite users

    // TODO
  }
});
