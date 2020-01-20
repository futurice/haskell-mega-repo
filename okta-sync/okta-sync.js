futu.onload(function () {
    //imports
    var $  = futu.$;
    var $_ = futu.$_;
    var $$ = futu.$$;
    var assert = futu.assert;
    var buttonOnClick = futu.buttonOnClick;

    var addUsersButton = $("button#add-users");
    if (addUsersButton) {
        console.log("Initialising okta-sync");

        // Remove users
        var addUsersSrc$ = menrva.source([]);
        var addUsers$ = addUsersSrc$.map(function (xs) {
            return _.sortedUniq(_.sortBy(xs));
        }, _.isEqual);

        var checkboxes = $$("input[data-okta-add-user]");
        console.log(checkboxes);
        checkboxes.forEach(function (chk) {
            var ghLogin = chk.dataset.oktaAddUser;
            chk.addEventListener("change", function () {
                if (chk.checked) {
                    menrva.transaction([
                        addUsersSrc$, function (xs) {
                            return xs.concat([ghLogin]);
                        }]).commit();
                } else {
                    menrva.transaction([
                        addUsersSrc$, function (xs) {
                            return xs.filter(function (x) {
                                return x !== ghLogin;
                            });
                        }]).commit();
                }
            });
        });

        addUsers$.onValue(function (xs) {
            addUsersButton.disabled = xs.length === 0;
        });

        buttonOnClick(addUsersButton, function () {
            if (addUsersButton.disabled) return;
            addUsersButton.disabled = true;

            var addUsers = addUsers$.value();
            console.log("Adding users", addUsers);
            futu.commandFetchJSON("/command/remove-users", addUsers)
                .then(function (res) {
                    addUsersButton.className = "button success";
                })
                .catch(function (exc) {
                    addUsersButton.className = "button alert";
                });
        });
    }
});
