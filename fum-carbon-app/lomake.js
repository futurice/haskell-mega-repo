lomake = (function () {
  console.info("loading lomake");

  // imports
  var $ = futu.$;
  var $_ = futu.$_;
  var $$ = futu.$$;
  var trace = futu.trace;
  var buttonOnClick = futu.buttonOnClick;

  // forms
  var forms = {};

  // initialisation
  function initialiseForm(formElement) {
    var formName = formElement.dataset.lomakeForm;
    console.info("found lomake: ", formName, formElement);

    // The url where we will submit the form.
    var formSubmitUrl = formElement.dataset.lomakeFormSubmit;

    // Elements
    var inputElements = $$("*[data-lomake-id]", formElement);
    var resetBtn = $_("button[data-lomake-action=reset]", formElement);
    var submitBtn = $_("button[data-lomake-action=submit]", formElement);
    console.log("elements", formName, resetBtn, submitBtn, inputElements);

    // Collect inputs
    var defs = {};

    _.forEach(inputElements, function (el) {
      var elName = el.dataset.lomakeId;

      // jQuery element
      var $el;
      if (el.tagName == "SELECT") {
        var opts = {};
        // dynamic values

        if (el.dataset.lomakeValuesLink) {
          opts.ajax = {
            url: el.dataset.lomakeValuesLink,
            dataType: "json",
            cache: true,
            processResults: function (data) {
              _.forEach(data.results, function (r) {
                // if selected value is not set, pick the empty value if there is one
                if (!el.dataset.lomakeSelectedValue && r.id === "") {
                    r.selected = true;
                // otherwise select value defined
                } else if (el.dataset.lomakeSelectedValue === r.id) {
                    r.selected = true;
                }
              });
              return data;
            }
          };
        }
        $el = jQuery(el).select2(opts);
      } else {
        $el = jQuery(el);
      }

      // we define the def object here, so we can refer to it already.
      var def = { el: el, $el: $el };

      // not used atm.
      // var checkbox = def.el.type === "checkbox";

      // original value, so we can find changed elements.
      def.original$ = menrva.source(inputValue($el));

      // the value in the element
      def.source$ = menrvaInputValue($el);

      // todo:
      var check = undefined;

      // selectbox selector.
      // TODO: non-mandatory selectboxes.
      if (el.tagName === "SELECT") {
        check = function (value) {
          return value === "" || value === "-" ? undefined : value;
        };
      } else if (typeof el.dataset.lomakeRegexp === "string") {
        var checkRegexp = new RegExp(el.dataset.lomakeRegexp);
        trace(elName, "check regexp", checkRegexp);
        check = function (value) {
          var m = value.match(checkRegexp);
          return m ? value : undefined;
        };
      }

      // checked value
      def.signal$ = _.isFunction(check) ? def.source$.map(check) : def.source$;

      // changed = original != source
      // note: check may say it's invalid - but it's still changed!
      def.changed$ = menrva.combine(def.original$, def.source$, function (original, source) {
        return !_.isEqual(original, source);
      });

      // submittable = signal != undefined
      // check function should return `undefined` for invalid values.
      // TODO: diagnostic reporting.
      def.submittable$ = def.signal$.map(function (x) {
        return x !== undefined;
      });

      // dirty = "touched elements".
      def.dirty$ = menrva.source(false);
      $el.blur(function () {
        trace(elName, "blur");
        menrva.transaction([def.dirty$, true]).commit();
      });

      // per element validation.
      menrva.combine(def.dirty$, def.changed$, def.submittable$, function (dirty, changed, submittable) {
        if ((dirty || changed) && !submittable) {
          return "error";
        } else if (changed) {
          return "pending";
        } else {
          return false;
        }
      }).onValue(function (state) {
        if (state === "error") {
          def.el.parentElement.classList.add("error");
          def.el.parentElement.classList.remove("pending");
        } else if (state === "pending") {
          def.el.parentElement.classList.remove("error");
          def.el.parentElement.classList.add("pending");
        } else {
          def.el.parentElement.classList.remove("pending");
          def.el.parentElement.classList.remove("error");
        }
      });

      defs[elName] = def;
    });

    // actions
    function markDirty() {
      var tr = [];
      _.forEach(defs, function (def) {
        tr.push(def.dirty$);
        tr.push(true);
      });
      menrva.transaction(tr).commit();
    }

    function markClean() {
      var tr = [];
      _.forEach(defs, function (def) {
        tr.push(def.dirty$);
        tr.push(false);
        tr.push(def.signal);
        tr.push(inputValue(def.$el));
      });
      menrva.transaction(tr).commit();
    }

    // Form signals
    var formChanged$ = menrva.record(_.mapValues(defs, "changed$")).map(function (rec) {
      return _.chain(rec).values().some().value();
    });

    var formDirty$ = menrva.record(_.mapValues(defs, "dirty$")).map(function (rec) {
      return _.chain(rec).values().some().value();
    });

    var formSubmittable$ = menrva.record(_.mapValues(defs, "submittable$")).map(function (rec) {
      console.log("formSubmittable", rec);
      return _.chain(rec).values().every().value();
    });

    // reset button state
    menrvaSome(formChanged$, formDirty$).onValue(function (changed) {
      resetBtn.disabled = !changed;
    });

    buttonOnClick(resetBtn, function () {
      var tr = [];
      _.forEach(defs, function (def) {
        // we didn't touch the element
        tr.push(def.dirty$);
        tr.push(false);

        // source value should be what the UI shows.
        // TODO: menrva should support setting to the value of other signal!
        // https://github.com/phadej/menrva/issues/16
        tr.push(def.source$);
        tr.push(def.original$.value());
      });

      menrva.transaction(tr).commit();
    });

    // submit button state
    menrva.combine(formSubmittable$, formChanged$, function (submittable, changed) {
      return changed || !submittable;
    }).onValue(function (enabled) {
      submitBtn.disabled = !enabled;
    });

    formSubmittable$.onValue(function (submittable) {
      console.log("toggling submitBtn", submitBtn);
      if (submittable) {
        submitBtn.classList.remove("alert");
        submitBtn.classList.add("success");
      } else {
        submitBtn.classList.add("alert");
        submitBtn.classList.remove("success");
      }
    });

    buttonOnClick(submitBtn, function () {
      if (formSubmittable$.value()) {
        var values = _.mapValues(defs, function (def) {
          return def.signal$.value();
        });

        console.log("Submitting", formSubmitUrl, values);

        // Modal
        var modalElement = document.createElement("DIV");
        modalElement.className = "reveal";
        modalElement.dataset.reveal = "";
        modalElement.innerText = "Thinking..."; // todo some spinner?

        var modal = new Foundation.Reveal(jQuery(modalElement), { closeOnClick: false, closeOnEsc: false });
        modal.open();

        // Request
        postJSON(formSubmitUrl, values)
            .then(function (response) {
                switch (response.tag) {
                    case "LomakeResponseNoop": 
                        modal.close();
                        break;
                    case "LomakeResponseError":
                        throw new Error(response.contents);
                        // break;
                    case "LomakeResponseReload":
                        location.reload();
                        break;
                    case "LomakeResponseRedirect":
                        location.href = response.contents;
                        break;
                    default:
                        throw new Error("Unknown LomakeResponse " + JSON.stringify(response));
                }
            })
            .catch(function (exc) {
                console.error(exc);

                modalElement.innerText = "" + exc;

                var btn = document.createElement("BUTTON");
                btn.className = "button alert";
                btn.innerText = "Close";

                buttonOnClick(btn, function () {
                    modal.close();
                });

                modalElement.appendChild(document.createElement("HR"));
                modalElement.appendChild(btn);
            });
      } else {
        markDirty();
      }
    });

    // write down form
    forms[formName] = {
      markDirty: markDirty,
      markClean: markClean,
    };
  }

  // onload event
  futu.onload(function () {
    console.info("initialising lomake");

    // Initialise forms.
    $$('div[data-lomake-form]').forEach(initialiseForm);
  });

  // Menrva

  // make a menrva.source with bi-directional binding.
  function menrvaInputValue($el) {
    var value$ = menrva.source(inputValue($el), _.isEqual);
    var cb = function () {
      menrva.transaction()
        .set(value$, inputValue($el))
        .commit();
    };
    $el.change(cb);

    value$.onValue(function (value) {
      setInputValue($el, value);
    });

    return value$;
  }

  function menrvaSome() {
    var signals = _.toArray(arguments);
    return menrva.sequence(signals).map(function (values) {
      return _.some(values);
    });
  }

  // DOM
  function inputValue($el) {
    return $el.val();
  }

  function setInputValue($el, value) {
    $el.val(value).change(); // change for select2
  }

  // Fetch
  function postJSON(url, body) {
    // absolute urls only!
    if (url[0] !== "/") {
      url = "/" + url;
    }

    var headers = new Headers();
    headers.append("Accept", "application/json");
    headers.append("Content-Type", "application/json");

    var opts = {
      method: "POST",
      headers: headers,
      credentials: "same-origin",
      body: JSON.stringify(body),
    };

    return fetch(url, opts).then(function (res) {
        if (res.status !== 200) {
          throw new Error("Non-200 status: " + res.status);
        }

        var contentType = res.headers.get("content-type");
        if (contentType && contentType.indexOf("application/json") !== -1) {
          return res.json();
        } else {
          return res.text().then(function (txt) {
            throw new Error("Not a JSON: " + txt);
          });
        }
      });
  }

  // exports
  return forms;
}());
