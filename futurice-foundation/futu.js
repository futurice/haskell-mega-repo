futu = (function () {
  console.info("loading futu");

  function $(selector, el) {
    el = el || document;
    return el.querySelector(selector);
  }

  // mandatory element
  function $_(selector, el) {
    el = el || document;
    var res = el.querySelector(selector);
    assert(res, "Non-existing element for selector: " + selector);
    return res;
  }

  function $$(selector, el) {
    el = el || document;
    var res = el.querySelectorAll(selector);
    return Array.prototype.slice.call(res);
  }

  function assert(cond, msg) {
    if (!cond) {
      console.error(msg);
      throw new Error(msg);
    }
  }

  // fetch
  function fetchJSON(url, postPayload) {
    var headers = new Headers();
    headers.append("Accept", "application/json");

    var opts = {
      method: "GET", // by default GET, if postPayload provided: POST.
      headers: headers,
      credentials: "same-origin",
    };

    if (postPayload !== undefined) {
      opts.method = "POST";
      headers.append("Content-Type", "application/json");
      opts.body = JSON.stringify(postPayload);
    }

    return fetch(url, opts)
      .then(function (res) {
        var contentType = res.headers.get("Content-Type");
        if (contentType && contentType.indexOf("application/json") !== -1) {
          return res.json();
        } else {
          return res.text().then(function (txt) {
            throw new Error("Not a JSON: " + txt);
          });
        }
      });
  }

  // command fetch json!
  function commandFetchJSON(url, postPayload) {
      var modalElement = document.createElement("DIV");
      modalElement.className = "reveal";
      modalElement.dataset.reveal = "";
      modalElement.innerText = "Thinking...";

      var modal = new Foundation.Reveal(jQuery(modalElement), { closeOnClick: false, closeOnEsc: false });
      modal.open();

      // Request
      return fetchJSON(url, postPayload)
          .then(function (response) {
              switch (response.tag) {
                  case "CommandResponseOk":
                      modal.close();
                      return response.contents
                  case "CommandResponseError":
                      throw new Error(response.contents);
                      // break;
                  case "CommandResponseReload":
                      location.reload();
                      break;
                  case "CommandResponseRedirect":
                      location.href = response.contents;
                      break;
                  default:
                      throw new Error("Unknown CommandResponse " + JSON.stringify(response));
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

              throw exc;
          });
  }

  // if _.isEqual is Eq
  // then compare is Ord
  function compare(a, b) {
      return 0; // todo
  }

  function trace() {
    // return;
    var args = ["TRACE"].concat(_.toArray(arguments));
    console.log.apply(console, args);
  }

  /*
  function traceCall(f, args) {
    var args = [f.name].concat(_.toArray(args));
    console.info.apply(console, args);
  }
  */

  function buttonOnClick(btn, callback) {
    btn.addEventListener("click", function (e) {
      e.preventDefault();
      callback(e);
      return false;
    });
  }

  // global initialisation

  var onloadCallbacks = [];

  function onload(f) {
    if (typeof f === "function") {
      onloadCallbacks.push(f);
    } else {
      console.error("Onload: trying to register a non function", f);
    }
  }

  function linkButton(btn) {
    btn.disabled = false;
    buttonOnClick(btn, function () {
      location.href = btn.dataset.futuLinkButton;
    });
  }

  function sortableTable(tbl) {
    var thead = $("thead", tbl);
    var tbody = $("tbody", tbl);

    if (!thead || !tbody) {
      console.log("warn: no thead or tbody in sortable tble");
    }

    var index = -1;
    var order = "asc";

    var children = _.toArray(tbody.children).filter(function (c) {
      return c.tagName === "TR";
    });

    function orderTable() {
      // "metric"
      function iteratee(tr) {
        var tds = _.toArray($$("td", tr));
        var td = tds[index];

        // no cell: return null
          if (!td) return null;

          if(td.innerText.match(/\d+.\d+\sh/)) {
              return parseFloat(td.innerText);
          }

        // otherwise return inner text
        return td.innerText;
      }

      // sort
      children = _.orderBy(children, [iteratee], [order]);

      // remove children
      children.forEach(function (c) { tbody.removeChild(c); });

      // put them back
      // as they are sorted in the array, they will be sorted in DOM.
      children.forEach(function (c) { tbody.appendChild(c); });
    }

    // header click handlers
    $$("th", thead).forEach(function (th, i) {
      if (!$("a", th)) {
        th.style.cursor = "pointer";
        th.addEventListener("click", function () {
          if (index === i) {
            order = order === "desc" ? "asc" : "desc";
          } else {
            index = i;
            order = "asc";
          }

          orderTable();
        });
      }
    });
  }

  document.addEventListener("DOMContentLoaded", function () {
    console.info("initialising futu");

    // use jQuery datepickers
    $$("input[type=date]").forEach(function (el) {
      el.type = "text";
      jQuery(el). datepicker({
        dateFormat: "yy-mm-dd",
        onSelect: function (dateText, inst) {
          this.value = dateText;
          this.dispatchEvent(new Event("change"));
        },
      });
    });

    // Link buttons
    $$("button[data-futu-link-button]").forEach(linkButton);

    // Sortable tables
    $$("table[data-futu-sortable-table=true]").forEach(sortableTable);

    // registered callbacks
    onloadCallbacks.forEach(function (f) {
      f();
    });
  });

  // exports

  return {
    $: $,
    $_: $_,
    $$: $$,
    assert: assert,
    onload: onload,
    buttonOnClick: buttonOnClick,
    trace: trace,
    fetchJSON: fetchJSON,
    commandFetchJSON: commandFetchJSON,
  };
}());

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

    // The "good" class for submit button
    var submitButtonClass = formElement.dataset.lomakeSubmitButtonClass || "success";

    // Elements
    var inputElements = $$("*[data-lomake-id]", formElement);
    var resetBtn = $("button[data-lomake-action=reset]", formElement);
    var submitBtn = $_("button[data-lomake-action=submit]", formElement);
    trace("elements", formName, resetBtn, submitBtn, inputElements);

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

    // Can change?
    // form with all hidden inputs cannot change,
    // so submit button will be enable before change
    var formCannotChange = _.chain(defs).values().map(function (def) {
        return def.el.tagName === "INPUT" && def.el.type === "hidden";
    }).every().value();

    // Form signals
    var formChanged$ = formCannotChange ? menrva.source(true) :
      menrva.record(_.mapValues(defs, "changed$")).map(function (rec) {
        return _.chain(rec).values().some().value();
      });

    var formDirty$ = menrva.record(_.mapValues(defs, "dirty$")).map(function (rec) {
      return _.chain(rec).values().some().value();
    });

    var formSubmittable$ = menrva.record(_.mapValues(defs, "submittable$")).map(function (rec) {
      return _.chain(rec).values().every().value();
    });

    // reset button state
    if (resetBtn) {
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
    }

    // submit button state
    menrva.combine(formSubmittable$, formChanged$, function (submittable, changed) {
      return changed || !submittable;
    }).onValue(function (enabled) {
      trace("toggling submitBtn disable", formName, submitBtn);
      submitBtn.disabled = !enabled;
    });

    formSubmittable$.onValue(function (submittable) {
      trace("toggling submitBtn class", formName, submitBtn);
      if (submittable) {
        submitBtn.classList.remove("alert");
        submitBtn.classList.add(submitButtonClass);
      } else {
        submitBtn.classList.add("alert");
        submitBtn.classList.remove(submitButtonClass);
      }
    });

    buttonOnClick(submitBtn, function () {
      if (formSubmittable$.value()) {
        var values = _.mapValues(defs, function (def) {
          return def.signal$.value();
        });

        trace("Submitting", formSubmitUrl, values);

        futu.commandFetchJSON(formSubmitUrl, values);
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
    $el.keyup(cb);

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
