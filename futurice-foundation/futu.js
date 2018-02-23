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
    onloadCallbacks.push(f);
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
  };
}());
