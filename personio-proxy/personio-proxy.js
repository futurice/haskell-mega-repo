futu.onload(function () {
  // imports
  var $  = futu.$;
  var $_ = futu.$_;
  var $$ = futu.$$;
  var assert = futu.assert;
  var buttonOnClick = futu.buttonOnClick;

  var cmdE = $_("#cmd");
  var helpE = $_("#help");
  var tableE = $_("#table");
  var singleE = $_("#single");
  var rowsE = $$("tr[data-futu-keywords]");

  var KEYWORDS = [ ":help" ] // More keywords are added from data

  console.log("Initialising personio proxy");

  var TRIPLES = []

  rowsE.forEach(function (row) {
    var words = JSON.parse(row.dataset.futuWords);
    var keywords = JSON.parse(row.dataset.futuKeywords);
    keywords.forEach(function (k) {
      if (KEYWORDS.indexOf(k) === -1) {
        KEYWORDS.push(k);
      }
    });

    TRIPLES.push([row, words, keywords]);
  });

  console.log("KEYWORDS", KEYWORDS);

  var cmd$ = menrvaInputValue(jQuery(cmdE));
  var query$ = cmd$.map(toQuery, _.isEqual);
  var predicate$ = query$.map(function (query) {
    return function (words, keywords) {
      var w = query.words.length === 0 ? true : allmatchesPartial(query.words, words);
      var k = query.keywords.length === 0 ? true : allmatches(query.keywords, keywords);
      return w && k;
    };
  });

  // visibility

  var helpVisible$ = query$.map(function (q) {
    return _.includes(q.keywords, ":help");
  });

  helpVisible$.onValue(function (b) {
    helpE.style.display = b ? "block" : "none";
  });

  // single match

  var singleMatch$ = predicate$.map(function (predicate) {

    var acc = null;
    for (var i = 0; i < TRIPLES.length; i++) {
      var triple = TRIPLES[i];
      var matches = predicate(triple[1], triple[2]);
      if (matches) {
        if (acc === null) { acc = triple; }
        else { return null; }
      }
    }

    return acc;
  });

  singleMatch$.onValue(function (v) { if (v !== null) {
    var e = JSON.parse(v[0].dataset.futuEmployee);
    console.log("single", e);
    $_("#s-fullname").innerText = e.first + " " + e.last;

    setText($_("#s-email"), e.email);
    setText($_("#s-office"), e.office);
    setText($_("#s-tribe"), e.tribe);
    setText($_("#s-position"), e.position);
    setText($_("#s-country"), e.country);
    setText($_("#s-employer"), e.employer);
    setText($_("#s-phone"), e.workPhone);
    setText($_("#s-role"), e.role);

    setText($_("#s-login"), e.login);
    setText($_("#s-github"), e.github);
    setText($_("#s-flowdock"), e.flowdock);

    setText($_("#s-status"), e.status);
    setText($_("#s-employment-type"), e.employmentType);
    setText($_("#s-hire-date"), e.hireDate);
    setText($_("#s-contract-ends"), e.endDate);
    setText($_("#s-cost-center"), e.costCenter);
    setText($_("#s-supervisor"), e.supervisor);
    setText($_("#s-weekly-hours"), e.weeklyHours);
    setText($_("#s-contract-type"), e.contractType);
    setText($_("#s-salary-type"), e.salaryType);
    setText($_("#s-hr-number"), e.hRNumber);
    setText($_("#s-expat"), e.expat);
  }});

  function setText(element, x) {
    if (x === undefined) {
      return element.innerHTML = '<span class="label secondary">not set</span>';
    } else if (x === "") {
      return element.innerHTML = '<span class="label secondary">empty string</span>';
    } else if (x === true) {
      return element.innerText = "True";
    } else if (x === false) {
      return element.innerText = "False";
    } else if (x === "Active") {
      return element.innerHTML = '<span class="label success">Active</span>';
    } else if (x === "Inactive") {
      return element.innerHTML = '<span class="label alert">Inactive</span>';
    } else if (x === "internal") {
      return element.innerHTML = '<span class="label success">Internal</span>';
    } else if (x === "external") {
      return element.innerHTML = '<span class="label primary">External</span>';
    } else {
      return element.innerText = x;
    }
  }

  // more visibility

  var tableVisible$ = menrva.combine(helpVisible$, function (hv, sm) {
    return !hv;
  });

  tableVisible$.onValue(function (b) {
    tableE.style.display = b ? "block" : "none";
  });

  var singleVisible$ = menrva.combine(helpVisible$, singleMatch$, function (hv, sm) {
    return !hv && sm !== null; 
  });

  singleVisible$.onValue(function (b) {
    singleE.style.display = b ? "block" : "none";
  });

  // query

  predicate$.onValue(function (predicate) {
    TRIPLES.forEach(function (triple) {
      var matches = predicate(triple[1], triple[2]);
      triple[0].style.display = matches ? "table-row" : "none";
    });
  });

  // tab completion

  jQuery(cmdE).keydown(function (e) {
    if (e.keyCode == 9) {
      var words = this.value.split(/ /);
      var lastWord = words[words.length - 1]
      if (lastWord[0] === ":") {
        var keywords = KEYWORDS.filter(function (k) {
          return k.substr(0, lastWord.length) === lastWord
        });

        // single match
        if (keywords.length === 1) {
          words.pop();
          words.push(keywords[0]);
          this.value = words.join(" ") + " ";
        }
        // todo multiple matches

        console.log(words[words.length - 1], keywords);
      }
      e.preventDefault();
      return false;
    }
  });

  // Helpers
  function toQuery(cmd) {
    var words    = cmd.split(/ +/);
    var keywords = words.filter(function (w) { return w[0] === ":"; }).map(function (w) { return w.toLowerCase(); });
    var other    = words.filter(function (w) { return w.length > 2 && w[0] !== ":"; }).map(function (w) { return w.toLowerCase(); });

    return {
      keywords: _.sortBy(_.intersection(keywords, KEYWORDS)),
      words: _.sortBy(other),
    };
  }

  function allmatches(xs, ys) {
    for (var i = 0; i < xs.length; i += 1) {
      var x = xs[i];
      if (!_.includes(ys, x)) {
        return false;
      }
    }
    return true;
  }

  function allmatchesPartial(xs, ys) {
    for (var i = 0; i < xs.length; i += 1) {
      var x = xs[i];

      if (!_.some(ys, function (y) { return _.includes(y, x); })) {
        return false
      }
    }
    return true;
  }

  // Copied from futu.js

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

  // DOM
  function inputValue($el) {
    return $el.val();
  }

  function setInputValue($el, value) {
    $el.val(value).change(); // change for select2
  }
});
