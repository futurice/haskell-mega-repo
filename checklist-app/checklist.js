futu.onload(function () {
  console.info("Initialising checklist js");

  // imports
  var $ = futu.$;
  var $_ = futu.$_;
  var $$ = futu.$$;
  var assert = futu.assert;
  var buttonOnClick = futu.buttonOnClick;

  var SELECT2_OPTS = {
    minimumResultsForSearch: 10,
  };

  // Reload indicator
  var futuReloadIndicatorEl = $_("#futu-reload-indicator");
  buttonOnClick(futuReloadIndicatorEl, function () {
    // todo: check: no pending.
    location.reload();
  });
  var futuReloadIndicator$ = menrva.source({ total: 0, done: 0 }, _.isEqual);
  futuReloadIndicator$.onValue(function (val) {
    futuReloadIndicatorEl.innerText = val.done + "/" + val.total;
    futuReloadIndicatorEl.style.display = val.total === 0 ? "none" : "";
  });

  $$("form").forEach(function (form) {
    var formId = form.dataset.futuId;

    switch (formId) {
      case "selector": return selectorForm(form);
      case "employee-create": return employeeCreateForm(form);
      case "employee-edit": return employeeEditForm(form);
      case "checklist-create": return checklistCreateForm(form);
      case "checklist-edit": return checklistEditForm(form);
      case "task-create": return taskCreateForm(form);
      case "task-edit": return taskEditForm(form);
      case "task-add": return taskAddForm(form);
      default: return unknownForm(form);
    }
  });

  $$("button[data-futu-id=task-remove]").forEach(taskRemoveBtn);
  $$("button[data-futu-id=employee-remove]").forEach(employeeRemoveBtn);
  $$("button[data-futu-id=employee-archive]").forEach(employeeArchiveBtn);
  $$("input[data-futu-id=task-done-checkbox]").forEach(taskToggleCheckbox);
  $$("input[data-futu-id=task-comment-editbox]").forEach(taskCommentEditInput);
  $$("button[data-futu-link-button]").forEach(linkButton);
  $$("div[data-futu-id=error-callout] button").forEach(function (btn) {
    buttonOnClick(btn, function () {
      var el = $("div[data-futu-id=error-callout]");
      if (el) {
        el.style.display = "none";
      }
    });
  });
  $$("input[type=text][data-futu-values]").forEach(function (el) {
    var values = JSON.parse(el.dataset.futuValues);
    console.log("Autocomplete", el, values);
    jQuery(el).autocomplete({
      source: values,
    });
  });

  function unknownForm(form) {
    console.warn("Unknown form", form.dataset.futuId, form);

    function disable(el) {
      el.disabled = true;
    }

    $$("button", form).forEach(disable);
    $$("input", form).forEach(disable);
    $$("select", form).forEach(disable);
    $$("textarea", form).forEach(disable);
  }

  function selectorForm(form) {
    console.info("Initialising selector form");
    var btn = $_("button", form);
    var inputs = $$("select, input", form).map(function (el) {
      if (el.tagName === "SELECT") {
        return [el.name, jQuery(el).select2(SELECT2_OPTS)];
      } else {
        return [el.name, jQuery(el)];
      }
    });

    buttonOnClick(btn, function () {
      var querys = [];
      inputs.forEach(function (p) {
        var name = p[0];
        var $el = p[1];
        var value = inputValue($el);
        if (value !== "" && value !== false) {
          querys.push(name + "=" + encodeURIComponent(value));
        }
      });

       location.search = "?" + querys.join("&");
    });
  }

  function employeeCreateForm(form) {
    console.info("Initialising employee creation form");

    var defs = {
      checklistId: { sel: "select[data-futu-id=employee-checklist", check: nonEmptyCheck },
      personio: { sel: "input[data-futu-id=employee-personio" },
      firstName: { sel: "input[data-futu-id=employee-firstname", check: nonEmptyCheck },
      lastName: { sel: "input[data-futu-id=employee-lastname", check: nonEmptyCheck },
      contractType: { sel: "select[data-futu-id=employee-contract-type", check: nonEmptyCheck },
      location: { sel: "select[data-futu-id=employee-location", check: nonEmptyCheck },
      confirmed: { sel: "input[data-futu-id=employee-confirmed" },
      startingDay: { sel: "input[data-futu-id=employee-starting-day", check: dayCheck },
      supervisor: { sel: "input[data-futu-id=employee-supervisor", check: nonEmptyCheck },
      tribe: { sel: "select[data-futu-id=employee-tribe", check: nonEmptyCheck },
      info: { sel: "textarea[data-futu-id=employee-info" },
      phone: { sel: "input[data-futu-id=employee-phone" },
      contactEmail: { sel: "input[data-futu-id=employee-contact-email" },
      fumLogin: { sel: "input[data-futu-id=employee-fum-login" },
      hrNumber: { sel: "input[data-futu-id=employee-hr-number", check: optionalCheck(numberCheck) },
      notice: { sel: "input[data-futu-id=employee-notice" },
    };

    var actions = initialiseFormDefs(defs, form);

    initialiseSubmitButton(actions.submitBtn, defs, actions, function (values) {
      var checklistId = values.checklistId;
      var edit = _.omit(values, "checklistId");
      cmdCreateEmployee(checklistId, edit);
    });
  }

  function employeeEditForm(form) {
    var employeeId = form.dataset.futuEmployeeId;
    console.info("Initialising employee editing form: " + employeeId);

    var defs = {
      firstName: { sel: "input[data-futu-id=employee-firstname", check: nonEmptyCheck },
      lastName: { sel: "input[data-futu-id=employee-lastname", check: nonEmptyCheck },
      contractType: { sel: "select[data-futu-id=employee-contract-type" },
      location: { sel: "select[data-futu-id=employee-location" },
      confirmed: { sel: "input[data-futu-id=employee-confirmed" },
      startingDay: { sel: "input[data-futu-id=employee-starting-day", check: dayCheck },
      supervisor: { sel: "input[data-futu-id=employee-supervisor", check: nonEmptyCheck },
      tribe: { sel: "select[data-futu-id=employee-tribe", check: nonEmptyCheck },
      info: { sel: "textarea[data-futu-id=employee-info" },
      phone: { sel: "input[data-futu-id=employee-phone" },
      contactEmail: { sel: "input[data-futu-id=employee-contact-email" },
      fumLogin: { sel: "input[data-futu-id=employee-fum-login" },
      hrNumber: { sel: "input[data-futu-id=employee-hr-number", check: optionalCheck(numberCheck) },
      notice: { sel: "input[data-futu-id=employee-notice" },
    };

    var actions = initialiseFormDefs(defs, form);

    initialiseSubmitButton(actions.submitBtn, defs, actions, function (values) {
      cmdEditEmployee(employeeId, values);
    });
  }

  function taskCreateForm(form) {
    console.info("Initialising task creation form");

    var defs = {
      name: { sel: "input[data-futu-id=task-name]", check: nonEmptyCheck },
      info: { sel: "input[data-futu-id=task-info]" },
      role: { sel: "select[data-futu-id=task-role]", check: nonEmptyCheck },
      offset: { sel: "input[data-futu-id=task-offset]", check: numberCheck },
      app: { sel: "input[data-futu-id=task-app]", check: applianceCheck },
      prereqs: { sel: "select[data-futu-id=task-prereqs", check: isArrayCheck },
      tags: { sel: "select[data-futu-id=task-tags", check: isArrayCheck },
      comment: { sel: "input[data-futu-id=task-comment" },
      list1: { sel: "select[data-futu-id=task-checklist-1]" },
      list2: { sel: "select[data-futu-id=task-checklist-2]" },
    }

    var actions = initialiseFormDefs(defs, form);

    initialiseSubmitButton(actions.submitBtn, defs, actions, function (values) {
      var lists = [];
      if (values.list1) { lists.push(values.list1) }
      if (values.list2) { lists.push(values.list2) }

      var edit = {
        name: values.name,
        info: values.info,
        role: values.role,
        offset: values.offset,
        app: values.app,
        prereqs: values.prereqs,
        tags: values.tags
      };

      cmdCreateTask(edit, lists);
    });
  }

  function taskEditForm(form) {
    var taskId = form.dataset.futuTaskId;

    console.info("Initialising task editing form: " + taskId);

    var defs = {
      name: { sel: "input[data-futu-id=task-name]", check: nonEmptyCheck },
      info: { sel: "input[data-futu-id=task-info]" },
      role: { sel: "select[data-futu-id=task-role]", check: nonEmptyCheck },
      offset: { sel: "input[data-futu-id=task-offset]", check: numberCheck },
      app: { sel: "input[data-futu-id=task-app]", check: applianceCheck },
      prereqs: { sel: "select[data-futu-id=task-prereqs", check: isArrayCheck },
      tags: { sel: "select[data-futu-id=task-tags", check: isArrayCheck },
      comment: { sel: "input[data-futu-id=task-comment" },
    };

    var actions = initialiseFormDefs(defs, form);

    initialiseSubmitButton(actions.submitBtn, defs, actions, function (values) {
      cmdEditTask(taskId, values);
    });
  }

  function taskAddForm(form) {
    var checklistId = form.dataset.futuChecklistId;

    console.info("Initialising task addition form", checklistId);

    var defs = {
      task: { sel: "select[data-futu-id=task-id]", check: nonEmptyCheck },
    };

    var actions = initialiseFormDefs(defs, form);

    initialiseSubmitButton(actions.submitBtn, defs, actions, function (values) {
      cmdAddTask(checklistId, values.task);
    });
  }

  function linkButton(btn) {
    buttonOnClick(btn, function () {
      location.href = btn.dataset.futuLinkButton;
    });
  }

  function taskRemoveBtn(btn) {
    buttonOnClick(btn, function () {
      // prevent dbl-click
      btn.disabled = true;

      var checklistId = btn.dataset.futuChecklistId;
      var taskId = btn.dataset.futuTaskId;

      cmdRemoveTask(checklistId, taskId);
    });
  }

  function employeeRemoveBtn(btn) {
    buttonOnClick(btn, function () {
      // prevent dbl-click
      btn.disabled = true;

      var employeeId = btn.dataset.futuEmployeeId;

      if (employeeId && confirm("Delete employee?")) {
        cmdRemoveEmployee(employeeId);
      } else {
        btn.disabled = false;
      }
    });
  }

  function employeeArchiveBtn(btn) {
    buttonOnClick(btn, function () {
      // prevent dbl-click
      btn.disabled = true;

      var employeeId = btn.dataset.futuEmployeeId;

      if (employeeId && confirm("Archive employee?")) {
        cmdArchiveEmployee(employeeId);
      } else {
        btn.disabled = false;
      }
    });
  }

  function taskToggleCheckbox(chk) {
    var employeeId = chk.dataset.futuEmployee;
    var taskId = chk.dataset.futuTask;
    chk.addEventListener("change", function () {
      var done = chk.checked;

      cmdTaskDoneToggle(employeeId, taskId, done);
    });
  }

  function taskCommentEditInput(el) {
    var employeeId = el.dataset.futuEmployee;
    var taskId = el.dataset.futuTask;

    var orig = el.value;
    var state$ = menrva.source("orig");

    state$.onValue(function (state) {
      switch (state) {
        case "orig": el.style.border = ""; break;
        case "dirty": el.style.border = "1px solid blue"; break;
        case "saving": el.style.border = "1px solid orange"; break;
        case "saved": el.style.border = "1px solid green"; break;
        case "error": el.style.border = "1px solid red"; break;
      }
    });

    var saveCb = _.debounce(function () {
      menrva.transaction([state$, "saving"]).commit();
      var value = el.value;
      cmdTaskCommentEdit(employeeId, taskId, value)
        .then(function () {
            menrva.transaction([state$, "saved"]).commit();
            orig = value;
        })
        .catch(function () {
            menrva.transaction([state$, "error"]).commit();
        });
    }, 1000);

    var cb = function () {
      var state = state$.value();
      if ((state === "orig" || state === "saved") && el.value !== orig) {
        menrva.transaction([state$, "dirty"]).commit();
        saveCb();
      } else if (state === "dirty") {
        saveCb();
      }
    };

    el.addEventListener("change", cb);
    el.addEventListener("blur", cb);
    el.addEventListener("keyup", cb);
  }

  // Commands

  function cmdCreateEmployee(checklistId, edit) {
    traceCall(cmdCreateEmployee, arguments);
    return command({
      cmd: "create-employee",
      cid: checklistId,
      edit: edit,
    });
  }

  function cmdEditEmployee(employeeId, edit) {
    traceCall(cmdEditEmployee, arguments);
    return command({
      cmd: "edit-employee",
      eid: employeeId,
      edit: edit,
    }, true);
  }

  function cmdCreateTask(edit, lists) {
    traceCall(cmdCreateTask, arguments);
    return command({
      cmd: "create-task",
      edit: edit,
      lists: lists,
    });
  }

  function cmdEditTask(taskId, edit) {
    traceCall(cmdEditTask, arguments);
    return command({
      cmd: "edit-task",
      tid:  taskId,
      edit: edit,
    }, true);
  }

  function cmdAddTask(checklistId, taskId) {
    traceCall(cmdAddTask, arguments);
    return command({
      cmd: "add-task",
      cid: checklistId,
      tid: taskId,
    }, true);
  }

  function cmdRemoveTask(checklistId, taskId) {
    traceCall(cmdRemoveTask, arguments);
    return command({
      cmd: "remove-task",
      cid: checklistId,
      tid: taskId,
    }, true);
  }

  function cmdRemoveEmployee(employeeId) {
    traceCall(cmdRemoveEmployee, arguments);
    return command({
      cmd: "archive-employee",
      eid: employeeId,
      delete: true,
    }, "/");
  }

  function cmdArchiveEmployee(employeeId) {
    traceCall(cmdArchiveEmployee, arguments);
    return command({
      cmd: "archive-employee",
      eid: employeeId,
      delete: false,
    }, "/");
  }

  function cmdTaskDoneToggle(employeeId, taskId, done) {
    traceCall(cmdTaskDoneToggle, arguments);
    return command({
      cmd: "task-item-toggle",
      eid: employeeId,
      tid: taskId,
      done: done ? "done" : "todo",
    });
  }


  function cmdTaskCommentEdit(employeeId, taskId, comment) {
    traceCall(cmdTaskCommentEdit, arguments);
    return command({
      cmd: "task-edit-comment",
      eid: employeeId,
      tid: taskId,
      comment: comment,
    });
  }

  function command(cmd, reload) {
    var url = "/command";

    menrva
      .transaction([futuReloadIndicator$, function (x) {
        return { total: x.total + 1, done: x.done };
      }])
      .commit();

    var headers = new Headers();
    headers.append("Accept", "application/json");
    headers.append("Content-Type", "application/json");

    var opts = {
      method: "POST",
      headers: headers,
      credentials: "same-origin",
      body: JSON.stringify(cmd),
    };

    return fetch(url, opts)
      .then(function (res) {
        var contentType = res.headers.get("content-type");
        if (contentType && contentType.indexOf("application/json") !== -1) {
          return res.json();
        } else {
          return res.text().then(function (txt) {
            throw new Error("Not a JSON" + txt);
          });
        }
      })
      .then(function (res) {
        console.info("command ack", res);
        switch (res.ack) {
          case "ok":
            if (reload === true) {
              console.info("Reloading");
              location.reload();
            } else if (reload) {
              console.info("Loading", reload);
              location.href = reload;
            } else {
              menrva
                .transaction([futuReloadIndicator$, function (x) {
                  return { total: x.total, done: x.done + 1 };
                }])
                .commit();
            }
            break;
          case "load":
            location.pathname = res.url;
            break;
          case "error":
            console.error("Again error", res.reason);
            break;
          default:
            console.error("Unknown ack type");
        }
      })
      .catch(function (exc) {
        console.error(exc);
        var errorCallout = $("div[data-futu-id=error-callout]");
        var errorCalloutContent = $("div[data-futu-id=error-callout-content]", errorCallout)
        if (errorCallout && errorCalloutContent) {
          errorCalloutContent.innerText = exc;
          errorCallout.style.display = "";
        }
        throw exc;
      });
  }

  // Form "library"

  function nonEmptyCheck(str) {
    var t = str.trim();
    return t === "" ? undefined : t;
  }

  function isArrayCheck(x) {
    return _.isArray(x) ? x : undefined;
  }

  // Quick 'n' dirty check for appliance
  function applianceCheck(x) {
    var m = x.match(/^(all|berlin|external|fixed-term|helsinki|london|munich|not|other|part-timer|permanent|stockholm|summer-worker|tampere|and|or| +|\(|\))*$/i);
    return m ? x : undefined;
  }

  function optionalCheck(check) {
    return function (str) {
      return str.trim() === "" ? null : check(str);
    };
  }

  function numberCheck(str) {
    return str.match(/^-?\d+$/) ? (str | 0) : undefined;
  }

  function dayCheck(str) {
    return str.match(/^\d{4}-\d\d-\d\d$/) ? str : undefined;
  }

  function initialiseFormDefs(defs, form) {
    assert(form, "initialiseFormDefs: provide form element");

    _.forEach(defs, function (def, k) {
      def.el = $_(def.sel, form);
      def.$el = def.el.tagName === "SELECT"
        ? jQuery(def.el).select2(SELECT2_OPTS)
        : jQuery(def.el);
      def.orig = inputValue(def.$el);
      def.signal = menrvaInputValue(def.$el);
      def.signal$ = def.check ? def.signal.map(def.check) : def.signal;

      def.changed$ = def.signal.map(function (x) {
        return !_.isEqual(x, def.orig);
      });

      def.submittable$ = def.signal$.map(function (x) { return x !== undefined; });

      // dirty
      def.dirty$ = menrva.source(false);
      def.$el.blur(function () {
        menrva.transaction([def.dirty$, true]).commit();
      });

      menrva.combine(def.dirty$, def.submittable$, function (dirty, submittable) {
        return dirty && !submittable;
      }).onValue(function (errorneous) {
        if (errorneous) {
          def.$el.parent().addClass("error");
          def.$el.addClass("error");
        } else {
          def.$el.parent().removeClass("error");
          def.$el.removeClass("error");
        }
      });
    });

    // reset btn
    var resetBtn = $_("button[data-futu-action=reset]", form);
    initaliseResetButton(resetBtn, defs, { markClean: markClean });

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

    return {
      markDirty: markDirty,
      markClean: markClean,
      submitBtn: $_("button[data-futu-action=submit]", form)
    };
  }

  function initaliseResetButton(resetBtn, defs, actions) {
    // change handling: reset button
    var changed$ = menrva.record(_.mapValues(defs, "changed$")).map(function (rec) {
      return _.chain(rec)
        .values()
        .some()
        .value();
    });

    var dirty$ = menrva.record(_.mapValues(defs, "dirty$")).map(function (rec) {
      return _.chain(rec)
        .values()
        .some()
        .value();
    });

    menrvaSome(changed$, dirty$).onValue(function (enabled) {
      resetBtn.disabled = !enabled;
    });

    buttonOnClick(resetBtn, function () {
      _.forEach(defs, function (def) {
        setInputValue(def.$el, def.orig);
      });

      // after value update!
      actions.markClean();

      // force change events
      // otherwise select2 won't update.
      _.forEach(defs, function (def) {
        def.$el.change();
      });
    });
  }

  function initialiseSubmitButton(submitBtn, defs, actions, callback) {
    var submittable$ = menrva.record(_.mapValues(defs, "submittable$")).map(function (rec) {
      return _.chain(rec)
        .values()
        .every()
        .value();
    });

    var changed$ = menrva.record(_.mapValues(defs, "changed$")).map(function (rec) {
      return _.chain(rec)
        .values()
        .some()
        .value();
    });

    menrva.combine(submittable$, changed$, function (submittable, changed) {
      return changed || !submittable;
    }).onValue(function (enabled) {
      submitBtn.disabled = !enabled;
    });

    submittable$.onValue(function (submittable) {
      if (submittable) {
        submitBtn.classList.remove("alert");
        submitBtn.classList.add("success");
      } else {
        submitBtn.classList.add("alert");
        submitBtn.classList.remove("success");
      }
    });

    buttonOnClick(submitBtn, function () {
      if (submittable$.value()) {
        var values = _.mapValues(defs, function (def) { return def.signal$.value(); });
        callback(values)
      } else {
        actions.markDirty();
      }
    });
  }

  // Menrva helpers
  function menrvaInputValue($el) {
    var value$ = menrva.source(inputValue($el), _.isEqual)
    var cb = function () {
      menrva.transaction()
        .set(value$, inputValue($el))
        .commit();
    };
    $el.change(cb);
    $el.keyup(cb);
    return value$;
  }

  function menrvaSome() {
    var signals = _.toArray(arguments);
    return menrva.sequence(signals).map(function (values) {
      return _.some(values);
    });
  }

  // Utilities
  function inputValue($el) {
    if ($el.is("input[type=checkbox]")) {
      return $el.prop("checked");
    } else {
      return $el.val();
    }
  }

  function setInputValue($el, val) {
    if ($el.is("input[type=checkbox]")) {
      $el.prop('checked', true);
    } else {
      $el.val(val);
    }
  }

  function traceCall(f, args) {
    var args = [f.name].concat(_.toArray(args));
    console.info.apply(console, args);
  }
});
