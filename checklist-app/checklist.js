document.addEventListener("DOMContentLoaded", function () {
  console.info("Initialising checklist js");

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
      case "selector": return;
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
  $$("input[data-futu-id=task-done-checkbox]").forEach(taskToggleCheckbox);
  $$("button[data-futu-link-button]").forEach(linkButton);
  $$("div[data-futu-id=error-callout] button").forEach(function (btn) {
    buttonOnClick(btn, function () {
      var el = $("div[data-futu-id=error-callout]");
      if (el) {
        el.style.display = "none";
      }
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

  function employeeCreateForm(form) {
    console.info("Initialising employee creation form");

    var defs = {
      checklistId: { sel: "select[data-futu-id=employee-checklist", check: nonEmptyCheck },
      firstName: { sel: "input[data-futu-id=employee-firstname", check: nonEmptyCheck },
      lastName: { sel: "input[data-futu-id=employee-lastname", check: nonEmptyCheck },
      contractType: { sel: "select[data-futu-id=employee-contract-type", check: nonEmptyCheck },
      location: { sel: "select[data-futu-id=employee-location", check: nonEmptyCheck },
      confirmed: { sel: "input[data-futu-id=employee-confirmed" },
      startingDay: { sel: "input[data-futu-id=employee-starting-day", check: dayCheck },
      supervisor: { sel: "input[data-futu-id=employee-supervisor", check: nonEmptyCheck },
      tribe: { sel: "input[data-futu-id=employee-tribe", check: nonEmptyCheck },
      info: { sel: "textarea[data-futu-id=employee-info" },
      phone: { sel: "input[data-futu-id=employee-phone" },
      contactEmail: { sel: "input[data-futu-id=employee-contact-email" },
      fumLogin: { sel: "input[data-futu-id=employee-fum-login" },
      hrNumber: { sel: "input[data-futu-id=employee-hr-number", check: optionalCheck(numberCheck) },
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
      tribe: { sel: "input[data-futu-id=employee-tribe", check: nonEmptyCheck },
      info: { sel: "textarea[data-futu-id=employee-info" },
      phone: { sel: "input[data-futu-id=employee-phone" },
      contactEmail: { sel: "input[data-futu-id=employee-contact-email" },
      fumLogin: { sel: "input[data-futu-id=employee-fum-login" },
      hrNumber: { sel: "input[data-futu-id=employee-hr-number", check: optionalCheck(numberCheck) },
    };

    var actions = initialiseFormDefs(defs, form);

    initialiseSubmitButton(actions.submitBtn, defs, actions, function (values) {
      cmdEditEmployee(employeeId, values);
    });
  }

  function checklistCreateForm(form) {
    console.info("Initialising checklist creation form");

    var defs = {
        name: { sel: "input[data-futu-id=checklist-name]", check: nonEmptyCheck },
    };

    var actions = initialiseFormDefs(defs, form);

    initialiseSubmitButton(actions.submitBtn, defs, actions, function (values) {
      cmdCreateChecklist(values.name);
    });
  }

  function checklistEditForm(form) {
    var checklistId = form.dataset.futuChecklistId;

    console.info("Initialising checklist editing form: " + checklistId);

    var nameEl = $_("input[data-futu-id=checklist-name]", form);

    var nameOrig = nameEl.value;

    var submitBtn = $_("button[data-futu-action=submit]", form);
    var resetBtn = $_("button[data-futu-action=reset]", form);

    var name$ = menrvaInputValue(nameEl);

    var changed$ = menrva.combine(name$, function (name) {
      return !_.isEqual(name, nameOrig);
    });

    changed$.onValue(function (changed) {
      submitBtn.disabled = !changed;
      resetBtn.disabled = !changed;
    });

    buttonOnClick(resetBtn, function () {
      console.info("Checklist edit reset");
      nameEl.value = nameOrig;
    });

    buttonOnClick(submitBtn, function () {
      var name = name$.value();

      cmdEditChecklist(checklistId, name);
    });
  }

  function taskCreateForm(form) {
    console.info("Initialising task creation form");

    var defs = {
      name: { sel: "input[data-futu-id=task-name]", check: nonEmptyCheck },
      info: { sel: "input[data-futu-id=task-info]" },
      role: { sel: "select[data-futu-id=task-role]", check: nonEmptyCheck },
      prereqs: { sel: "select[data-futu-id=task-prereqs", check: isArrayCheck },
      list1: { sel: "select[data-futu-id=task-checklist-1]" },
      app1:  { sel: "input[data-futu-id=task-checklist-appliance-1]", check: applianceCheck },
      list2: { sel: "select[data-futu-id=task-checklist-2]" },
      app2:  { sel: "input[data-futu-id=task-checklist-appliance-2]", check: applianceCheck },
      list3: { sel: "select[data-futu-id=task-checklist-3]" },
      app3:  { sel: "input[data-futu-id=task-checklist-appliance-3]", check: applianceCheck },
    }

    var actions = initialiseFormDefs(defs, form);

    initialiseSubmitButton(actions.submitBtn, defs, actions, function (values) {
      var lists = [];
      if (values.list1) { lists.push({ cid: values.list1, app: values.app1 }); }
      if (values.list2) { lists.push({ cid: values.list2, app: values.app2 }); }
      if (values.list3) { lists.push({ cid: values.list3, app: values.app3 }); }

      var edit = {
        name: values.name,
        info: values.info,
        role: values.role,
        prereqs: values.prereqs,
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
      role: { sel: "select[data-futu-id=task-role]" },
      prereqs: { sel: "select[data-futu-id=task-prereqs", check: isArrayCheck },
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
      appl: { sel: "input[data-futu-id=task-appliance]", check: applianceCheck },
    };

    var actions = initialiseFormDefs(defs, form);

    initialiseSubmitButton(actions.submitBtn, defs, actions, function (values) {
      cmdAddTask(checklistId, values.task, values.appl);
    });
  }

  function linkButton(btn) {
    buttonOnClick(btn, function () {
      btn.disabled = true;
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

  function taskToggleCheckbox(chk) {
    var employeeId = chk.dataset.futuEmployee;
    var taskId = chk.dataset.futuTask;
    chk.addEventListener("change", function () {
      var done = chk.checked;

      cmdTaskDoneToggle(employeeId, taskId, done);
    });
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
    });
  }

  function cmdCreateChecklist(name) {
    traceCall(cmdCreateChecklist, arguments);
    return command({
      cmd: "create-checklist",
      name: name,
    });
  }

  function cmdEditChecklist(checklistId, name) {
    traceCall(cmdEditChecklist, arguments);
    return command({
      cmd: "rename-checklist",
      cid: checklistId,
      name: name,
    });
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
    });
  }

  function cmdAddTask(checklistId, taskId, appliance) {
    traceCall(cmdAddTask, arguments);
    return command({
      cmd: "add-task",
      cid: checklistId,
      tid: taskId,
      appliance: appliance,
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

  function cmdTaskDoneToggle(employeeId, taskId, done) {
    traceCall(cmdTaskDoneToggle, arguments);
    return command({
      cmd: "task-item-toggle",
      eid: employeeId,
      tid: taskId,
      done: done ? "done" : "todo",
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
            if (reload) {
              location.reload();
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
    var m = x.match(/^(all|berlin|external|fixed-term|helsinki|london|munich|not|other|part-timer|permanent|stockholm|summer-worker|tampere|and|or| +|\(\))*$/i);
    return m ? x : undefined;
  }

  function optionalCheck(check) {
    return function (str) {
      return str.trim() === "" ? null : check(str);
    };
  }

  function numberCheck(str) {
    return str.match(/^\d+$/) ? (str | 0) : undefined;
  }

  function dayCheck(str) {
    return str.match(/^\d{4}-\d\d-\d\d$/) ? str : undefined;
  }

  function initialiseFormDefs(defs, form) {
    assert(form, "initialiseFormDefs: provide form element");

    _.forEach(defs, function (def, k) {
      def.el = $_(def.sel, form);
      def.checkbox = def.el.type === "checkbox";
      def.orig = inputValue(def.el)
      def.signal = menrvaInputValue(def.el);
      def.signal$ = def.check ? def.signal.map(def.check) : def.signal;

      def.changed$ = def.signal.map(function (x) {
        return !_.isEqual(x, def.orig);
      });

      def.submittable$ = def.signal$.map(function (x) { return x !== undefined; });

      // dirty
      def.dirty$ = menrva.source(false);
      def.el.addEventListener("blur", function () {
        menrva.transaction([def.dirty$, true]).commit();
      });

      menrva.combine(def.dirty$, def.submittable$, function (dirty, submittable) {
        return dirty && !submittable;
      }).onValue(function (errorneous) {
        if (errorneous) {
          def.el.parentElement.classList.add("error");
          def.el.classList.add("error");
        } else {
          def.el.parentElement.classList.remove("error");
          def.el.classList.remove("error");
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
        tr.push(inputValue(def.el));
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
        // TODO: inputValueSet
        if (def.el.type === "checkbox") {
          def.el.checked = def.orig;
        } else {
          def.el.value = def.orig;
        }
      });

      // after value update!
      actions.markClean();
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
        callback(values);
      } else {
        actions.markDirty();
      }
    });
  }

  // Menrva helpers
  function menrvaInputValue(el) {
    var value$ = menrva.source(inputValue(el), _.isEqual)
    var cb = function () {
      menrva.transaction()
        .set(value$, inputValue(el))
        .commit();
    };
    el.addEventListener("keyup", cb);
    el.addEventListener("change", cb);
    return value$;
  }

  function menrvaSome() {
    var signals = _.toArray(arguments);
    return menrva.sequence(signals).map(function (values) {
      return _.some(values);
    });
  }

  // Utilities

  function $(selector, el) {
    el = el || document;
    return el.querySelector(selector, el);
  }

  // mandatory element
  function $_(selector, el) {
    el = el || document;
    res = el.querySelector(selector, el);
    assert(res, "Non-existing element for selector: " + selector);
    return res;
  }

  function $$(selector, el) {
    el = el || document;
    var res = el.querySelectorAll(selector, el);
    return Array.prototype.slice.call(res);
  }

  function inputValue(el) {
    if (el.tagName === "INPUT" && el.type === "checkbox") {
        return el.checked;
    } else if (el.tagName === "INPUT") {
        return el.value.trim();
    } else if (el.tagName === "SELECT" && el.multiple) {
        return $$("option:checked", el).map(function (o) { return o.value.trim(); });
    } else if (el.tagName === "SELECT") {
        return el.value.trim();
    } else {
        return "";
    }
  }

  function assert(cond, msg) {
    if (!cond) {
      console.error(msg);
      throw new Error(msg);
    }
  }

  function traceCall(f, args) {
    var args = [f.name].concat(_.toArray(args));
    console.info.apply(console, args);
  }

  function buttonOnClick(btn, callback) {
    btn.addEventListener("click", function (e) {
      e.preventDefault();
      callback(e);
      return false;
    });
  }
});
