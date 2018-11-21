futu.onload(function () {
  console.info("Initialising library js");
  // imports
  var $ = futu.$;
  var $_ = futu.$_;
  var $$ = futu.$$;
  var assert = futu.assert;
  var buttonOnClick = futu.buttonOnClick;

  $$("table#book-index-table tbody tr").forEach(function (row) {
      row.addEventListener("click", function () {
          window.location.href = this.children[1].children[0].href; //fetch address from title link
      });
  });

  $$("table#boardgame-index-table tbody tr").forEach(function (row) {
      row.addEventListener("click", function () {
          window.location.href = this.children[0].children[0].href; //fetch address from title link
      });
  });

  $$("button[data-futu-id=delete-item]").forEach(function (btn) {
      buttonOnClick(btn, function () {
          // prevent dbl-click
          btn.disabled = true;
          var itemId = btn.dataset.itemId;
          fetch("/item/" + itemId, { method: "DELETE" })
              .then(function (response) { response.json().then(function (t) { location=t;}); })
              .catch(function (error) { console.log("Got error " + error); });
      });
  });

  $$("button[data-futu-id=return-loan]").forEach(function (btn) {
    buttonOnClick(btn, function () {
        // prevent dbl-click
        btn.disabled = true;
        var loanId = btn.dataset.loanId;
        fetch("/return/" + loanId, { method: "POST"})
            .then(function (response) { console.log("Return was successful"); location=location.href; })
            .catch(function (error) { console.log("Couldn't return book") });
    });
  });

  $$("button[data-futu-id=loan-item]").forEach(function (btn) {
    buttonOnClick(btn, function () {
        // prevent dbl-click
        btn.disabled = true;
        var bookid = btn.dataset.itemId;
        var office = btn.dataset.library;
        var payload = { "book": parseInt(bookid), "library": { "office" : office } };
        futu.fetchJSON("/book/borrow/", payload)
            .then(function (response) { console.log("Borrow was successful"); location=location.href; })
            .catch(function (error) { console.log("Couldn't borrow book") });
    });
  });

  $$("button[data-futu-id=snatch-item]").forEach(function (btn) {
    buttonOnClick(btn, function () {
        // prevent dbl-click
        btn.disabled = true;
        var itemid = btn.dataset.itemId;
        fetch("/book/snatch/" + itemid, { method: "POST"})
            .then(function (response) { console.log("Force borrow was successful"); location=location.href; })
            .catch(function (error) { console.log("Couldn't force borrow book") });
    });
  });


    var isbnCheck = function(isbn) {
        if (/[^0-9\-]+/g.test(isbn)) {
            var isbnAlert = document.createElement("span");
            isbnAlert.className = 'alert label';
            isbnAlert.innerText = 'No letters allowed in isbn';
            futu.$("form[data-form-type=book] input[name='isbn']").after(isbnAlert);
            return false;
        }
        return true;
    };

    $$("form[data-futu-id=edit-book]").forEach(function (frm) {
        frm.onsubmit = function () {
            $$('span.alert').forEach(function (s) {
                s.remove();
            });
            var isbn = $("form[data-form-type=book] input[name='isbn']").value;
            return isbnCheck(isbn);
        };
    });

    $$("form[data-futu-id=add-new-book]").forEach(function (frm) {
        frm.onsubmit = function () {
            $$('span.alert').forEach(function (s) {
                s.remove();
            });
            var isbn = $("form[data-form-type=book] input[name='isbn']").value;
            var noErrors = isbnCheck(isbn);
            if (!$("form[data-futu-id=add-new-book] input[name='cover-file']").value
                && !$("form[data-futu-id=add-new-book] input[name='bookinformationid']").value
                && !$("img#cover-image").style.display) {
                var coverAlert = document.createElement("span");
                coverAlert.className = 'alert label';
                coverAlert.innerText = 'Cover picture must be added';
                futu.$("form[data-futu-id=add-new-book] input[name='cover-file']").after(coverAlert);
                noErrors = false;
            }
            return noErrors;
        };
    });

    $$("select[data-futu-id=add-new-item-select]").forEach(function (sel) {
        sel.onchange = function () {
            $$("form").forEach(function (s) { s.style.display = "none" });
            if (this.value === "Book") {
                $$("form[data-futu-id=add-new-book]").forEach(function (s) { s.style.display = "block" });
            } else if (this.value === "Boardgame") {
                $$("form[data-futu-id=add-new-boardgame]").forEach(function (s) { s.style.display = "block" });
            }
        };
    });

    $$("button[data-futu-id=find-by-isbn]").forEach(function (btn) {
        buttonOnClick(btn, function () {
            var isbn = $("input#isbn").value;
            if(isbn) {
                $("form[data-futu-id=add-new-book] input[name='bookinformationid']").value = '';
                futu.fetchJSON("/book/isbn/" + isbn)
                    .then(function (response) {
                        //Fill form with the isbn values
                        if(response.dataSource.source === 'Database') {
                            $("form[data-futu-id=add-new-book] input[name='bookinformationid']").value = response.dataSource.bookinformationid;
                            $("form[data-futu-id=add-new-book] input[name='title']").value = response.title;
                            $("form[data-futu-id=add-new-book] input[name='author']").value = response.author;
                            $("form[data-futu-id=add-new-book] input[name='publisher']").value = response.publisher;
                            $("form[data-futu-id=add-new-book] input[name='published']").value = response.published;
                            $("form[data-futu-id=add-new-book] input[name='amazon-link']").value = response.amazonLink;
                            var currentBooks = '<ul>';
                            for (var lib in response.books) {
                                currentBooks = currentBooks + '<li>' + lib + ' ' + response.books[lib] + '</li>';
                            }
                            currentBooks = currentBooks + '</ul>';
                            $("div#info-box").innerHTML =
                                '<span class="label warning">Books with this ISBN exists already in these libraries:'+currentBooks+'Add new copies by selecting a library and quantity.</span>';
                            $("img#cover-image").src = "/book/cover/" + response.dataSource.coverhash;
                            $("img#cover-image").style.display = "block";
                        } else if(response.dataSource.source === 'Amazon') {
                            $("form[data-futu-id=add-new-book] input[name='title']").value = response.title;
                            $("form[data-futu-id=add-new-book] input[name='author']").value = response.author;
                            $("form[data-futu-id=add-new-book] input[name='publisher']").value = response.publisher;
                            $("form[data-futu-id=add-new-book] input[name='published']").value = response.published;
                            $("form[data-futu-id=add-new-book] input[name='amazon-link']").value = response.amazonLink;
                            $("div#info-box").innerHTML = '<span class="label warning">Book information found from Amazon</span>';
                            $("form[data-futu-id=add-new-book] input[name='cover-url']").value = response.dataSource.coverurl;
                            $("img#cover-image").src = response.dataSource.coverurl;
                            $("img#cover-image").style.display = "block";
                        }
                        $("button[data-futu-id=find-by-isbn]").style.display = "none";
                        $("button[data-futu-id=clear-add-new-book]").style.display = "block";
                    })
                    .catch(function (error) {
                        $("div#info-box").innerHTML = '<span class="info">No books found in database with this ISBN</span>';
                    });
            }
        });
    });

    $$("button[data-futu-id=clear-add-new-book]").forEach(function (btn) {
        buttonOnClick(btn, function () {
            $$("form[data-futu-id=add-new-book] input").forEach(function (i) {
                i.value = "";
            });
            $$('span.alert').forEach(function (s) {
                s.remove();
            });
            $("img#cover-image").src = "";
            $("img#cover-image").style.display = "none";
            $("div#info-box").innerHTML = "";
            $("button[data-futu-id=find-by-isbn]").style.display = "block";
            $("button[data-futu-id=clear-add-new-book]").style.display = "none";
        });
    });
});
