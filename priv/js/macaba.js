function preview_show() {
    var M = $("textarea#message").val();
    var AJ = { url: "/util/preview", type: "POST", context: document.body, data: { markup: M }};
    $.ajax(AJ).done(function(Value) {
        $("div#preview-popup").show();
        $("div#preview-content").html(Value);
    });
}

function admin_delete_thread(Bid, Tid) {
    if (confirm("Confirm delete thread?")) {
    }
}

function admin_delete_post(Bid, Pid) {
    if (confirm("Confirm delete post?")) {
    }
}

function admin_delete_file(Bid, Pid) {
    if (confirm("Confirm delete all attached files?")) {
    }
}

function admin_ban_for_post(Bid, Pid) {
    if (confirm("Confirm ban user for this post?")) {
    }
}
