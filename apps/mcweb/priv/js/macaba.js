function rest_call(Selector, Url, PostData) {
    $.ajax({
        type:"POST",
        url: Url,
        accepts: "application/json",
        dataType: "json",
        contentType: "application/json",
        data: PostData,
        success: function(data, textStatus, jqXHR){
            $(Selector).show().html(textStatus+"<br/>"+data);
        },
        error:function(jqXHR, textStatus, errorThrown){
            $(Selector).show().html(textStatus+"<br/>"+errorThrown);
        }
    });
}

/*
  <input type="checkbox" id="admin_delete_{{p.post_id}}" />Delete post {% if forloop.counter == 1 %}and thread{% endif %}
  <input type="checkbox" id="admin_deletefile_{{p.post_id}}" />Delete file
  <input type="checkbox" id="admin_search_{{p.post_id}}" />All posts of this user
  <input type="checkbox" id="admin_ban_{{p.post_id}}" />Ban
  <input type="text" id="admin_ban_{{p.post_id}}" class="input-small" placeholder="Reason" />
  for <select id="admin_ban_duration_{{p.post_id}}" class="input-small">
    <option value="3600">1 hour</option>
    <option value="10800">3 hours</option>
    <option value="43200">12 hours</option>
    <option value="86400">1 day</option>
    <option value="259200">3 days</option>
    <option value="604800">7 days</option>
    <option value="2592000">1 month</option>
    <option value="7776000">3 months</option>
    <option value="31104000">1 year</option>
  </select>
  {% if p.attach_info %}
  <input type="checkbox" id="admin_delete_file_{{p.post_id}}" /><i class="icon-picture"></i> Delete file
*/

function admin_manage_post(Bid, Tid) {
    if (confirm("Confirm delete thread or post? ")) {
        rest_call("div#admin_"+Tid, "/rest/board/" + Bid + "/post/" + Tid + "/manage",
                  {action: "delete"});
    }
}
function admin_pin_thread(Bid, Tid) {
    rest_call("div#admin_"+Tid, "/rest/board/" + Bid + "/post/" + Tid + "/manage",
              {action: "delete"});
}

function preview_show() {
    var Req = { markup: $("textarea#message").val() };
    $.ajax({
        type:"POST",
        url: "/rest/post/preview",
        headers: { Accept : "application/json" },
        dataType: "text",
        contentType: "application/json",
        data: JSON.stringify(Req),
        success: function(data, textStatus, jqXHR){
            J = JSON.parse(data);
            $("div#preview-popup").show();
            $("div#preview-content").html(J.html);
        },
        error:function(jqXHR, textStatus, errorThrown){
            $("div#preview-popup").show();
            $("div#preview-content").html(textStatus+"<br/>"+errorThrown);
        }
    });
}
