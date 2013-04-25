$(function(){
  editor = ace.edit("editor");
  editor.setTheme("ace/theme/tomorrow");
  editor.setShowPrintMargin(false);
  editor.getSession().setMode("ace/mode/janus");
  editor.getSession().setTabSize(4);
  editor.getSession().setUseSoftTabs(true);

  var showExample = function(exampleId) {
    var match = exampleId.match(/#examples\/([a-zA-Z0-9-]+)/);
    if (match) {
      $.get("examples/" + match[1] + ".ja", function(data) {
        editor.setValue(data);
        editor.gotoLine(0);
      });
    }
  };

  $("#examples a").click(function(e) {
    showExample(e.target.hash);
  });

  $("#run").click(function() {
    $("#output").empty().html("Executing...").show();
    $("#editor").css("bottom", "160px");
    editor.resize();
    var code = editor.getValue();
    $.post("execute.php", {
      "code": code
    }, function(res) {
      $("#output").empty().append($("<pre>").html(res));
    });
  });

  showExample(window.location.hash);
});
