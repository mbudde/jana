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
        removeErrorMarkers();
      });
    }
  };

  var prevErrors = [];
  var removeErrorMarkers = function() {
    var session = editor.getSession();
    for (var i = 0; i < prevErrors.length; ++i) {
      session.removeGutterDecoration(prevErrors[i], "errorGutter");
      prevErrors = [];
    }
  };

  var formatOutput = function(output) {
    var session = editor.getSession();
    var match = output.match(/line (\d+), column (\d+)/);

    removeErrorMarkers();
    if (match) {
      var line = parseInt(match[1]) - 1;
      session.addGutterDecoration(line, "errorGutter");
      prevErrors.push(line);
    }
    return $("<pre>").html(output);
  };

  $("#examples a").click(function(e) {
    showExample(e.target.hash);
  });

  $("#run").click(function() {
    $("#output").empty().html("Executing...").show();
    $("#editor").css("bottom", "160px");
    editor.resize();
    var code = editor.getValue();
    var intSize = $("#options input[name='options-int-size']:checked").val();
    $.post("execute.php", {
      "code": code,
      "intsize": intSize
    }, function(res) {
      $("#output").empty().append(formatOutput(res));
    });
  });

  showExample(window.location.hash);
});
