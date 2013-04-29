$(function(){
  editor = ace.edit("editor");
  editor.setTheme("ace/theme/tomorrow");
  editor.setShowPrintMargin(false);
  editor.getSession().setMode("ace/mode/janus");
  editor.getSession().setTabSize(4);
  editor.getSession().setUseSoftTabs(true);
  editor.commands.addCommand({
    name: 'runCommand',
    bindKey: {win: 'Ctrl-Enter',  mac: 'Command-Enter'},
    exec: function(editor) {
        runCode();
    },
    readOnly: true // false if this command should not apply in readOnly mode
  });

  var $editor = $("#editor");
  var $outputPane = $("#output-pane");
  var $output = $("#output");
  var $executing = $("#output-pane .executing");

  function setEditorContent(code) {
    editor.setValue(code);
    editor.gotoLine(0);
    removeErrorMarkers();
  }

  function loadCode(hash) {
    var match = hash.match(/#examples\/([a-zA-Z0-9-]+)/);
    if (match) {
      $.get("examples/" + match[1] + ".ja").done(setEditorContent);
      return;
    }

    match = hash.match(/#([a-z0-9]{8})/);
    if (match) {
      $.get("load.php", {hash: match[1]}).done(setEditorContent);
    }
  }

  function getOptions() {
    var options = {
      intSize: $("#options input[name='options-int-size']:checked").val()
    };
    return options;
  }

  function runCode() {
    showOutputPane();
    $executing.show();
    $output.empty();

    var code = editor.getValue();
    var options = getOptions();
    $.post("execute.php", {
      "code": code,
      "intsize": options["intSize"]
    })
    .done(formatOutput)
    .fail(formatError)
    .always(function() { $executing.hide(); })
  }

  var prevErrors = [];
  function removeErrorMarkers() {
    var session = editor.getSession();
    for (var i = 0; i < prevErrors.length; ++i) {
      session.removeGutterDecoration(prevErrors[i], "errorGutter");
      prevErrors = [];
    }
  }

  function showOutputPane() {
    $outputPane.show();
    $editor.css("bottom", $outputPane.outerHeight()+"px");
    $editor.resize();
  }

  function hideOutputPane() {
    $outputPane.hide();
    $editor.css("bottom", "0");
    $editor.resize();
  }

  function formatOutput(output) {
    // First line contains the exit code
    var retval = parseInt(output.substr(0, output.indexOf("\n")));
    output = output.substring(output.indexOf("\n") + 1);

    removeErrorMarkers();
    if (retval > 0) {
      var session = editor.getSession();
      var match = output.match(/line (\d+), column (\d+)/);

      if (match) {
        var line = parseInt(match[1]) - 1;
        session.addGutterDecoration(line, "errorGutter");
        prevErrors.push(line);
      }
      $output.html($("<pre>").text(output).addClass("error"));
    } else {
      $output.html($("<pre>").text(output));
    }
  }

  function formatError(data) {
    $output.append(
      '<div class="alert alert-error">An error occured while trying to run the program.</div>'
    );
  }

  $("#examples a").click(function(e) {
    loadCode(e.target.hash);
  });

  $("#run").click(runCode);

  $("#output-pane button.close").click(function() {
    $("#output-pane").hide();
    $("#editor").css("bottom", "0px");
    editor.resize();
  });

  var sharePopoverVisible = false;
  $("#share").click(function(e) {
    var code = editor.getValue();
    $.post("save.php", {"code": editor.getValue()})
    .done(function(res) {
      var url = location.protocol+'//'+location.host+location.pathname+"#"+res;
      $("#share").popover({
        title: "Share this URL",
        content: "<input value=\""+url+"\" type=text size=40>",
        placement: "bottom",
        html: true,
        trigger: "manual"
      }).popover("show");
      // Select input field in popover
      $(".popover input").select();
      // Prevent click event from bubbling up to document.
      $(".popover").bind("click", function(e) { return false; });
      sharePopoverVisible = true;
    });
    return false;
  });

  $(document).click(function() {
    if (sharePopoverVisible) {
      $("#share").popover("destroy");
    }
  });

  window.onbeforeunload = function() {
    return "All your changes will be lost when you leave the site.";
  };

  loadCode(location.hash);
});
