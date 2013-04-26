<?php


$prog_text = $_POST["code"];
$jana_flags = array("-t30");

if (isset($_POST["intsize"]) && $_POST["intsize"] === "32") {
  array_push($jana_flags, "-m");
}

# Read program from stdin
array_push($jana_flags, "-");

$dir = dirname(__FILE__);
$cmd = "$dir/../jana " . implode(" ", $jana_flags);

$cwd = "/tmp";
$descriptorspec = array(
    0 => array("pipe", "r"),
    1 => array("pipe", "w")
);
$env = array();

$process = proc_open($cmd, $descriptorspec, $pipes, $cwd, $env);

if (is_resource($process)) {

    fwrite($pipes[0], $prog_text);
    fclose($pipes[0]);

    $output = stream_get_contents($pipes[1]);
    fclose($pipes[1]);

    $return_value = proc_close($process);

    echo $return_value . "\n";

    if ($return_value === 124) {
      echo "Execution timed out!\n";
    }
    echo $output;
}

?>
