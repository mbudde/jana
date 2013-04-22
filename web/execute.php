<?php


$prog_text = $_POST['code'];


$descriptorspec = array(
   0 => array("pipe", "r"),
   1 => array("pipe", "w")
);

$cmd = dirname(__FILE__) . '/../jana -';
$cwd = '/tmp';
$env = array();

$process = proc_open($cmd, $descriptorspec, $pipes, $cwd, $env);

if (is_resource($process)) {

    fwrite($pipes[0], $prog_text);
    fclose($pipes[0]);

    echo stream_get_contents($pipes[1]);
    fclose($pipes[1]);

    $return_value = proc_close($process);

    echo "command returned $return_value\n";
}

?>
