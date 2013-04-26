<?php

$prog_text = $_POST["code"];
$hash = sha1($prog_text);

$res = file_put_contents(dirname(__FILE__) . "/programs/$hash.ja", $prog_text);

if ($res === FALSE) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
} else {
    echo $hash;
}

?>
