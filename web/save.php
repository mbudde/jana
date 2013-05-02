<?php

$prog_text = filter_input(INPUT_POST, "code", FILTER_UNSAFE_RAW);
$hash = substr(sha1($prog_text), 0, 8);

$res = file_put_contents(dirname(__FILE__) . "/programs/$hash.ja", $prog_text);

if ($res === FALSE) {
    header("HTTP/1.1 500 Internal Server Error");
    exit;
} else {
    echo $hash;
}

?>
