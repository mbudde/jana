<?php

$hash = filter_input(INPUT_GET, "hash", FILTER_VALIDATE_REGEXP, array(
  "options" => array("regexp" => "/^[a-z0-9]{8}$/")
));

if (!$hash) {
    header("HTTP/1.1 400 Bad Request");
    echo "bad hash";
    exit;
}

$res = file_get_contents(dirname(__FILE__) . "/programs/$hash.ja");

if ($res === FALSE) {
    header("HTTP/1.1 404 Not Found");
    exit;
} else {
    header("Content-Type: text/janus");
    echo $res;
}

?>
