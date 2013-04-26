<?php

$hash = $_GET["hash"];

if (!isset($hash) || !preg_match('/^[a-z0-9]{40}$/', $hash)) {
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
