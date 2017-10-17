<?php
$myfile = fopen("data.csv", "w") or die("Unable to open file!");
$txt = $_POST['csv'];
fwrite($myfile, $txt);
fclose($myfile);
?>