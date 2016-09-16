<?php

// print error
error_reporting(E_ALL);
ini_set('display_errors', 1);

// execute R script from shell
$command = "/usr/local/bin/Rscript linear_regression.R";
exec($command, $output, $result);
// $output = passthru($command, $result);

// print result
echo '<pre>';
print_r($output);
echo'</pre>';
var_dump($result);