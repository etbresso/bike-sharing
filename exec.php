<?php
	$e = exec('/usr/local/bin/Rscript bikeTestFinal.R');
    echo var_dump($e);?>


<?php  
$dateStart = $_POST['dateStart'];
$timeStart = $_POST['timeStart'];
$dateEnd = $_POST['dateEnd'];
$timeEnd = $_POST['timeEnd'];

print("<center> $dateStart $timeStart $dateEnd $timeEnd</center>"); 
?> 



