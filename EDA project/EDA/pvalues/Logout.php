<?php
session_start();
?>
<?php
if (empty($_SESSION['username'])) {
  if (empty($_POST['username'])) {
    $_SESSION['username'] = strtolower($_POST['returningusername']);
    } else {
     $_SESSION['username'] = strtolower($_POST['username']);
  }
}
?>
<?php
$userFileName = '../records/People/'.
  trim(str_replace(' ', '', $_SESSION['username'])).'.txt';
$userFile = file($userFileName);
if(preg_match('/[0-9]/', $userFile[0])){
  $toDo = explode("|", $userFile[0]);
}else {
  $toDo = array();
}
if(preg_match('/[0-9]/', $userFile[1])){
  $completed = explode("|", $userFile[1]);
} else {
  $completed = array();
}
$totalDone = count($completed);
$total = count($toDo) + count($completed);
$percentFinished = round(100*$totalDone/$total);
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns=""http://www.w3.org/1999/xhtml" xml:lang = "en" lang = "en">
<head>
	<meta http-equiv="Content-Type"
	content = "text/html; charset=utf-8"/>
	<title>P-value survey</title>
<link rel = "stylesheet" type = "text/css" href = "main.css">
</head>
<body class = "mainContent">
<div>
<h1>Thank you for your help!</h1>

<?php 
  /*
print '<p>You are currently ';
print $percentFinished;
print '% of the way through the survey.</p>';
  */

if($percentFinished < 100){
  print '<p>Please come back later to complete the remaining questions</p>';
}
?>

<form action = "LoginD2.php" method = "post" align = "center">
<input type = "submit" name = "submit" value = "Go back to login page" />
</form>

</div>
</body>