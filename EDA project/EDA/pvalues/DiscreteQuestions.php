<?php
session_start();
?>
<?php
include('phpfunctions1.php');
?>
<?php
if (empty($_SESSION['username'])) {
  if (empty($_POST['username'])) {
    $_SESSION['username'] = strtolower($_POST['returningusername']);
    } else {
     $_SESSION['username'] = strtolower($_POST['username']);
  }
}
$userFileName = '../records/People/'.
  trim(str_replace(' ', '', $_SESSION['username'])).'.txt';
$userFile = file($userFileName);
if(preg_match('/[0-9]/', $userFile[0])){
  $toDo = explode("|", $userFile[0]);
} else {
         ob_end_clean();
         header('Location: ./Survey.php', true, 307);
         exit();
}
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns=""http://www.w3.org/1999/xhtml" xml:lang = "en" lang = "en">
<head>
	<meta http-equiv="Content-Type"
	content = "text/html; charset=utf-8"/>
	<title>P-value survey</title>
<link rel = "stylesheet" type = "text/css" href = "main.css">
</head>

<?php
if (!empty($_POST['questionType'])) {
	print_answers($_SESSION, $_POST);
}
?>

<?php
if(preg_match('/[0-9]/', $userFile[1])){
  $completed = explode("|", $userFile[1]);
} else {
  $completed = array();
}

$totalDone = count($completed);
$total = count($toDo) + count($completed);
$percentFinished = round(100*$totalDone/$total);

$whichQuestion = array_rand($toDo, 1);
$whichQArray = explode(",",$toDo[$whichQuestion]);
$questionShuffle = trim($whichQArray[0]);
$versionShuffle = trim($whichQArray[1]);

$completed[] = trim($toDo[$whichQuestion]);
$userFile[1] = implode("|",array_map('trim',$completed));

unset($toDo[$whichQuestion]);
$userFile[0] = implode("|", array_map('trim',$toDo))."\n";

file_put_contents($userFileName, $userFile);
?>

<body class = "mainContent">
<div>

<?php
switch ($questionShuffle) {
      case 1:
	print_Q1($versionShuffle, "q", "0", "0");
        break;
      case 2:
        $graphOrder = array(1, 2, 3, 4);
        shuffle($graphOrder);
	print_Q2($versionShuffle, $graphOrder, "q",
		 array(0,0,0,0));
        break;
      case 3: 
       $graphOrder = array(1,2);
       shuffle($graphOrder);
       print_Q3($versionShuffle, $graphOrder, "q", "0");
       break;
      case 4:
	print_Q4($versionShuffle, "q", "0", "0");
        break;
      case 5:
	print_Q5($versionShuffle, "q", "0", "0");
        break;
}
?>

<form align = "center" action = "Logout.php" method = "post">
<?php 
print "<p>Percent finished: ".$percentFinished."%</p>";
?>
<input type = "submit" name = "submit" value = "That'll do, log me out" />
</form>

</div>
</body>