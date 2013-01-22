<?php
session_start();
?>
<?php
include('phpfunctions.php');
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
if (empty($_SESSION['username'])) {
  if (empty($_POST['username'])) {
    $_SESSION['username'] = strtolower($_POST['returningusername']);
    } else {
     $_SESSION['username'] = strtolower($_POST['username']);
  }
}
?>

<body class = "mainContent">
<div>

<?php
    if (!empty($_POST['questionType'])) {
	print_answers($_SESSION, $_POST);
    } else {
      $questionShuffle = rand(1,5);
      switch ($questionShuffle) {
      case 1:
        $versionShuffle = rand(1,110);
	print_Q1($versionShuffle, "q", "0", "0");
        break;
      case 2:
        $versionShuffle = rand(1,55);
        $graphOrder = array(1, 2, 3, 4);
        shuffle($graphOrder);
	print_Q2($versionShuffle, $graphOrder, "q",
		 array(0,0,0,0));
        break;
      case 3: 
       $versionShuffle = rand(1,55);
       $graphOrder = array(1,2);
       shuffle($graphOrder);
       print_Q3($versionShuffle, $graphOrder, "q", "0");
       break;
      case 4:
        $versionShuffle = rand(1,40);
	print_Q4($versionShuffle, "q", "0", "0");
        break;
      case 5:
        $versionShuffle = rand(1,50);
	print_Q5($versionShuffle, "q", "0", "0");
        break;
      }
    }
?>

</div>

<form align = "center" action = "Logout.php" method = "post">
<p>Percent finished: 100%</p>
<input type = "submit" name = "submit" value = "That'll do, log me out" />
</form>

</body>
</html>