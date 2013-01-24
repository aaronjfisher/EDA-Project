<?php
session_start();
$_SESSION = array();
session_destroy();
ob_start();
date_default_timezone_set('America/New_York');
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
<h1>Step right up to guess our p-values!</h1>

<?php
  if ($_SERVER['REQUEST_METHOD'] == 'POST'){
  if (empty($_POST['username']) &
    empty($_POST['returningusername'])){
  print "<p class=\"loginNote\" id=\"loginProblem\"><strong>You're going to be somebody someday... How
  about today? Please give us a name.</strong></p>";
  } else {
   $nameFile = '../records/NameList.txt';
   $previousUsers = file($nameFile);
   $previousUsers = array_map('strtolower', $previousUsers);
    if(!empty($_POST['username'])){
    $namecheck = strtolower($_POST['username']) . PHP_EOL;
      if(in_array($namecheck, $previousUsers, true)){
       print  "<p class=\"loginNote\" id=\"loginProblem\"><strong>You've got a doppelganger, ".$_POST['username'].". Please pick another name.</strong></p>";
      } else {
         file_put_contents($nameFile,
	   strip_tags($_POST['username']) . PHP_EOL,
           FILE_APPEND);
         ob_end_clean();
         header('Location: ./NewUserDetails.php', true, 307);
         exit();
       }
    } else {
         ob_end_clean();
         header('Location: ./Survey.php', true, 307);
         exit();
       }
  }
  }
?>
  
<div id = "newuser">
<form action="Login.php" method = "post">
  <h2>New?</h2>
  <p>Please choose an alias to use for this site.</p> 
<p class = "loginNote">
<strong>NOTE:</strong>
Once you create this alias, you will be able to log back in anytime by finding your name on the "Back for more?" list. The list of these names is viewable by anyone, so please choose a pseudonym if you'd prefer to remain anonymous.</p>
  <p>
<label for = "username">Type your alias here:</label>
  <input type = "text" 
	name = "username" id = "username" size = "20" />
  <input type = "submit" name = "submit" value = "Take a dip" />
  </p>
</form>
</div>

<?php
   $nameFile = '../records/NameList.txt';
   $previousUsers = file($nameFile);
 print "<div id = 'returninguser'>";
 print "<form action=\"Login.php\" method = \"post\">";
 print "<h2>Back for more?</h2>";
 print "<p><label for = \"returningusername\">Find your alias: </label>";
 print "<select name = \"returningusername\">";
    for($i = 0; $i < count($previousUsers); ++$i){
      print "<option value = \"";
      print $previousUsers[$i];
      print "\">";
      print $previousUsers[$i]."</option>";
   	}
 print "<input type = \"submit\"
   name = \"submit\" value = \"Dive back in \"/></p>";
  print "</form>";
  print "</div>";
?>
       
</div>
</body>
</html>