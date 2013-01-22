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
  if (empty($_POST['username']) ||
    empty($_POST['password'])){
  print "<p class=\"loginNote\" id=\"loginProblem\">Please submit both a user name and a password to login to this survey.</p>";
  } else {
    $username = strip_tags($_POST['username']);
    $password = strip_tags($_POST['password']);
    $thisUser = $username."|".$password;

   $nameFile = '../records/NameList.txt';
   $userList = file($nameFile);
   $userList = array_map('strtolower', $userList);
   $userList = array_map('trim', $userList);

    if(in_array($thisUser, $userList, true)){
     
      $initialFile = '../records/InitialInfo.txt';
      $initialInfo = file($initialFile);

      if(preg_grep("/$username/", $initialInfo)){
	 ob_end_clean();
         header('Location: ./DiscreteQuestions.php', true, 307);
         exit();
      } else{
	 ob_end_clean();
         header('Location: ./NewUserDetails.php', true, 307);
         exit();
      }

    } else {
      print "<p class = \"loginNote\"> Please make sure your user name and password are correct and try again.</p>";
    }
  }
}
?>
  
<div>
<form action = "LoginD2.php" method = "post">
<h2>Login</h2>
<p>
<label for = "username">User name:</label>
<input type = "text" name = "username" id = "username" size = "20" />
</p>
<p>
<label for = "password">Password:</label>
<input type = "password" name = "password" id = "password" size = "20" />
</p>
<p>
<input type = "submit" name = "submit" value = "Rock and roll!" />
</p>
</div>

<div id = "Notice">
  <h2>Notice</h2>
<p class = "loginNote">
<strong>NOTE:</strong>
Information about the survey/IRB notices.</p>
</div>

</div>
</body>
</html>