<?php
session_start();
if (!empty($_POST['formSubmitted'])) {

  $toPrint = $_SESSION['username']."|";
  $toPrint = $toPrint.$_POST['degree']."|";
  $toPrint = $toPrint.$_POST['year']."|";
  $toPrint = $toPrint.$_POST['fieldOfStudy'].PHP_EOL;

  $initialFile = '../records/InitialInfo.txt';
  file_put_contents($initialFile, $toPrint, FILE_APPEND);

  ob_end_clean();
  header('Location: ./DiscreteQuestions.php', true, 307);
  exit();
}
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
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns=""http://www.w3.org/1999/xhtml" xml:lang = "en" lang = "en">
<head>
	<meta http-equiv="Content-Type"
	content = "text/html; charset=utf-8"/>
	<title>P-value survey</title>
<link rel = "stylesheet" type = "text/css" href = "main.css">
</head>
<body class = "mainContent">

<h2>Tell us about yourself:</h2>
<form id = "subForm" name = "subForm" action = "NewUserDetails.php" method = "post">
<p>
  <span class = "label">Do you have a terminal degree (PhD, MD, JD, etc.)?</span>
<label>
<input name="degree" type="radio" value="yes" />Yes</label>
<label>
<label>
<input name="degree" type="radio" value="no" />No</label>
<label>
</p>
<p>
<label for = "year" class = "label">
The first time I saw a p-value was the:
</label>
<select name = "year" id = "year">
<option value = "null">Select a decade</option>
<option value = "70searlier">1970s or earlier</option>
<option value = "80s">1980s</option>
<option value = "90s">1990s</option>
<option value = "00s">2000s</option>
<option value = "10s">2010s</option>
<option value = "never">A what?</option>
</select>
</p>
<p>
  <label for = "fieldOfStudy" class = "label">If I had to describe my field in one word, it would be:</label> 
<input type = "text" name = "fieldOfStudy" id = "fieldOfStudy"/>
</p>
<p>
<input type = "submit" name = "Submit" id = "goToQuestions" value = "And we're off!" />
</p>
<input type = "hidden" name = "formSubmitted" value = "yes" />
</form>


</body>
</html>