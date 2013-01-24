<?php
    function print_answers($_SESSION, $_POST) {
      $toPrint = trim($_SESSION['username'])."|";
      $toPrint = $toPrint.trim($_POST['questionType'])."|";
      $toPrint = $toPrint.trim($_POST['questionVersion'])."|";
      if (empty($_POST['answer'])) {
	$toPrint = $toPrint.'NA|';
      } else { 
	if($_POST['questionType'] == 2){
	  $question2Answers = implode(",",$_POST['answer']);
	  $toPrint = $toPrint.trim($question2Answers).'|';
	} else{
	  $userAnswer = strip_tags($_POST['answer']);
	  $toPrint = $toPrint.trim($userAnswer).'|';
	}
      }
      $toPrint = $toPrint.trim($_POST['trueAnswer']);
      $toPrint = $toPrint."|extra";

      $responseFile = '../records/Responses.txt';
      file_put_contents($responseFile,
			$toPrint . PHP_EOL, 
			FILE_APPEND);
      
      switch ($_POST['questionType']){
      case "1":
	print_Q1($_POST['questionVersion'], "a",
		 $_POST['trueAnswer'], $_POST['answer']);
	break;
      case "2":
	$graphOrder = explode(",", $_POST['graphOrderPost']);
	print_Q2($_POST['questionVersion'],
		 $graphOrder, "a", $_POST['answer']);
	break;
      case "3":
	$graphOrder = explode(",", $_POST['graphOrderPost']);
	print_Q3($_POST['questionVersion'],
		 $graphOrder, "a", $_POST['answer']);
	break;
      case "4":
	print_Q4($_POST['questionVersion'], "a",
		 $_POST['trueAnswer'], $_POST['answer']);
	break;
      case "5":
	print_Q5($_POST['questionVersion'], "a",
		 $_POST['trueAnswer'], $_POST['answer']);
	break;
      }
    }

     function carry_true_answer($questionType, $questionVersion){
        $answerFile = '../pGraphs/Q'.$questionType.'/Answer/Version-'.$questionVersion.'.txt';
        $trueAnswer = file($answerFile);
	if($questionType == 2){
	  $trueAnswer = implode(",", $trueAnswer);
	  $trueAnswer = str_replace(PHP_EOL,'',$trueAnswer);
	} else {
	  $trueAnswer = $trueAnswer[0];
	}
	print '<input type = "hidden" name = "questionType" value = "';
	print $questionType;
	print '" />';
	print '<input type = "hidden" name = "questionVersion" value = "';
	print $questionVersion;
	print '" />';
	print '<input type = "hidden" name = "trueAnswer" value = "';
	print $trueAnswer;
	print '" />';
     }

function print_Q1($qVersion, $qOrA, $trueAnswer, $guess){
     $PValueGraph = "../pGraphs/Q1/QFigure"; 
     $PValueVersion = $qVersion;
     print '<h2>What is the p-value?</h2>';
     print '<div class = "formWrapper">';
     print '<div class = "figureWrapper">';
     print '<div class = "figureA">';
     print '<div class = "graph">';
     print '<IMG src="';
     print $PValueGraph;
     print '/Version-';
     print $PValueVersion;
     print '.png" width = "250" height = "260">';
     print '</div>';
     if($qOrA == "a"){
       print "<p>p-value : $trueAnswer</p>";
       print "<p>Your guess : ".strip_tags($guess)."</p>";
     }
     print '</div>';
     print '</div>';
     print '<br />';
     print '<form action="Survey.php" method = "post">';
     if ($qOrA == "q") {
       print '<p>The p-value is ';
       print '<input type = "text" name = "answer"';
       print 'size = "10" />';
       print '<input type = "submit" name = "submit"';
       print 'value = "That\'s my best guess"/>';
       carry_true_answer(1, $qVersion);
     } else {
       print '<input type = "submit" name = "submit"';
       print 'value = "Onward!"/>';
     }
     print '</p>';
     print '</form>';
     print '</div>';
  }

function print_Q2($qVersion, $graphOrder, $qOrA, $guess){
  $PValueGraph = "../pGraphs/Q2/Figure";
  $PValueVersion = $qVersion;
  $pvalueFile = "../pGraphs/Q2/PValues/Version-".$qVersion.".txt";
  $pvalues = file($pvalueFile);
  
  if($qOrA == "q"){
    print '<h2>Select all graphs with p-values below 0.05</h2>';
  } else {
    print '<h2>The actual p-values for each graph are:</h2>';
  }
  print '<div class = "formWrapper">';
  print '<form action="Survey.php" method = "post">';
  
  print '<div class = "figureWrapper">';
  
  for($i = 0; $i < 4; ++$i){
    print '<div class = "figure">';
    print '<div class = "graph">';
    print '<IMG src = "../pGraphs/Q2/Figure';
    print $graphOrder[$i];
    print '/Version-';
    print $qVersion;
    print '.png" width = "250" height = "260">';
    print '</div>';
    if ($qOrA == "q") {
      print '<input type = "checkbox" name = "answer[]" value = "';
      print $graphOrder[$i];
      print '">';
    } else {
      $porder = $graphOrder[$i] - 1;
      print "<p>p-value : $pvalues[$porder]</p>";
      if(empty($guess)){
	print "<p>Your guess : >0.05</p>";
      } else {
	if(in_array($graphOrder[$i], $guess)){
	  print "<p>Your guess : <0.05</p>";
	} else {
	  print "<p>Your guess : >0.05</p>";
	}
      }
    }
    print '</div>';
  }
  if ($qOrA == "q") {
    print '<input type = "submit" name = "submit"';
    print 'value = "That\'s my best guess" />';
   carry_true_answer(2, $qVersion);
   print '<input type = "hidden" name = "graphOrderPost"';
   print 'value = "';
   $graphOrderPost = implode(",", $graphOrder);
   print $graphOrderPost;
   print '" />';
  } else {
    print '<input type = "submit" name = "submit"';
    print 'value = "Onward!" />';
  }
  print '</form></div>';
} 

function print_Q3($qVersion, $graphOrder, $qOrA, $guess){
  $PValueGraph = "../pGraphs/Q3/Figure";
  $PValueVersion = $qVersion;
  $pvalueFile = "../pGraphs/Q3/PValues/Version-".$qVersion.".txt";
  $pvalues = file($pvalueFile);
  
  if($qOrA == "q"){
    print '<h2>Which graph has the smallest p-value?</h2>';
  } else {
    print '<h2>The actual p-values for each graph are:</h2>';
  }
  print '<div class = "formWrapper">';
  print '<form action="Survey.php" method = "post">';
  
  print '<div class = "figureWrapper">';
  
  for($i = 0; $i < 2; ++$i){
    print '<div class = "figure">';
    print '<div class = "graph">';
    print '<IMG src = "../pGraphs/Q3/Figure';
    print $graphOrder[$i];
    print '/Version-';
    print $qVersion;
    print '.png" width = "250" height = "260">';
    print '</div>';
    if($qOrA == "q") {
      print '<input type = "radio" name = "answer" value = "';
      print $graphOrder[$i];
      print '">';
    } else {
      $porder = $graphOrder[$i] - 1;
      print "<p>p-value : $pvalues[$porder]</p>";
      if(empty($guess)){
	print "<p>You did not guess.</p>";
      } else {
	if ($guess == $graphOrder[$i]){
	  print "<p>Your guess: lower p-value</p>";
	} else {
	  print "<p>Your guess: higher p-value</p>";
	}
      }
    }
    print '</div>';
  }
  if ($qOrA == "q") {
    print '<input type = "submit" name = "submit"';
    print 'value = "That\'s my best guess" />';
    carry_true_answer(3, $qVersion);
   print '<input type = "hidden" name = "graphOrderPost"';
   print 'value = "';
   $graphOrderPost = implode(",", $graphOrder);
   print $graphOrderPost;
   print '" />';
  } else {
    print '<input type = "submit" name = "submit"';
    print 'value = "Onward!" />';
  }
  print '</form></div>';
} 

function print_Q4($qVersion, $qOrA, $trueAnswer, $guess){
     $PValueGraph = "../pGraphs/Q4/QFigure"; 
     $PValueVersion = $qVersion;
     print '<h2>How many clusters do you see?</h2>';
     print '<div class = "formWrapper">';
     print '<div class = "figureWrapper">';
     print '<div class = "figureA">';
     print '<div class = "graph">';
     print '<IMG src="';
     print $PValueGraph;
     print '/Version-';
     print $PValueVersion;
     print '.png" width = "325" height = "338">';
     print '</div>';
     if($qOrA == "a"){
       print "<p>Number of clusters : $trueAnswer</p>";
       print "<p>Your guess : ".strip_tags($guess)."</p>";
     }
     print '</div>';
     print '</div>';
     print '<br />';
     print '<form action="Survey.php" method = "post">';
     if ($qOrA == "q") {
       print '<p>There are ';
       print '<input type = "text" name = "answer"';
       print 'size = "10" />';
       print ' clusters.</p>';
       print '<input type = "submit" name = "submit"';
       print 'value = "That\'s my best guess"/>';
       carry_true_answer(4, $qVersion);
     } else {
       print '<input type = "submit" name = "submit"';
       print 'value = "Onward!"/>';
     }
     print '</p>';
     print '</form>';
     print '</div>';
  }

function print_Q5($qVersion, $qOrA, $trueAnswer, $guess){
     $PValueGraph = "../pGraphs/Q5/QFigure"; 
     $PValueVersion = $qVersion;
     print '<h2>What do you think is the correlation between these two variables?</h2>';
     print '<div class = "formWrapper">';
     print '<div class = "figureWrapper">';
     print '<div class = "figureA">';
     print '<div class = "graph">';
     print '<IMG src="';
     print $PValueGraph;
     print '/Version-';
     print $PValueVersion;
     print '.png" width = "250" height = "260">';
     print '</div>';
     if($qOrA == "a"){
       print "<p>p-value : $trueAnswer</p>";
       print "<p>Your guess : ".strip_tags($guess)."</p>";
     }
     print '</div>';
     print '</div>';
     print '<br />';
     print '<form action="Survey.php" method = "post">';
     if ($qOrA == "q") {
       print '<p>The correlation is ';
       print '<input type = "text" name = "answer"';
       print 'size = "10" />';
       print '<input type = "submit" name = "submit"';
       print 'value = "That\'s my best guess"/>';
       carry_true_answer(5, $qVersion);
     } else {
       print '<input type = "submit" name = "submit"';
       print 'value = "Onward!"/>';
     }
     print '</p>';
     print '</form>';
     print '</div>';
  }

?>