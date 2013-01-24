var pickTrue = Math.floor(Math.random()*trueStatements.length)
var trueChoice = new Array (trueStatements[pickTrue],
			  trueReasons[pickTrue],
			   "true")

var falseChoices = new Array(3)
for(i = 0; i < 3; i++){
var pickFalse =  Math.floor(Math.random()*falseStatements.length)
    falseChoices[i] = [falseStatements[pickFalse],
		       falseReasons[pickFalse],
		      "false"]
    falseStatements.splice(pickFalse,1)
}

var questionChoices = new Array(
    trueChoice,
    falseChoices[0],
    falseChoices[1],
    falseChoices[2]
)

var unorderedChoices = new Array(4)
for(i = 0; i < 4; i++){
    var pickOrder = Math.floor(Math.random()*(4-i))
    unorderedChoices[i] = questionChoices[pickOrder]
    questionChoices.splice(pickOrder,1)
}

for(i = 0; i < 4; i++){
    if(unorderedChoices[i][2] == "true"){
	var trueLocation = i
    }
}

function giveChoices(){
    divOutputA = document.getElementById("divOutputA");
    divOutputB = document.getElementById("divOutputB");
    divOutputC = document.getElementById("divOutputC");
    divOutputD = document.getElementById("divOutputD");

    divOutputA.innerHTML = unorderedChoices[0][0];
    divOutputB.innerHTML = unorderedChoices[1][0];
    divOutputC.innerHTML = unorderedChoices[2][0];
    divOutputD.innerHTML = unorderedChoices[3][0];
}
