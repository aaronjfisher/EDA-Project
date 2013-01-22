library(utils)
library(R2HTML)

#setwd("C:/Users/Aaron/Documents/JH/Problem Sets for John/")

question.set <- data.frame(name = c("Question1","Question2"), number = c(20,20),source.file = c("generate_questions_1plot","generate_questions_5plot"))

dir.create("PracticeQuestions")
file.copy("printChoices.js","PracticeQuestions/printChoices.js")
setwd("PracticeQuestions")
for(q in 1:nrow(question.set)){
	ticker<-1
  question.name <- as.character(question.set$name[q])
  q.file <- paste("../",paste(question.set[q,"source.file"],".r",sep=""),sep="")
  n.q1 <- question.set[q,"number"]
  for(i in 1:n.q1){
    #Clean up to make sure there aren't any leftover true
    #or false statements or questions
    rm("question")
    if(length(which(ls() == "q.output")) > 0){ rm("q.output")}
    if(length(which(ls() == "q.table")) > 0){ rm("q.table")}
    rm(list=grep("true.statement",ls(),value=TRUE))
    rm(list=grep("true.reason",ls(),value=TRUE))
    rm(list=grep("false.statement",ls(),value=TRUE))
    rm(list=grep("false.reason",ls(),value=TRUE))

    #create names for figures, javascript file, html file
    v <- i
    q.figure.name <- paste(question.name, "-QFigure-",v,
                         ".png",sep="")
    s.figure.name <- paste(question.name, "-SFigure-",v,
                         ".png",sep="")
    q.name <- paste(question.name,"-",v,".html",sep="")
    js.name <- paste(question.name,"-",v,".js",sep="")

    #run sweave on the template file
    Sweave("../HTMLTemplate.Rnw",
           driver = RweaveHTML)

    #Combine all true statements into one vector
    true.stats <- c()
    for(i in grep("true.statement",ls(),value=TRUE)){
      true.stats <- c(true.stats, get(i))
    }
    #Combine all true reasons into one vector
    true.reas <- c()
    for(i in grep("true.reason",ls(),value=TRUE)){
      true.reas <- c(true.reas, get(i))
    }
   #Combine all false statements into one vector
    false.stats <- c()
    for(i in grep("false.statement",ls(),value=TRUE)){
      false.stats <- c(false.stats, get(i))
    }
   #Combine all false reasons into one vector
    false.reas <- c()
    for(i in grep("false.reason",ls(),value=TRUE)){
      false.reas <- c(false.reas, get(i))
    }
    
    #write true and false statements to the javascript file
    cat("var trueStatements = new Array (","\n",file="javascript.js",
        append=FALSE)
    cat(paste("\"", true.stats, "\"",collapse=",",sep=""),"\n",
        file="javascript.js",append=TRUE)
    cat(");","\n",file="javascript.js",append=TRUE)
    #
    cat("var trueReasons = new Array (","\n",file="javascript.js",
        append=TRUE)
    cat(paste("\"", true.reas, "\"",collapse=",",sep=""),"\n",
        file="javascript.js",append=TRUE)
    cat(");","\n",file="javascript.js",append=TRUE)
    #
    cat("var falseStatements = new Array (","\n",file="javascript.js",
        append=TRUE)
    cat(paste("\"", false.stats, "\"",collapse=",",sep=""),"\n",
        file="javascript.js",append=TRUE)
    cat(")","\n",file="javascript.js",append=TRUE)
    #
    cat("var falseReasons = new Array (","\n",file="javascript.js",
        append=TRUE)
    cat(paste("\"", false.reas, "\"",collapse=",",sep=""),"\n",
        file="javascript.js",append=TRUE)
    cat(");","\n",file="javascript.js",append=TRUE)
    
    #rename files
    file.rename("FigureQuestion.png", q.figure.name)
    file.rename("FigureSolution.png", s.figure.name)
    file.rename("javascript.js", js.name)
    file.rename("HTMLTemplate.html",
                q.name)
  }
}
setwd("../")

