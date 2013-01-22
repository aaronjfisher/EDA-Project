###################################################################
###################### CODE SECTION ###############################
###################################################################

template.type <- 3

###### Put any required code here



###################################################################
###################### ANSWER SECTION #############################
###################################################################

###### Record with dataset (1 or 2) has the lowest
###### p-value as "correct.choice" (replace the current "NA" value)
correct.choice <- NA

###### Assign the true p-value for dataset 1 to "answer.1"
###### (change from the current "NA" value)
answer.1 <- NA
if(round(answer.1, 2) < 0.01){
  answer.1 <- "<0.01"
} else {
  answer.1 <- formatC(answer.1, digits = 2, format = "f")
}

###### Assign the true p-value for dataset 1 to "answer.1"
###### (change from the current "NA" value)
answer.2 <- NA
if(round(answer.2, 2) < 0.01){
  answer.2 <- "<0.01"
} else {
  answer.2 <- formatC(answer.2, digits = 2, format = "f")
}

###################################################################
###################### FIGURE SECTION #############################
###################################################################

png("FigureQuestion1.png", width = 300, height = 300)

##### Put your code to plot dataset 1 here.

dev.off()

png("FigureQuestion2.png", width = 300, height = 300)

##### Put your code to plot dataset 2 here.

dev.off()

