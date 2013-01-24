###################################################################
###################### CODE SECTION ###############################
###################################################################

template.type <- 2

###### Put any required code here



###################################################################
###################### ANSWER SECTION #############################
###################################################################

###### Record which dataset(s) (1, 2, 3, or 4) has a p-value
###### below 0.05 as "correct.choice" (replace the current "NA" value)
###### Examples of possible values: c(1), c(1,2), c("NA")
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

###### Assign the true p-value for dataset 1 to "answer.1"
###### (change from the current "NA" value)
answer.3 <- NA
if(round(answer.3, 2) < 0.01){
  answer.3 <- "<0.01"
} else {
  answer.3 <- formatC(answer.3, digits = 2, format = "f")
}

###### Assign the true p-value for dataset 1 to "answer.1"
###### (change from the current "NA" value)
answer.4 <- NA
if(round(answer.4, 2) < 0.01){
  answer.4 <- "<0.01"
} else {
  answer.4 <- formatC(answer.4, digits = 2, format = "f")
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

png("FigureQuestion3.png", width = 300, height = 300)

##### Put your code to plot dataset 3 here.

dev.off()

png("FigureQuestion4.png", width = 300, height = 300)

##### Put your code to plot dataset 4 here.

dev.off()
