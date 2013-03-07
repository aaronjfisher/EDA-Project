COURSERA FILES
=================



* Current plan is to have 8 questions
* Each with 2 versions, one significant one not
	* (7 questions are listed, but everyone has to do both sig and non sig for #2
* For other questions, you see either sig or nonsig with equal prob.
* To cancel out any effects of a "bad generated sample" we make 5 versions of each
	* This is shown in the "datVer" part of the file name. Each person just sees one version.
* Questions are: Small sample (n35), medium sample (n100) (reference), large sample (n200), axis scale, labels, outlier, bestfit
* Images are in the images folder
<<<<<<< HEAD
  * Each image filename has it's number, a 1 or 2 denoting the sig or non sig version respectively, the dataversion #, style & actual p-value (rounded)
  * For example: 
  	 * "coursera2_#1-1_datVer-1_n35_pval-0.025.png" is one version of first question they could see, with small sample and significant pvalue (denoted by either 1-1, or by looking at the actual size of the pvalue)
  	 * "coursera2_#1-1_datVer-2_n35_pval-0.023.png" will have different data, but otherwise exactly all the same problems as the previous filename
  	 * "coursera2_#1-2_datVer-1_n35_pval-0.338.png" is the same as the first file, but now with insignificant data. Note, a user would only see one of these three mentioned files, or maybe even a different version.

"alt" images with no outliers (ignorable)
----------
* Add an extra 10 to n, and then trim off the top and bottom 5 before proceeding to ensure we only get outliers when we want them?

"alt" images (ignorable)
----------
* The "alt" plan is the have 3 questions about size, and then 2 or three where the data is the same for all people, but each person is randomized to see a different presentation style
* "alt" images have the same data, and give all different presentation styles over the same data. This essentially "holds the data constant" but it doesn't seem like that's the best use of resources. The data should be replicatable, so it's not the most important thing to hold *exactly* constant. It's more useful to hold *person* constant by comparing against the reference with the same people in it. Ex, compare the accuracy rate of people in bestFit-sig with *their* accuracy in in n100-sig. 
  * Each image file has it's number, style & p-value stored in the filename
  * 1-1 and 1-2 are the two possible figures a student might see for question 1. Likewise for questions 2 through 7.
  
  




