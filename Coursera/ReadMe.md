COURSERA FILES
=================



* Current plan is to have 8 questions
* Each with 2 versions (except questions 2-3), one version with significant data, and one without
* To cancel out any effects of a "bad generated sample" we make 5 versions of each
	* This is shown in the "datVer" part of the file name. Each person just sees one version.
* Questions are: Small sample (n35), medium sample sig & nonsig (n100) (reference), large sample (n200), bestfit, axis scale, labels, & outlier
* To ensure we only get outliers when we want them, we add an extra 10 entries to the x vector, and then trim off the top and bottom 5% before proceeding


pvalue plot images
----------------
  * Each image filename has it's number, a 1 or 2 denoting the sig or non sig version respectively, the dataversion #, style & actual p-value (rounded)
  * For example: 
  	 * (EX1) "coursera2_#1-1_datVer-1_n35_pval-0.025.png" is one version of first question they could see, with small sample and significant pvalue (denoted by either 1-1, or by looking at the actual size of the pvalue)
  	 * (EX2) "coursera2_#1-1_datVer-2_n35_pval-0.023.png" will have different data, but otherwise exactly the same trend as EX1.
  	 * (EX3) "coursera2_#1-2_datVer-1_n35_pval-0.338.png" Same sample size as EX1, but now with insignificant data. (Any given use would only be assigned to see one of these files).


<<<<<<< HEAD
Heatmaps
-----
* Images for heatmap in heatmaps_images. The generating script is in this folder
* "mag" in file name denotes absolute value of max signal (normal noise has stdev=1.
=======
Heatmap_Images and Dendrogram_images
-----
* N=40 ("people"); p=500 ("measurements")
* There are 3 levels of signal (magnitude, or "mag" in the file name).
* Level of signal has a versions with 6 - 7 clusters in it (6 options).
* 3 levels of signal * 6 options for #clusters = 18 plots total.
* The dendrograms at the top of the columns are really useful for guessing the right # of clusters.
* Heatmap_images folder also has some other older color choices, which are ignorable.
* The generating script is in this folder (Heatmaps.R). This generates the heatmaps and the dendrograms from the same data. In other words, across all the heatmaps & dendrograms, there are only 18 total data matrixes.

Some stuff in the "old stuff" folder (ignorable)
----------
* The "alt" plan was the have 3 questions about size, and then 2 or three where the data is the same for all people, but each person is randomized to see a different presentation style
* "alt" images have the same data, and give all different presentation styles over the same data. This essentially "holds the data constant" but it doesn't seem like that's the best use of resources. The data should be replicatable, so it's not the most important thing to hold *exactly* constant. It's more useful to hold *person* constant by comparing against the reference with the same people in it. Ex, compare the accuracy rate of people in bestFit-sig with *their* accuracy in in n100-sig. 
  * Each image file has it's number, style & p-value stored in the filename
  * 1-1 and 1-2 are the two possible figures a student might see for question 1. Likewise for questions 2 through 7.
  
  




