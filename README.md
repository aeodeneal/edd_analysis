# EdD Analysis
A repository to demonstrate the work I did for the quantitative analysis for my EdD thesis. 

# About the data
The two .csv files contain the quantitative data I collected for my EdD thesis. These files contain data from three points in the 2022-23 academic year each, using a psychometric testing questionnaire (Self-Regulation Questionnaire - Academic) using a Likert scale. The statements in the questionnaires related to students' motivations to engage in academic study and on their perspectives of the use of target grades as an extrinsic motivator. 
The first symbol in the code (one of E, D, N, I, A) relate to the motivation type of the student. The second symbol (1-5) relates to which statement of the questionnaire they were responding to (e.g. E3 is the third statement they responded to relating to _external_ motivation). The third symbol (a-c) relates to the data collection point in the year (with _a_ being first and _c_ last). 

# Analysis 

_Descriptive Statistics_

I calculated the following descriptive statistics in R: 
* Mean value for each motivation type
* Relative Autonomy Index (RAI) - a weighted mean of participants' responses to the statements to indicate the extent to which they were autonomously motivated
* Target Grade Affinity Index (TGAI) - a weighted mean of participants' responses to the statements relating explicitly to their/their teachers' use of target grades.  

_Inferential Statistics_

Additionally, I calculated the following inferential statistics in R: 
* Spearman Rank Correlation Coefficient - calculated to find the correlations between TGAI & students' mean target grades; TGAI & RAI
* Linear regression (RAI vs TGAI, and mean target grade vs TGAI)
* t-tests for dependent means (time vs RAI; time vs TGAI)
* Stuart-Maxwell test to indicate the change in motivation type (a categorical variable) over time
* Cronbach's alpha test for reliability of data (by finding the internal consistency of the dataset following negative coding of negatively-worded statements in the SRQ-A). 


# Summary of outcomes

_This is written in short bullet points; if you would like to read more of my writing relating to my quantitative findings then please get in touch and I will share a .pdf of my draft Quantitative Findings chapter._

* **Motivation types**: Motivation types remained mostly stable, with more students showing controlled rather than autonomous motivation across the year. Students became less autonomously motivated between the first and second points in Year 10, but this reversed back between the second and third points in the year. 
* **RAI Values**: Relative Autonomy Index (RAI) values indicated most students experienced controlled motivation, with minor fluctuations across different collection points. On average, students were ambivalent in terms of academic motivation but there was a reasonable spread of students. 
* **TGAI Values**: Target Grade Affinity Index (TGAI) values showed students had a mildly positive perspective on target grade use, with little variation. Again, students were mostly ambivalent but there was some skew in the Year 10 data through the year. 
* **Relationship between RAI and TGAI**: A weak but statistically significant positive correlation showed that higher autonomous motivation slightly predicted higher affinity for target grades. 
* **Relationship between Mean Target Grades and TGAI**: No statistically significant correlation was found between students’ target grades and their affinity for using target grades i.e. students' academic ability does not impact their academic motivation. 
* **Change in TGAI Over Time**: No significant changes in TGAI were found over the year, indicating stable student perspectives on target grades. 
* **Change in RAI Over Time**: No significant changes in RAI occurred, suggesting students’ autonomous motivation remained relatively constant throughout the year. 
* **Change in Motivation Type Over Time**: Motivation types remained largely unchanged, though a temporary shift towards controlled motivation was noted mid-year for Year 10 students (and the reverse shift happened at the end of the year). 

