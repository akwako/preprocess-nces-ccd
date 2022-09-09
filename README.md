
NCES-CCD Preprocessing Scripts
===============

# Purpose

The National Center for Education Statistics (NCES) compiles information on all public elementary and secondary schools in the United States every year, as a part of their Common Core of Data (CCD). 

These data can be extremely useful to educational researchers, yet there is a learning curve in understanding and using the data. The purpose of this project is twofold: 

1. To help educational researchers get started using the [NCES Common Core of Data](https://nces.ed.gov/ccd/) using the statistical programming language, [R](https://www.r-project.org/about.html). 
    * While NCES generously makes raw data available, they do not provide scripts for cleaning the data, combining variables, or handling missing data. 
    * This project provides some code to help with all three of these import pieces in using NCES data. 
2. To be transparent about the data processing and statistical methods used in survey research that I have conducted with [John Rogers](https://arch.gseis.ucla.edu/directory/john-rogers/) and the [Institute for Democracy, Education, and Access](https://idea.gseis.ucla.edu/) at UCLA. Publications that have used (some of) these methods include:
    * [School and Society in the Age of Trump](https://idea.gseis.ucla.edu/publications/school-and-society-in-age-of-trump), a report on how broader, political challenges affect schools, based on a nationally representative sample of U.S. public high school principals. 
    * [How Politics in our Democracy Prevent Schooling for our Democracy](https://democracyeducationjournal.org/home/vol29/iss2/3/), a paper that examines the relationships between partisanship and civics-oriented professional development opportunities available to teachers. 
    * [Building Community to Support Inclusion and Reduce Intolerance](https://idea.gseis.ucla.edu/publications/building-community), a short report on factors related to principals' proactive stance with regard to building community at their schools.
    * A (soon to be published) paper on the nature of high school principals' responses to school-based student protests against gun violence that followed the mass shooting at Parkland, FL. 

# Dowloading NCES Data

Download [U.S. public school / district data](https://nces.ed.gov/ccd/files.asp) from [NCES](https://nces.ed.gov/). This is known as the "Common Core of Data" or the "Public Elementary/Secondary School Universe Survey Data." 
* At the bottom of the webpage, make your selection, e.g., "NonFiscal," "School," and "2020-2021" (2020-2021 is the most recent academic year for which NCES data is available). Datafiles will population upon your selecton. 
* On the left ("Flat and SAS Files"), download all 5 datafiles: "Directory", "Membership", "Staff", "School Characteristics," and "Lunch Program Eligibility." I use the "Flat" (i.e. CSV) files. 
* It's also a good idea to download the Documentation, in the middle column, for each datafile, while you're here. 

At the bottom of the webpage, there's also a link to the [EDGE website](https://nces.ed.gov/programs/edge/Geographic/SchoolLocations), where you can download geocoding data, which goes as far back as 2015.

# Downloading Presidential Election Data

[Voting data at the Congressional District level](https://docs.google.com/spreadsheets/d/1XbUXnI9OyfAuhP5P3vWtMuGc5UJlrhXbzZo3AwMuHtk/edit#gid=0) can be downloaded from [Daily Kos](https://www.dailykos.com/).
* This file requires a bit of manual adjustment to turn it into a legitimate dataframe, readable by R as a CSV file. 
* There is an accompanying [article](https://www.dailykos.com/stories/2020/11/19/1163009/-Daily-Kos-Elections-presidential-results-by-congressional-district-for-2020-2016-and-2012) that describes the dataset and methodology used to generate this data. 

[Voting data at the state level]((https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX)) is provided by the [MIT Election Data and Science Lab](https://electionlab.mit.edu/data). 

# R Scripts for Processing NCES and Survey Data

Below are scripts used to process data from NCES ("NCES Mgmt.") and to combine NCES data with national voting trends ("NCES Join"). I have also included three survey-related scripts that I have used to combine NCES data with survey data ("Survey Mgmt."), generate survey results using bootstrap methods ("Survey Results"), and produce text output files ("Survey Output"). 

Some scripts are capable of being used in multiple contexts. For example, the format of data from 2016-2021 is more or less the same, so the same script is used. All that is required is to change the file location and year. Every few years, NCES changes the format of the data, and so the overall processing script needs a corresponding change. 

Please feel welcome to [report issues or make recommendations](https://github.com/akwako/preprocess-nces-ccd/issues), or generate pull requests. 

| Academic Year | NCES Mgmt. | NCES Join | Survey Mgmt. | Survey Results | Survey Output | 
| --- | --- | --- | --- | --- | --- | 
| 2015-2016 |  |  |  |  |  |
| 2016-2017 | [2016-2021](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_cleanNCESsch_2016-2021.R) |  |  |  |  |
| 2017-2018 | [2016-2021](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_cleanNCESsch_2016-2021.R) | [2017-2018](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_nces_combine_addVote2018.R) | [2018](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220901_procSurv2018.R) | [2018/2022](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220829_toplineRes.R) | [2018/2022](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20200826_genResults.R) |
| 2018-2019 | [2016-2021](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_cleanNCESsch_2016-2021.R) |  |  |  |  |
| 2019-2020 | [2016-2021](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_cleanNCESsch_2016-2021.R) | [2019-2020](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_nces_combine_addVote2022.R) | [2022](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220818_procSurvAndRepresentative.R) | [2018/2022](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220829_toplineRes.R) | [2018/2022](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20200826_genResults.R) |


Some big-picture notes on what scripts do:

1. NCES Mgmt. 
    * Manages NCES data
        * Combines select variables from NCES CCD datasets
        * Determines missing values using NCES codes
        * Adds additional variables, e.g., Percentage White Students ("white_pct2020")
    * Script is to be run separately for each academic year 
2. NCES Join
    * Joins multiple academic years into single NCES dataset
    * Adds Trump Vote (at state and congression district levels) from external data sources
    * Fills in missing data with data from prior year, where available
    * For remaining missing data, imputess missing values using [mice](https://cran.r-project.org/web/packages/mice/mice.pdf) package
3. Survey Mgmt
    * Manages Survey data
        * Cleans survey data 
        * (Survey data not provided)
    * Joins NCES data with school survey data via unique identifier, "NCESSCH"
4. Gen. Results
    * Generates CSV file of survey results
    * Statistics (including mean, standard deviation, and confidence intervals) are computed using bootstrap methods
    * Produces separate output file of survey results in aggregate and survey results disaggregated by background variables (contingency tables)
5. Gen. Output
    * Generates a .txt file output of survey results
    * For each item, includes wording, number of respondents included in calculations, and table of responses
    * Separate files are generated for each contingency table desired

# Example Output