
NCES-CCD Preprocessing Scripts
===============

# Purpose

To help others use NCES Common Core of Data for educational research.

The National Center for Education Statistics (NCES) compiles information on all public elementary and secondary schools in the United States every year, as a part of their Common Core of Data (CCD). With the [Institute for Democracy, Education, and Access](https://idea.gseis.ucla.edu/) at UCLA, I have used this data to help analyze data at the national level to help understand several challenges that face public schooling. We combined the information with a survey of U.S. public high school principals, administered during the summer of 2018, to better understand [School and Society in the Age of Trump](https://idea.gseis.ucla.edu/publications/school-and-society-in-age-of-trump), [how politics in our democracy prevent schooling for our democracy](https://democracyeducationjournal.org/home/vol29/iss2/3/), which factors contribute to [building community to support inclusion and reduce intolerance](https://idea.gseis.ucla.edu/publications/building-community), and the nature and administrative response of protests against gun violence that followed the mass shooting at Parkland, FL. 

Below are scripts used to preprocess data from NCES, and to combine data with national voting trends (at the Congressional District level). They are meant to help education researchers conduct their own independent research projects. Recommendations and pull requests are welcome. If original scripts are desired (used in the above publications), please contact me directly. 

Finally, these scripts are meant to compliment the work of other (more comprehensive) educational data projects, such as the [Urban Insititute](https://educationdata.urban.org/data-explorer). The scripts provided herein are meant to be used modified and, at this point, not a one-stop-shop solution to all education-related research projects. 

# Dowloading Data

Public Elementary/Secondary School Universe Survey Data

Download NCES school/district data at: https://nces.ed.gov/ccd/files.asp
At the bottom, make your selection, e.g., "NonFiscal," "School," and "2020-2021" (the most recent academic year for which NCES data is available). On the left, download all 5 datafiles: "Directory", "Membership", "Staff", "School Characteristics," and "Lunch Program Eligibility." It's also a good idea to download the Documentation, in the middle column, for each datafile, while you're here. At the bottom of the webpage, there's also a link to the EDGE website, where you can download geocoding data, which goes as far back as 2015. I'll the provide the link here for convenience: https://nces.ed.gov/programs/edge/Geographic/SchoolLocations

Voter data

# Using the Script for Preprocessing Data

| Academic Year | NCES Mgmt. | NCES Join | Survey Mgmt. | Survey Results | Survey Output | 
| --- | --- | --- | --- | --- | --- | 
| 2015-2016 |  |  |  |  |  |
| 2016-2017 | [2016-2021](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_cleanNCESsch_2016-2021.R) |  |  |  |  |
| 2017-2018 | [2016-2021](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_cleanNCESsch_2016-2021.R) | [2017-2018](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_nces_combine_addVote2018.R) | [2018](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220901_procSurv2018.R) | [2018/2022](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220829_toplineRes.R) | 5 |
| 2018-2019 | [2016-2021](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_cleanNCESsch_2016-2021.R) |  |  |  |  |
| 2019-2020 | [2016-2021](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_cleanNCESsch_2016-2021.R) | [2019-2020](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220807_nces_combine_addVote2022.R) | [2022](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220818_procSurvAndRepresentative.R) | [2018/2022](https://github.com/akwako/preprocess-nces-ccd/blob/main/preproc-scripts/20220829_toplineRes.R) | 5 |


Depending on which year you need, download the appropriate script. Every few years, NCES changes the format of the data, and so the processing needs to be changed. While there are minor differences year-to-year, I've kept years that have the same general structure in the same script out of convenience. 

1. NCES Mgmt. 
    * Manages NCES data
        * Combines select variables from NCES CCD datasets, 
        * Determines missing values using NCES codes, 
        * Adds additional variables, e.g., Percentage White Students ("white_pct2020")
    * Script is to be run separately for each academic year 
2. NCES Join
    * Joins multiple academic years into single NCES dataset
    * Adds Trump Vote (at state and congression district levels) from external data sources
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