## Kiva Loans Analysis

This project comes out of the kaggle datasets for Kiva, a crowdfunding site for microloans. I'm interested in the core question being asked, and thought that some mapping and use of shiny for its dynamic rendering properties could make for some good results. Still a work in progress!

### Current Elements:

* GIS Shiny App: https://skirmer.shinyapps.io/Kiva_Loans/   
* Agriculture Drilldown (using python and reticulate): https://github.com/skirmer/kaggle_kiva/blob/master/ag_drilldown.Rmd   
* Regional data NLP: Infeasible. Turns out, the majority of records in the loan data give entirely non-standard region descriptions. For example, a loan record indicates "Bais, Negros Oriental" in the Philippines, which is a perfectly accurate city and province, but the region description data only names the more general regional name "Central Visayas". These two records should be matched, geographically, but no data is available to do this algorthmically, and this same situation is present for many records.   


### Sources:

* GIS and national economic data: http://www.naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-countries/  
* Kiva data: https://www.kaggle.com/kiva/data-science-for-good-kiva-crowdfunding/ 
