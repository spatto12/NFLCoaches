# NFL Coaches
R Scripts, CSV files, and glossary terms for head coaching data for the NFL during the Super Bowl Era (1966 - 2023). Data is from Pro Football Reference.

The Data Cleaning folder includes a CSV file; coachhistory that includes head coaches ('66 - '21) coaching experience for who they worked for and employed 
during their career. The R script in the same folder; coach, cleans this data and prepares it for network analysis. The 2022 - 2023 seasons are mainly added
to the final CSV file; tree.

The Shiny App folder includes the R script for the deployed app (https://pattonanalytics.shinyapps.io/NFLCoaches/). The script uses three csv files; tree, 
games, and wins. The games file includes every regular season game during the Super Bowl Era. The wins file looks at every season coached by a head coach. 
Interim coaches are not included in the coaching tree analysis, but are included in the other two files. There is a csv file that provides definitions for 
all the terms used in these three csv files. 
