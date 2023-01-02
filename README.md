# Analyzing the Importance of the Serve in Professional Tennis
### Summary

This project is an attempt to provide an analysis of the serve in professional tennis and its association with win probability. We completed extensive data exploration and wrangling to both find important trends and to shape the data into useable forms to build models. The final model is a Bradley-Terry model that focuses on how first serve percentage is associated with win probability for different players. After completing the model building and testing, we built a `Shiny` app that allows users to plot the fitted Bradley-Terry model lines for players of interest and specific opponents. The app allows users to examine how the association between the probability of winning a match and first serve percentage changes for different players and first serve percentages.

### Link to Shiny app:

<https://stlawu.shinyapps.io/Tennis_Serve/>

### References:

If using any of the data in this repository please see the references below for the source and citation of data. 

The `wta_matches_year.csv` and `atp_matches_year.csv` files can be obtained under the Creative Commons License <https://creativecommons.org/licenses/by-nc-sa/4.0/> from

Sackmann, Jeff. tennis_wta, (2020), GitHub Repository, https://github.com/JeffSackmann/tennis_wta 

Sackmann, Jeff. tennis_atp, (2020), GitHub Repository, https://github.com/JeffSackmann/tennis_atp 


The `year-major-matches.csv` and `year-major-points.csv` files can be obtained under the Creative Commons License https://creativecommons.org/licenses/by-nc-sa/4.0/ from

Sackmann, Jeff. tennis_slam_pointbypoint, (2020), GitHub Repository, https://github.com/JeffSackmann/tennis_slam_pointbypoint


And the point importance data can be obtained from the `deuce` `R` package

Kovalchik, Stephanie (2019). `deuce`: Resources for Analysis of Professional Tennis Data. `R` package version 1.3



