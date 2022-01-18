# Bikeshares
Analysis of flows between DC bikeshare docks.

# TODO
- Construct bar graph showing trends in total trips
- Estimate total trips in counterfactual world where transportation cost does not depend on elevation changes


# DONE
- Estimate transportation cost between stations as a function of elevation change
-- https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
- Impute 0s for stations which are both operational in a particular month but do not see pairwise traffic
- Reverse geocode station coordinates
- Calculate driving distances between docking stations
- OSRM failed to calculate routes ending at '34th & Water St NW'; the values of distance/duration/geometry for these cases are imputed from the reverse trip.
