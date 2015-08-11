Milne-Inlet-Narwhals-2014
=========================

R code to munge, summarize and analyze shore-based Milne Inlet narwhal observations

The scripts should be run in this order:

1) Tide Munging (get tide data ready for integration with count data)

2) Munging 2014 Data (get count data ready for tables, plots, analysis)

3) Table 2014 Data 

4) Plot 2014 Data

5) Combine Annual Data
 * The code in this script contains the call to fit the mixed-effects negative binomial model to the count data (~line 245). 

Notes: The EnviroDataMunging script is not currently used, but could form the basis for munging environmental data (e.g., wind data) in the future. 

