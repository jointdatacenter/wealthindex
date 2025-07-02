# wealthindex
Standardized methodology for calculating an asset-based wealth index from refugee and host community survey data


1. To assess the suitability of this method for your data -  use 1_check.R file before any data cleaning
   
2. Data cleaning - 2_datacleaning.R     (for FDS South Sudan, newer versions don't require the same level of data cleaning)  

3. To create figures of the descriptive analysis - use 3_desc_figures.R 

4. for calculation of the wealth index and plotting of the quintiles - run 4_PCA.R





(WI_PAK.R and WI_CMR.R calculate the index for the FDS in Pakistan and Cameroon (which have some variables pre-calculated, which makes the calculation much shorter. Datacleaning is included in those files, does not require the datacleaning file separately.))
