Data Preparation for Network Meta-Analysis (NMA) in R using netmeta📊

This is efficacy data, where hazard ratios (HR) and their confidence intervals (CI) were extracted for the overall population and patient subgroups (BRCA & HRD mutation-positive patients).

Before analyzing the data using the netmeta package in R, HR values need to be transformed:

Convert HR to log(HR): 

efficacy$logHR <- log(efficacy$HR)

Calculate Standard Error (SE) for log(HR) from the confidence interval (CI) using:

efficacy$SE_lnHR <- (log(`Higher CI`) - log(`Lower CI`)) / (2 * 1.96)

For easier analysis, data for each outcome has been organized into separate Excel sheets.

This transformation is essential for conducting network meta-analysis (NMA) and generating forest plots in netmeta.
