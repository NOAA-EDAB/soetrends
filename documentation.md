---
output: html_document
resource_files:
- ui.R
- soetrends.Rproj
- server.R
---
# Trend Analysis Documentation


This shiny application was developed to explore State of the Ecosystem indicator trend analysis. If you have questions not answered below, please contact Kimberly.bastille@noaa.gov.

## Data sources
The datasets available in this shiny app are fisheries and ecosystem indicators found in the State of the Ecosystem reports and are available in our [ecodata](https://github.com/NOAA-EDAB/ecodata) R package. More detailed descriptions of the indicator methodology is available in the [Technical Documentation](https://noaa-edab.github.io/tech-doc/) for these reports. 

## Data analysis

This app uses the model methodology from [Samhouri et al. 2017](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.1860), code adapted from [slarge/GAM-thresholds](https://github.com/slarge/GAM-thresholds/blob/master/gamThresholds_final_KA_2016.10.19.R). This allows for comparing linear models (Linear), Generalized addative models (GAM), linear models with autocorrelation (LMAC), and Generalized addative mixed models (GAMM). The shiny app follows the model selction criteria below. 


      ### Identify "best model"#####
      ### 1a) Is p.ac <= 0.05? If yes, keep and evaluate selection criteria between GAMM and LMAC as best model.If no, move to step 2.
      ### 1b) Is GAMM edf > 2.0 for GAMM? If yes, keep. If no, LMAC is best.
      ### 1c) Is delta.AIC > 2.0 between GAMM and LMAC. If yes, GAMM is best. If no, LMAC is most parsimonious.
      ### 2a) If p.ac > 0.05, then revert to GAM model and ask if edf of GAM > 2.0? If yes, keep GAM. If no, linear model is best model.
      ### 2b) Is GCV minimized in GAM compared to Linear model? If delta.GCV.gam.lm is negative then keep GAM. If delta.GCV.gam.lm is positive then linear model is best.
      ### 2c) Is deltaAIC > 2.0 for GAM? If yes, then GAM is best model. If no, then linear model is best model.


 **CLICK HERE for [Mid-Atlantic indicators GAM summary statistics](https://noaa-edab.github.io/soetrends/stat_summary_MA)**

 **CLICK HERE for [New England indicators GAM summary statistics](https://noaa-edab.github.io/soetrends/stat_summary_NE)**

## Options
**Choose Region/EPU**: The options in this section should be chosen based on area of interest. Please note that not all indicators are available at all scales. For example, some human dimension indictaors are reported at the regional scale while environmental indicators are available at the EPU level. 

![plot of chunk unnamed-chunk-1](C:/Users/kimberly.bastille/Desktop/Rgghhh/soetrends/images/EPU_Designations_Map.jpg)

**Choose SOE Indicator**: Datasets available in this shiny app are ecosystem indicators developed for the State of the Ecosystem reports. Not all datasets are included here and if there is one that you are interested in exploring with this tool, please contact Kimberly.bastille@noaa.gov. More details on indicator methodology are available in the [State of the Ecosystem Technical Documentation](https://noaa-edab.github.io/tech-doc/). 



