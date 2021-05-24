# Trend Analysis Documentation

This shiny application was developed to explore State of the Ecosystem indicator trend analysis. If you have questions not answered below please contact Kimberly.bastille@noaa.gov.

## Data sources
The datasets available in this shiny app are ecosystem indicators found in the State of the Ecosystem reports and are available in our [ecodata](https://github.com/NOAA-EDAB/ecodata) R package. More detailed descriptions of the indicator methodology is available in the [Technical Documentation](https://noaa-edab.github.io/tech-doc/) for these reports. 

## Data analysis

This app uses Generalized Additive Models (GAMs) to analyze areas of localized trend and possible inflection points. These are calculated using the [mgcv](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.html) R package. 


## Options
**Choose Region/EPU**: The options in this section should be chosen based on area of interest. Please note that not all indicators are available at all scales. For example, some human dimension indictaors are reported at the regional scale while environmental indicators are available at the EPU level. 

![plot of chunk unnamed-chunk-1](C:/Users/kimberly.bastille/Desktop/Rgghhh/soetrends/images/EPU_Designations_Map.jpg)

**Choose SOE Indicator**: Datasets available in this shiny app are ecosystem indicators developed for the State of the Ecosystem reports. Not all datasets are included here and if there is one that you are interested in exploring with this tool, please contact Kimberly.bastille@noaa.gov. More details on indicator methodology are available in the [State of the Ecosystem Technical Documentation](https://noaa-edab.github.io/tech-doc/). 

**Choose Model Type**: The State of the Ecosystem reports use linear models to report on trends in datasets 30 years or longer. Based on feedback from SOE collaborators and the SSCs this app presents both the "SOE standard" trend line and the explorative GAM with confidence interval. The GAM can be manipulated using the knots and smoother function sliders. The AIC calculated for the GAM is presented below the plot and is calculate using the [AICcmodavg](https://www.rdocumentation.org/packages/AICcmodavg/versions/2.3-1/topics/AICcmodavg-package) R package. 

**Number of knots**: 


**Smoother Function**:




