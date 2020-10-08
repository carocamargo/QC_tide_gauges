# QC_tide_gauges
Quality Control (QC) checks applied to tide gauge data

# Description

Here we compile QC checks written to assured the credibility of tide gauge data at the IOC Sea Level Monitoring Facility.

These scripts are results of a master thesis (see file 'Camargo2018_Manucript.pdf'. 
In order to re-use it, we caution the user to read carefully the functions, and modify where necessary for own purposes.  

# Files
## functions3.R
  R script with the functions used
## QCmodule.Rmd
  R-markdown file with the functions and descriptions used to do Quality Control of the data
## results-QC.Rmd
  R-markdown script used to obtain the figures in the Thesis. This can be used as an example of how the functions were applied. 

# Abstract
The IOC Sea Level Monitoring Facility (IOC-SLMF) connects about 800 tide gauge stations worldwide. The Facility provides real time (RT) visualization and access to the sea level data of such stations. However, a minimal Quality Control (QC) is applied to the data. QC is important to assure credibility of the data being used and stored. Nonetheless, applying QC to RT data can be a challenging activity, for example the distinction between real outliers and signal fluctuations caused by e.g. tsunamis and storm surges may not be very clear. A good approach for a simple QC of sea level measurements in RT is to compare the observations with a tidal prediction. The purpose of the present work was to obtain tidal predictions based on the data from the 12 selected tide gauge stations, and to use the prediction as a rough QC of the RT observations at the IOC-SLMF. In order to obtain the tidal pattern correctly, a QC procedure was applied in the archived data of the IOC-SLMF. The QC was composed of 5 modules: Correction to mean sea level (MSL) as reference; Stability Check; Outlier Detection; Speed of Change Check; Spike Detection. As the QC method developed here had the purpose of tidal studies, it should not be applied to real-time data. Tsunami and storm surge signals were removed during the QC mainly by the Speed of Change and Outlier Detection modules. On average, the QC flagged 15% of the data, thus detecting and removing the noise of the time series. Regarding tidal predictions, there was no significant difference in using data in minute intervals, as provided by most of the stations, or using data after passing by an hourly filter. Furthermore, because the oldest time series considered had 12 years of data, it is only possible to solve 37 of the harmonic components with high accuracy. The tidal forecast was able to predict the sea level variation for the first months of 2018 with a small source of error, making possible to distinguish unpredicted events, such as tsunamis and storm surges, from the tidal curve.
