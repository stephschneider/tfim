# tfim

This is the code repository for the method developed in [Schneider et al. (2021)](https://doi.org/10.1021/acs.est.1c04042) to determine wildfire influence from air quality data. 

This repository includes the R package needed to run the method, and will automatically download the following data: 

- [NAPS data](https://data-donnees.ec.gc.ca/data/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/?lang=en) : ~10 mb/year/pollutant
- [HYSPLIT meteorology files](https://www.ready.noaa.gov/data/archives/narr/README.TXT) : ~ 3 gb/month of analysis 

Modis satellite data needs to be downloaded seperately, which can be done [here](https://firms.modaps.eosdis.nasa.gov/download/create.php). The data must be downloaded as a .csv file including only the MODIS satellite data. To reduce the file size, we recommend using a custom area that includes most (if not all!) of North America. When importing the FIRMS data into the `R` environment, the default file name is 'FIRMS_data', however you can specify any name with the `Import_FIRMS()` function by including the file name as an argument. 

Currently, there is all the information that you would need to run the model for Canada from 2000-2019. Unfortunately, the NARR meteorology is not available past 2019 and so the method must be adjusted to accomodate that. 

The NAPS station information can be downloaded into your working directory from the `/data` folder.
