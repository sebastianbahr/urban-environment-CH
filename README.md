<img src="https://github.com/sebastianbahr/urban-environment-CH/blob/main/images/Firefly_city_CH.jpg" alt="Title image 1" class="center" style="margin: 0px 0px 0px 0px; padding: 2px 2px 2px 2px;" />


# urban-environment-CH
The repository contains all files and selected data that was used for the manuscript "The relationship between urban greenery, mixed lan use and life satisfaction: An examination using remote sensing data and deep learning". Due to the terms of use of FORS, the Swiss Household Panel data used in the analysis can't be shared publicly. However, they can be requested at [FORS](https://forscenter.ch/projects/swiss-household-panel/data/) and matched with the remaining data using the household ID.

The code files can be found in the folder ```code```. Please run them in the following order:
* SHP_data_preparation.ipynb
* image_scraping_google.ipynb
* OpenEarthMap_image_segmentation.ipynb
* segmentation_CH.ipynb
* add_land_use_data.ipynb
* analysis_land_types.R
* visualizations.ipynb


## Land cover and usage data
If you don't want to regenerate the land cover and usage data, it is available in the ```data``` folder. It contains the proportion of the residents' neighborhoods covered by different greenery land types. Further, the proportion covered by land usage categories and the corresponding entropy score are reported as well. The suffix ```_210``` or ```_630``` indicates the buffer size of the neighborhood area considered in the calculation, in meters. The data set consists of all households surveyed in wave 21 of the Swiss Household Panel (SHP) aged between 20 and 85 and living in postcodes with a population density of at least 500 inhabitants / km2. It can be matched with the SHP survey data using the household-ID ```idhous21```. However, the SHP data need to be requested at [FORS](https://forscenter.ch/projects/swiss-household-panel/data/). The geocoordinates used in the analysis are not included in this data and need to be inquired additionaly at FORS.

## Weights trained models
The weights of the five models that have been trained and used as an ensemble to precit land cover can be downloaded [here](https://drive.google.com/drive/folders/1RpSzPsDdSzjjmkW5vWdEOsx1bC4Tdy77).
