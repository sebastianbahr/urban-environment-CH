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
