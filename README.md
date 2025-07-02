# Global alternatives of natural vegetation cover (GANVC)

This repository contains the raw data (download link and GEE scripts) and the annotated R scripts used in the study "Global alternatives of natural vegetation cover" accepted by Nature Communications (https://www.nature.com/ncomms/).

## Raw Data Access  
The raw data can be downloaded from the following link:  
https://dox.uliege.be/index.php/s/YGujaA889kGRVJD

## Raw Data Scripts
The raw data was computed using the following scripts:  
- Climate database computation:  
https://code.earthengine.google.com/9508240619b39c0e1ae31f4fbc87d025
- NEX historical database computation:  
https://code.earthengine.google.com/a49534c2554a52fa90d06aa3c4135e49
- NEX 2050 database computation:  
https://code.earthengine.google.com/10b02d677c8ece2a5762e96f7f494c33
- Fire frequency database computation:  
https://code.earthengine.google.com/6783ae7479ea02e8315f08ecf3a2038e
- Herbivory database computation:  
See: 0_Herbivory.R

## Study R Scripts  
The entire process, including data preparation, model training, validation and evaluation, and mapping, was performed using the R scripts from 1_MAIN.R to 8_Illustration.R. 
The names of the scripts are self-explanatory. The main script is 1_MAIN.R, which calls the others.

## Final maps: proportions and sensitiviy
The maps corresponding to figs 1 and 2 of the article can be downloaded from the following link:
https://dox.uliege.be/index.php/s/twsmd36R4OaNAPL
