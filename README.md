# Coho Escapement Survey Calculations

## Goal
The ultimate goal with these data are to produce a coho escapement count for aggregate escapement goals for two areas: the Ketchikan Area Index and the Sitka Area Index. Each index is comprised of several streams; the aggregate goal is a sum of all streams.  
If all streams are able to be surveyed, the peak counts can be summed and is very straightforward. However, it is very common for a stream to not be able to be counted. Therefore, the count will need to be imputed using standardized methods.  

## Data Collection

### *Ketchikan Area Index*
14 streams\
Two surveys\
Ketchikan Commercial Fisheries staff  

### *Sitka Area Index*
5 streams\
3--5 surveys, bracketed counts\
Sitka Sport Fisheries staff  

## Data Sources
Download from OceanAK using Subject Area "Region I - Salmon - Escapement Surveys". Apply the filters Species Name = "Coho" and Year >= 1987.  
Save these results as a .CSV and NOT as a .CSV UTF-8.  

## Steps
### *Data Download & QC Check*
First, download data from OceanAK. It is extremely important to first check if all surveys were entered as this is common to be missed. Cross reference OceanAK results against the emails from the survey teams.  
For Sitka area counts, you will need to email the Sitka Commercial Fisheries staff the results of the Sport Fish surveys; Sport Fish staff do not have access to OceanAK.  
Next, check with survey staff about whether all counts were valid. Sometimes there may be a low count that should be excluded (usage code miscoded) or a zero count that feels like a true absence of fish.
Note that these surveys rely on usage codes. These codes are an indication of whether the surveys are representative and unbiased. We have chosen to include all usage codes of "02" (Complete count, potentially useful) and "03" (peak survey count). 
There are many surveys from previous years that were either mistakenly included as complete or excluded. Many of these are manually fixed in the first script file "1_dataimport.R".  
These should be fixed in future years in the OceanAK database itself. 

### *Scripts*
