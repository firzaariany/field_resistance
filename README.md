# Description
This analysis is to find the relationship between resistance measured in the field
(field resistance) and measured in the lab (lab resistance).

The raw data consists of the measurement in field from year 2000 to 2010, as well as
measurement in the lab for the same period.

The spesies is Populus nigra (poplar) that was collected from several natural populations 
and sources in Western Europe.

Poplar is usually attacked by rust fungus that causes yellow spots on the poplar
leaves. These spots reduce poplar's effectiveness in carrying out photosynthesis processes
because these spots are covering the stomata in the leaves and reducing poplar's leaf
area to catch sunrays. The impact of the massive attack caused by rust fungus is a decreased
photosynthesis and growth rate which also lead to economic losses.

The idea of collecting these resistance data is to find the resistant poplar that can
fight back the rust fungus.

Field resistance (Notemax) is measured in the field by scoring the infection on 
poplar leaves (score 1 - 6)

Lab resistance is consisted of 3 parameters: latent period (latence), number of spores
on the leaves (nbsores), and spore size (taille). These parameters are measured in the
lab. The poplar leaves are firstly cut into disks and they are put in the leaf disks for
the lab experiment. The experiment then starts by infecting the poplar leaves with different
types of rust fungus (in the data set, they are coded like 98AG69, 93JE3, etc). 
Then they are put in the growth chamber for 14 days.

During 14 days, we observe:
1. Latent period = the time needed by the poplar leaves to start showing the first
symptom, measured daily  
2. Number of spores = number of spores found on the leaves, measured on the day
13 after infection  
3. Spore size = size of the spores, measured on the day 14 after inoculation using 
a scoring system (1 to 5, 1 being the smallest, 5 being the largest)  

# What's in the script
1. Raw dataset  
2. Identification of missing data (data cleaning)  
3. EDA and data dashboard  
4. Correlation plot  

# The objectives
1. Are there a completely resistant poplar in the field and in the lab?
2. What is the average resistance for poplar in the field and in the lab?
3. Is there a correlation between field resistance and lab resistance?
4. Is there a correlation between the parameters of lab resistance?
