# field_resistance
Exploratory data analysis for a raw dataset of poplar's field resistance
Poplar species used in this dataset is Populus nigra
Raw dataset contains:
- Classe = classes of poplar's population source (témoin = control; parent 14x13 = poplar generated from other breeding program; nigrapop = poplar collected from natural populations; coll. nationale = poplar collected from (French) national collection)
- Population = the name of the population source
- Department = the region where the population is located 
- Clone = the assigned name for each observed poplar
- Stabilité intersouche and Nb plants en serre41 = additional information (not considered as variables)
- Notemax = poplar infection measured in the field using scoring system (1 - 6), the year following "Notemax" indicates the year it was measured, and city names following the year indicates the location it was measured
- Latence = latent period (the time needed by poplars to start showing the first symptoms, observed in days in a lab experiment)
- nbsores = number of spores on poplar leaves, measured in counts
- taille = size of spores on poplar leaves, measured using scoring system 1 - 5 (1 being the smallest, 5 being the largest)
Codes following "Latence", "nbsores", and "taille" indicate the types of pathogen that infect poplar. The year following it indicates the year it was measured
