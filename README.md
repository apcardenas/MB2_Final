# MB2 Final Project
## Land Cover Change Detection in Inambaru, Peru due to illegal gold mining  

### Introduction
Illegal mining for several years has caused devastating effects in the Amazon. The presence of heavy machinery used for mining has destroyed the natural vegetation of the area  leading to the displacement of wildlife living in the forest [2]. In addition, the indiscriminate use of chemicals such as mercury has caused health damage to local populations and affected rivers and aquatic life [1].

In this context, Peru is among the countries facing major deforestation in the Amazon area due to illegal gold mining [1]. This project seeks through satellite images to examine the changes in forest cover in the northern part of the Malinowski River from 2014 to 2018 to determine the changes that mining has brought to the land cover in the study area. The following picture shows the study area in 2014, it can be seen some ilegal mining spots along the river and also on the upper area.

![ac5e9e9c-cc1a-4c72-980c-d567886bac46](https://user-images.githubusercontent.com/73172997/115453298-c5bb3b80-a21f-11eb-9640-f866adfc43ca.png)

The following is an image of the same area in 2018. Notice that the river flow has changed and also the mining pools have increase.

![dc7bd81c-a9ca-439d-9fda-2fd75d81f1ac](https://user-images.githubusercontent.com/73172997/115454047-a375ed80-a220-11eb-8d65-b426f175160e.png)

### Data and Methods
In this study, a time series of Sentinel 2 images were used to classify and analyze the land cover changes, occurring in the upper zone of the Malinwsky River. The scenes were preprocesed in QGIS. 

### Model training and classification
It was conducted a post-classification change detection based on the processed Sentinel 2 products to analyze forest change from the period of 2014 to 2018. Following classes
were defined for the analyses: Stable forest, forest gain, non-forest stable, forest loss. The statistical programming language R and GIS tools (QGIS) were used for the analysis.
The random forest algorithm (RF) was chosen for the analysis.

### Results
The main result of the study shows that there was a considerable forest loss among the years of the study. Additionally, it is seem changes among the flow of the river due to the due to the construction of sediment ponds for gold panning on the riverbanks of the river.

### References
[1] Brack A, Ipenza C, Alvarez J, Sotero V; Miner??a Aur??fera en Madre de Dios y Contaminaci??n con Mercurio - Una Bomba de Tiempo, Ministerio del Ambiente, Lima abril del 2011.

[2] Mongabay (2019). Miner??a ilegal: la peor devastaci??n en la historia de la Amazon??a from https://es.mongabay.com/2019/01/mapa-mineria-ilegal-amazonia/
