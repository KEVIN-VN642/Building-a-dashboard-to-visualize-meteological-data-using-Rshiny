Funtional Design Description:
1)	Project has following modules:
-	Server.R (main module to links with other sub modules)
-	Ui.R (for user interface tasks)
-	HT.R( support for Hutton tasks)
-	Visualize.R (support for plotting task)
-	Convert_csv_to_RData.R : This module supports to combine csv files and convert into RData file “sites_data.RData”. We will mainly to work with sites_data.RData
-	report.Rmd: this file support Rmarkdown task 
2)	Visualized tasks:
Since each aggregation method fits with specific time handling methods, so here is my assumption when create visualization:
-	Aggregation is “Monthly Average”: The plot is the same for three time handling methods
-	Aggregation is “Raw Hourly Data”: The plot is also the same for three time handling methods
-	Aggregation is “Daily Average”: Can work with “Calenda Time” or “Day in Week”
-	Aggregation is “Daily Maxima” or “Daily Minima”: Can work with three time methods

Publishing link as below:
https://nghiavu.shinyapps.io/Project_V1/


