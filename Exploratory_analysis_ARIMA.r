#Explanatory data analysis Time series in R

install.packages("DataExplorer") 
library(DataExplorer)

choco = read.csv("D:/Dataset/flavors_of_cacao.csv", header = T, stringsAsFactors = F)

#Data Cleaning

choco$Cocoa.Percent = as.numeric(gsub('%','',choco$Cocoa.Percent))
choco$Review.Date = as.character(choco$Review.Date)

#Variables

plot_str(choco)

#Man’s search for Missing Values

plot_missing(choco)

#Continuous Variables

plot_histogram(choco)
plot_density(choco)

#Multivariate Analysis
plot_correlation(choco, type = 'continuous','Review.Date')

#Categorical Variables-Barplots
plot_bar(choco)

create_report(choco)

