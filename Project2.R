#A.

library(data.table)
library(dplyr)
library(RColorBrewer)
library(stringr)
library(usmap)
library(writexl)

#Set working directory. 
wd = setwd("C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Projects/Project 2/ACS_DP02 data")

#Import ACS data from 2009-2017.
data_frame_names = paste0("acsdp2.5y", 09:17) #Create data frame names to store ACS data from each year in.
files = list.files(pattern = "*with_ann") #Create variable with files we are interested in importing.

#Write for loop to read each ACS file into RStudio and save it to corresponding data_frame_names variables.
for (i in seq_along(data_frame_names)) {
  assign(data_frame_names[i], read.csv(files[i], skip=1))}

#Subset each ACS data frame to only include variables of interest. Rename columns to make data frame more readable.
acsdp2.5y9 = acsdp2.5y9[, c(1,2,3,232,238,242,246,250,254,258,262)]
colnames(acsdp2.5y9) = c("fips", "id", "county", "pop09", "% < 9th", "% 9-12", "high school", "some college", "associate", "bachelor", "graduate")
acsdp2.5y10 = acsdp2.5y10[, c(1,2,3,232,238,242,246,250,254,258,262)]
colnames(acsdp2.5y10) = c("fips", "id", "county", "pop10", "% < 9th", "% 9-12", "high school", "some college", "associate", "bachelor", "graduate")
acsdp2.5y11 = acsdp2.5y11[, c(1,2,3,232,238,242,246,250,254,258,262)]
colnames(acsdp2.5y11) = c("fips", "id", "county", "pop11", "% < 9th", "% 9-12", "high school", "some college", "associate", "bachelor", "graduate")
acsdp2.5y12 = acsdp2.5y12[, c(1,2,3,232,238,242,246,250,254,258,262)]
colnames(acsdp2.5y12) = c("fips", "id", "county", "pop12", "% < 9th", "% 9-12", "high school", "some college", "associate", "bachelor", "graduate")
acsdp2.5y13 = acsdp2.5y13[, c(1,2,3,232,238,242,246,250,254,258,262)]
colnames(acsdp2.5y13) = c("fips", "id", "county", "pop13", "% < 9th", "% 9-12", "high school", "some college", "associate", "bachelor", "graduate")
acsdp2.5y14 = acsdp2.5y14[, c(1,2,3,232,238,242,246,250,254,258,262)]
colnames(acsdp2.5y14) = c("fips", "id", "county", "pop14", "% < 9th", "% 9-12", "high school", "some college", "associate", "bachelor", "graduate")
acsdp2.5y15 = acsdp2.5y15[, c(1,2,3,232,238,242,246,250,254,258,262)]
colnames(acsdp2.5y15) = c("fips", "id", "county", "pop15", "% < 9th", "% 9-12", "high school", "some college", "associate", "bachelor", "graduate")
acsdp2.5y16 = acsdp2.5y16[, c(1,2,3,232,238,242,246,250,254,258,262)]
colnames(acsdp2.5y16) = c("fips", "id", "county", "pop16", "% < 9th", "% 9-12", "high school", "some college", "associate", "bachelor", "graduate")
acsdp2.5y17 = acsdp2.5y17[, c(1,2,3,232,238,242,246,250,254,258,262)]
colnames(acsdp2.5y17) = c("fips", "id", "county", "pop17", "% < 9th", "% 9-12", "high school", "some college", "associate", "bachelor", "graduate")

#Calculate CHCI for each year and add this as a variable to each data frame.
acsdp2.5y9$chci09 = ((50 * acsdp2.5y9[,5]) + (100 * acsdp2.5y9[,6]) + (120 * acsdp2.5y9[,7]) + (130 * acsdp2.5y9[,8]) + (140 * acsdp2.5y9[,9]) + (190 * acsdp2.5y9[,10]) + (230 * acsdp2.5y9[,11])) / 100
acsdp2.5y10$chci10 = ((50 * acsdp2.5y10[,5]) + (100 * acsdp2.5y10[,6]) + (120 * acsdp2.5y10[,7]) + (130 * acsdp2.5y10[,8]) + (140 * acsdp2.5y10[,9]) + (190 * acsdp2.5y10[,10]) + (230 * acsdp2.5y10[,11])) / 100
acsdp2.5y11$chci11 = ((50 * acsdp2.5y11[,5]) + (100 * acsdp2.5y11[,6]) + (120 * acsdp2.5y11[,7]) + (130 * acsdp2.5y11[,8]) + (140 * acsdp2.5y11[,9]) + (190 * acsdp2.5y11[,10]) + (230 * acsdp2.5y11[,11])) / 100
acsdp2.5y12$chci12 = ((50 * acsdp2.5y12[,5]) + (100 * acsdp2.5y12[,6]) + (120 * acsdp2.5y12[,7]) + (130 * acsdp2.5y12[,8]) + (140 * acsdp2.5y12[,9]) + (190 * acsdp2.5y12[,10]) + (230 * acsdp2.5y12[,11])) / 100
acsdp2.5y13$chci13 = ((50 * acsdp2.5y13[,5]) + (100 * acsdp2.5y13[,6]) + (120 * acsdp2.5y13[,7]) + (130 * acsdp2.5y13[,8]) + (140 * acsdp2.5y13[,9]) + (190 * acsdp2.5y13[,10]) + (230 * acsdp2.5y13[,11])) / 100
acsdp2.5y14$chci14 = ((50 * acsdp2.5y14[,5]) + (100 * acsdp2.5y14[,6]) + (120 * acsdp2.5y14[,7]) + (130 * acsdp2.5y14[,8]) + (140 * acsdp2.5y14[,9]) + (190 * acsdp2.5y14[,10]) + (230 * acsdp2.5y14[,11])) / 100
acsdp2.5y15$chci15 = ((50 * acsdp2.5y15[,5]) + (100 * acsdp2.5y15[,6]) + (120 * acsdp2.5y15[,7]) + (130 * acsdp2.5y15[,8]) + (140 * acsdp2.5y15[,9]) + (190 * acsdp2.5y15[,10]) + (230 * acsdp2.5y15[,11])) / 100
acsdp2.5y16$chci16 = ((50 * acsdp2.5y16[,5]) + (100 * acsdp2.5y16[,6]) + (120 * acsdp2.5y16[,7]) + (130 * acsdp2.5y16[,8]) + (140 * acsdp2.5y16[,9]) + (190 * acsdp2.5y16[,10]) + (230 * acsdp2.5y16[,11])) / 100
acsdp2.5y17$chci17 = ((50 * acsdp2.5y17[,5]) + (100 * acsdp2.5y17[,6]) + (120 * acsdp2.5y17[,7]) + (130 * acsdp2.5y17[,8]) + (140 * acsdp2.5y17[,9]) + (190 * acsdp2.5y17[,10]) + (230 * acsdp2.5y17[,11])) / 100

#Merge ACS data frames to create one CHCI data frame.
chci_data = merge(acsdp2.5y9[,c(1,2,3,4,12)], acsdp2.5y10[,c(2,4,12)], by="id")
chci_data1 = merge(chci_data, acsdp2.5y11[,c(2,4,12)], by="id") 
chci_data2 = merge(chci_data1, acsdp2.5y12[,c(2,4,12)], by="id") 
chci_data3 = merge(chci_data2, acsdp2.5y13[,c(2,4,12)], by="id") 
chci_data4 = merge(chci_data3, acsdp2.5y14[,c(2,4,12)], by="id") 
chci_data5 = merge(chci_data4, acsdp2.5y15[,c(2,4,12)], by="id") 
chci_data6 = merge(chci_data5, acsdp2.5y16[,c(2,4,12)], by="id") 
chci_data7 = merge(chci_data6, acsdp2.5y17[,c(2,4,12)], by="id") 

#Reorder CHCI data frame columns.
setcolorder(chci_data7, c("id", "fips", "county", "chci09", "chci10", "chci11", "chci12", "chci13", "chci14", "chci15", "chci16", "chci17", "pop09", "pop10", "pop11", "pop12", "pop13", "pop14", "pop15", "pop16", "pop17"))

#Add CHCI growth and CHCI growth rate variables to the CHCI data frame.
chci_data7$chcig = (chci_data7$chci17 - chci_data7$chci09)
chci_data7$chcigr = (chci_data7$chci17 / (chci_data7$chci09) - 1)

#Arrange the data based on CHCI growth and CHCI growth rate (descending for both).
chci_data7 = arrange(chci_data7, desc(chcig), desc(chcigr))

#Change fips column so it only includes the 5 digit county code.
chci_data7$fips = str_sub(chci_data7$fips, -5, -1)
chci_data7 = as.data.frame(chci_data7)

#Plot U.S. county map with color for CHCI in 2017.
qt1=quantile(chci_data7$chci17, probs=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm=T)
chci_data7$chci17 = cut(chci_data7$chci17, breaks=qt1, labels=paste(qt1[-1]))

map17 = plot_usmap(regions = "counties", data = chci_data7, values = "chci17", color="black") + 
  theme(legend.position = "right") + scale_fill_brewer(palette = "RdBu") 
map17

#Save CHCI data sheet and map plot to excel.
outputDir = "C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Projects/Project 2"
write_xlsx(chci_data7, paste0(outputDir, "CHCI_Data.xlsx"))

#B.

library(readxl)

#Set working directory.
setwd("C:/Users/alexm/OneDrive/Desktop/UCLA Data Science Certification/Projects/Project 2")

#Import Corporate Tax data as data frame.
corp_tax= data.frame(read_excel("p02_Corporate tax.xlsx"))

#Replicate three linear regression equations from article.

lm_1 = lm(ypcg ~ ctax, data = corp_tax)
summary(lm_1) #Adjusted R^2 is 0.3977.

lm_2 = lm(ypcg ~ ctax + ypc2000, corp_tax)
summary(lm_2) #Adjusted R^2 is 0.528.

lm_3 = lm(ypcg ~ ctax + ypc2000 + dty + ctax*dty, corp_tax)
summary(lm_3) #Adjusted R^2 is 0.6367.

#Use lm_3 to predict hypothetical GDP per capital growth rate.
#Use ctax = 20%, ypc2000 = $10000, dty = 35%.

ctax = 20
ypc2000 = 10000
dty = 35
ypcg1 = lm_3$coef[[1]] + lm_3$coef[[2]]*ctax + lm_3$coef[[3]]*ypc2000 + lm_3$coef[[4]]*dty + lm_3$coef[[5]]*ctax*dty
ypcg1 #3.23%.

#Plot linear regression chart for lm_3. 
plot_lm3 = plot(corp_tax$ctax, corp_tax$ypcg, main = "Average Corporate Tax Rate Vs. Average GDP Per Capita Growth Rate", xlab="Average Corporate Tax Rate % (2000-2008)", ylab="Average GDP Per Capital Growth Rate (2000-2015)", cex.main = 0.8, cex.lab = 0.6)
abline(lm_3, lwd = 3, col = "red")

#Present best linear regression model for this data using adjusted R^2 value.
lm_4 = lm(ypcg ~ . - country, corp_tax)
summary(lm_4) #Adjusted R^2 is 0.6314.

lm_5 = lm(ypcg ~ ctax + ypc2000 + trade + dty + ihc, corp_tax)
summary(lm_5) #Adjusted R^2 is 0.6281.

lm_6 = lm(ypcg ~ ctax + ypc2000 + dty + ihc, corp_tax)
summary(lm_6) #Adjusted R^2 is 0.6131.

lm_7 = lm(ypcg ~ ctax + ypc2000 + ctax*ypc2000 + dty + ctax*dty, corp_tax)
summary(lm_7) #Adjusted R^2 is 0.6585.

lm_8 = lm(ypcg ~ ctax + ypc2000 + ctax*ypc2000 + dty + ctax*dty + ypc2000*dty, corp_tax)
summary(lm_8) #Adjusted R^2 is 0.6463. 

#The best model I found based on the adjusted R^2 value is lm_7, which has an adjusted R^2 of 0.6585.
#This model indicates that, holding other variables constant, decreasing corporate tax by 10% can predict
#a significant GDP per capita growth rate increase by about 0.22. The negative significant coefficient for 
#ypc2000 indicates that a greater GDP per capita in 2000 is correlated to a lower average GDP per capita 
#growth rate. The negative significant coefficient for dty indicates that when the debt to GDP ratio is rising 
#by 10% over GDP due to tax cuts, GDP growth rate will be reduced. There is also a significant positive interaction
#between tax rate and GDP per capita in 2000. This indicates that for a one unit increase in GDP per capita in 2000,
#this is associated with a greater impact that a corporate tax cut will have on the average GDP per growth. Finally, 
#there is evidence for a significant positive interaction between tax rate and debt to GDP ratio, which means that 
#if the debt ratio of a country is lower, the more impact a corporate tax cut will have on the average GDP growth.
