library(readxl)      #for excel, csv sheets manipulation
library(sdcMicro)    #sdcMicro package with functions for the SDC process 
library(tidyverse)   #for data cleaning

#Import data
setwd("C:/Users/LENOVO T46OS/Desktop/dra-unicef-microdata")
data <- read_excel("data.xlsx", col_types = c("numeric", "text", "numeric", "numeric", "text", 
                                               "text", "text", "text", "numeric", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "numeric", "text", "text", 
                                               "numeric", "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "text"))

selectedKeyVars <- c('Q11', 'Q13', 'Q15','Q21', 'Q22', 'Q34',
                     'Q35', 'Q36', 'Q37', 'Q38',	'Q42',	'Q43'
                     )

#Convert variables into factors
cols = c('Q13','Q1', 'Q19', 'Q41','Q17')
data[,cols] <- lapply(data[,cols], factor)
subFile <- c('ID', selectedKeyVars, pramVariables)
# Convert the sub file into dataframe
fileRes<-data[,subFile]
fileRes <- as.data.frame(fileRes)

#Assess the disclosure risk
objSDC <- createobjSDC(dat = fileRes, keyVars = selectedKeyVars)
#system.time(createobjSDC(dat = fileRes, keyVars = selectedKeyVars))
#Disclosure risk assessment 
#print(objSDC, "risk")

#Generating an internal (extensive) report
#report(objSDC, filename = "index", internal = TRUE) 

#Anonymization
pramVariables = c('Q1', 'Q19', 'Q41','Q17')
#PRAM
set.seed(12345)
objSDC <- pram(objSDC, variables = pramVariables, strata_variables = NULL, pd = 0.8,
               alpha = 0.5)
#system.time(pram(objSDC, variables=c('Q1', 'Q17','Q19', 'Q41'), pd = 0.8))
#Global recoding for the variable age
fileRes$Q11<-as.numeric(fileRes$Q11)
objSDC <- globalRecode(objSDC, column = "Q11", breaks = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))
#system.time(globalRecode(objSDC, column = c('Q11'), breaks = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)))

#Local suppression
objSDC <- localSuppression(objSDC, k=10, importance = NULL) #no importance vector
print(objSDC, "ls")
#system.time(localSuppression(objSDC, k=1, importance = NULL))

dataAnon <- extractManipData(objSDC)
fileUnanonymized<-data
fileUnanonymized[,subFile]<-list(NULL)
fileCombined<-bind_cols(x=dataAnon, y=fileUnanonymized)
write.csv(fileCombined,'unicef_philippines_anonymized_microdata.csv') 

#Re-assess the disclosure risk
print (objSDC, "risk")

# Utility loss measures
# Number of missing values (NA) after local suppression application
namesKeyVars<- names(objSDC@manipKeyVars) 
NAcount <- matrix(NA, nrow = 2, ncol = length(namesKeyVars)) 
colnames(NAcount)  <- c(paste0('NA', namesKeyVars))
rownames(NAcount)  <- c('initial', 'treated')
for(i in 1:length(namesKeyVars)) 
{
  NAcount[1, i] <- sum(is.na(objSDC@origData[,namesKeyVars[i]]))
  NAcount[2, i] <- sum(is.na(objSDC@manipKeyVars[,i]))
}   
NAcount

# Assess number of records changed for the PRAMmed variables
namesPramVars <- names(objSDC@manipPramVars) 
recordChanged<- rep(0, length(namesPramVars))  
names(recordChanged)  <- c(paste0('RC', namesPramVars)) 
for(j in 1:length(namesPramVars)) 
{comp <- objSDC@origData[namesPramVars[j]] != objSDC@manipPramVars[namesPramVars[j]] 
temp1 <- sum(comp, na.rm = TRUE)
temp2 <- sum(is.na(comp)) 
temp3 <- sum(is.na(objSDC@origData[namesPramVars[j]]) +
               is.na(objSDC@manipPramVars[namesPramVars[j]])==2)
recordChanged[j] <- temp1 + temp2 - temp3 
}
recordChanged

#Generating an internal (extensive) report
report(objSDC, filename = "index", internal = TRUE) 
