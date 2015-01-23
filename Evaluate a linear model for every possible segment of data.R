######################################################################################
# 
# Purpose:
# This function was written to evaluate a linear model for every possible segment
# There are 4 dependent variables and 3 segment variables cumulatively  with ~500 unique values
# Program executes ~ 6,000 regression models in < 20s 
#
# Error handling:
# Some variables are highly skewed
# Linear models will throw error if a certain subset doesn't contain data rows
# Skips iteration if that occurs
#
######################################################################################

lsDimensionOne <- unique(InputFile$Type1)
lsDimensionTwo <- unique(InputFile$Type2)
lsDimensionThree <- unique(InputFile$Type3)
lsImpactVariables <- c("DependentVar1", "DependentVar2", "DependentVar3", "DependentVar4")

data <- fileOfAllTrades

for (i in lsDimensionThree){
    for (j in lsDimensionTwo){
        for (k in lsDimensionOne){
          for (l in lsImpactVariables){
              
              SubsetInputData <- data[data$Type3==i&data$Type2==j&data$Type1==k,]  
        
              if(nrow(SubsetInputData)<100){
                next();
              }
        
              fml <- as.formula(paste0(l, " ~ IndependentVar1 + IndependentVar2 + IndependentVar3 +test"))
        
              aggregateData <- aggregate(
              fml, data=SubsetInputData, FUN=sum
              )
        
              if(eval(parse(text=paste0("min(aggregateData$",l,")" )))==0){
                next();
              }
              
              fml_reg <- as.formula(
              paste0("log(", l, ") ~ as.factor(IndependentVar3)+ as.factor(IndependentVar2)+ as.factor(IndependentVar1)+ test"
              )
              )
        
              reg1 = lm(fml_reg, data=aggregateData) 
        
              print (paste(i, j, k, l, sep=" - "))
        
              print(summary(reg1))
        
            }
        }
    }
}

