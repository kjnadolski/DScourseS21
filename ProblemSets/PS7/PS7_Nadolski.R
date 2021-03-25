### Problem Set 7 - due 3/25
### Karley Nadolski 

# setwd("~/DScourseS21/ProblemSets/PS7")

library(mice)
library(modelsummary)
library(tidyr)
library(tidyverse)
library(magrittr)

wages <- read.csv("wages.csv")
View(wages)
length(wages)

    # 5. Drop observations where either hgc or tenure are missing
    wages.new <-  mutate(wages) %>% drop_na(hgc) %>% drop_na(tenure)
    
    View(wages.new)
    sum(is.na(wages.new$tenure))
    sum(is.na(wages.new$hgc))
    
    # 6. Use modelsummary to produce a summary table of this dataset
    datasummary_skim(data = wages.new, histogram = F, output = "latex")
    
    # the log wages are missing at a rate of 25%. The log wage variables are
    # most likely missing due to 
    
    # 7. logwage = beta0 +beta1hgc + beta2college +beta3tenure + beta4tenure^2 +beta5age +beta6married + epsilon
          est <- logwage ~ hgc + as.factor(college) + poly(tenure,2,raw=T) + age + as.factor(married)
              # as.factor creates dummy variables out of character strings for married and college
              # poly creates two variables: tenure and tenure^2
    
      # Estimate this regression using only complete cases (listwise deletion on the log wage variable) -- assumes logwages are MCAR
        
        est1 <- lm(est, wages.new, na.action= na.omit)
        modelsummary(est1, output = "latex")
          # by default, lm in R uses listwise deletion to "throwout" any of the observations with NAs
        
      #  Perform Mean imputation to fill in the missing wages
        wages.new %<>% mutate(logwage2 = case_when(!is.na(logwage) ~ logwage, is.na(logwage) ~ mean(wages.new$logwage, na.rm=T)))
        head(wages.new)
        
        est2 <- lm(logwage2 ~ hgc + as.factor(college) + poly(tenure,2,raw=T) + age + as.factor(married), wages.new)
        modelsummary(list(est1,est2), output = "markdown")
        
        
      # Impute missing log wages as the expected values from the first regression
        pred.data = predict(est1, newdata = wages.new)
        wages.new %<>% mutate(logwage3 = case_when(!is.na(logwage)~ logwage, is.na(logwage)~ pred.data))
        est3 <- lm(logwage3 ~ hgc + as.factor(college) + poly(tenure,2,raw=T)+age+as.factor(married), wages.new)        
        modelsummary(list(est1,est2,est3), output = "markdown")        
        modelsummary(list(est1,est2,est3), output = "latex") 
        
      # Use Multiple imputation -- MICE
        wages.mice <- mice(wages.new, m=20, maxit=5, seed=1234) #method='pmm'
        est.mice1 <- with(wages.mice, exp = lm(logwage ~ hgc + as.factor(college) + poly(tenure,2,raw=T)
                                                           + age + as.factor(married)))
        
        completeData <- complete(wages.mice, 5)

        est.mice <- mice::pool(est.mice1)
        summary(est.mice1)
        summary(est1)
        modelsummary(list(est1,est2,est3,est.mice),output="latex")
        
        # The estimates from MICE are very similar to those from Listwise Deletion (but the standard errors can be different)