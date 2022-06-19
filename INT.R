install.packages("tidyverse")
install.packages("GGally")
install.packages("lme4")






#load necessary libraries
library(haven)
library(AER)
library(ggplot2)
library(stargazer)
library(GGally)


#import data using haven package and call it "intfam"

intf <- haven::read_dta("intactfamily-1.dta")


#create subset taking only the data where the child did not live with both
#parents or belongs to a not intact family

intfam <- intf [which(intf$two_bio == 0),]

#see imported data (intfam) structure
str(intfam)

#intfam is a tibble

#1.use stargazer package to print summary statistics as html for
#2. use in Microsoft later


#print summary table
stargazer(as.data.frame(intfam),type = "text")

#print to file

stargazer(as.data.frame(intfam),type = "html", title = "Summary Table" , 
          align = TRUE,
          out = "summary_table_1.html")

#see scatterplot and correlation

ggpairs(intfam[,c("col_grad", 
                  "reason_parent_died",
                  "reason_parent_divorced",
                  "reason_couldnt_care",
                  "reason_child_married",
                  "reason_child_own",
                  "reason_parent_ill",
                  "reason_court",
                  "reason_trouble",
                  "reason_child_ranaway",
                  "reason_child_college",
                  "reason_child_military",
                  "female",
                  "drunkmom",
                  "drunkdad"
)])


#introduce new variables for convenience
#create variable not_col_grad "Did not Graduate College" using var "col_grad"

intfam$not_col_grad <- ifelse(intfam$col_grad == 1,0,1)

#create variable not_white (is either black or hispanic) using variable "white"

intfam$not_white <- ifelse(intfam$white == 1,0,1)

#create variable not_female(is not female) using variable "female"
#intfam$not_female <- ifelse(intfam$female == 1,0,1)


#see sample means

prop.table(table(intfam$female))

prop.table(table(intfam$not_col_grad))

prop.table(table(intfam$not_white))

prop.table(table(intfam$drunkdad))

prop.table(table(intfam$drunkmom))

#Generate Correlation Matrix

correlationmatrix <- cor(intfam[,c("not_col_grad", 
                                   "reason_parent_died",
                                   "reason_parent_divorced",
                                   "reason_couldnt_care",
                                   "reason_child_married",
                                   "reason_child_own",
                                   "reason_parent_ill",
                                   "reason_court",
                                   "reason_trouble",
                                   "reason_child_ranaway",
                                   "reason_child_college",
                                   "reason_child_military",
                                   "female",
                                   "drunkmom",
                                   "drunkdad"
)])
correlationmatrix

stargazer(correlationmatrix, 
          title="Correlation Matrix",
          type = "html",
          out = "correlation.html",
          align = TRUE,
          column.sep.width = "1pt"
)


#Estimation of model (1) using Logit
logit1_intfam <- glm ( not_col_grad ~ 
                         female +
                         reason_parent_died +
                         reason_parent_divorced +
                         reason_couldnt_care +
                         reason_child_married +
                         reason_child_own +
                         reason_parent_ill +
                         reason_court +
                         reason_trouble +
                         reason_child_ranaway +
                         reason_child_college +
                         reason_child_military, 
                       family = binomial (link= "logit"),
                       data = intfam)

logit1_intfam


#Estimation of model(2) using probit
probit1_intfam <- glm ( not_col_grad ~ 
                          female +
                          reason_parent_died +
                          reason_parent_divorced +
                          reason_couldnt_care +
                          reason_child_married +
                          reason_child_own +
                          reason_parent_ill +
                          reason_court +
                          reason_trouble +
                          reason_child_ranaway +
                          reason_child_college +
                          reason_child_military, 
                        family = binomial (link= "probit"),
                        data = intfam)


probit1_intfam

#Estimation of model(3) using probit
probit2_intfam <- glm( not_col_grad ~
                         female +
                         reason_parent_died +
                         reason_parent_divorced +
                         reason_couldnt_care +
                         reason_child_married +
                         reason_child_own +
                         reason_parent_ill +
                         reason_court +
                         reason_trouble +
                         reason_child_ranaway +
                         reason_child_college +
                         reason_child_military +
                         not_white, 
                       family = binomial (link= "probit"),
                       data = intfam)


probit2_intfam

#Estimation of model(4) using probit
probit3_intfam <- glm( not_col_grad ~
                         female +
                         reason_parent_died +
                         reason_parent_divorced +
                         reason_couldnt_care +
                         reason_child_married +
                         reason_child_own +
                         reason_parent_ill +
                         reason_court +
                         reason_trouble +
                         reason_child_ranaway +
                         reason_child_college +
                         reason_child_military+ 
                         not_white +
                         drunkmom +
                         drunkdad,
                       family = "binomial" (link = "probit"),
                       data = intfam)

probit3_intfam



#Estimation of model(5) using probit
probit4_intfam <- glm(  not_col_grad ~
                          female +
                          reason_parent_died +
                          reason_parent_divorced +
                          reason_couldnt_care +
                          reason_child_married +
                          reason_child_own +
                          reason_parent_ill +
                          reason_court +
                          reason_trouble +
                          reason_child_ranaway +
                          reason_child_college +
                          reason_child_military+
                          not_white +
                          drunkmom +
                          drunkdad +
                          female*drunkmom +
                          female*drunkdad +
                          female*not_white,
                        family = "binomial" (link = "probit"),
                        data = intfam)

probit4_intfam

#load heteroskedasticity-robust standard errors of coefficient estimators in a
#list

for_print_se <- list(sqrt(diag(vcovHC(logit1_intfam, type = "HC1"))),
                     sqrt(diag(vcovHC(probit1_intfam, type = "HC1"))),  
                     sqrt(diag(vcovHC(probit2_intfam, type = "HC1"))),
                     sqrt(diag(vcovHC(probit3_intfam, type = "HC1"))),
                     sqrt(diag(vcovHC(probit4_intfam, type = "HC1")))
)

for_print_se


#print Regression Table

stargazer(logit1_intfam, probit1_intfam, probit2_intfam, 
          probit3_intfam, probit4_intfam,
          digits = 3,
          type = "html",
          title = "ISDA Regression Table" , align = TRUE, 
          out = "Regression_Table.html",
          header = FALSE,
          se = for_print_se,
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)","(3)", "(4)", "(5)"))

#Hypothesis Test


linearHypothesis(probit4_intfam, 
                 test = "F",
                 c("female = 0"
                 ),
                 vcov = vcovHC, type = "HC1")


linearHypothesis(probit4_intfam, 
                 test = "F",
                 c("female = 0",
                   "not_white = 0"
                 ),
                 vcov = vcovHC, type = "HC1")


linearHypothesis(probit4_intfam, 
                 test = "F",
                 c("female = 0",
                   "not_white = 0",
                   "female:drunkmom = 0"
                 ),
                 vcov = vcovHC, type = "HC1")

linearHypothesis(probit4_intfam, 
                 test = "F",
                 c("female = 0",
                   "not_white = 0",
                   "female:drunkmom = 0",
                   "female:drunkdad = 0",
                   "female:not_white = 0"),
                 vcov = vcovHC, type = "HC1")
#print hypothesis test tables

hyp1 <- linearHypothesis(probit4_intfam, 
                         test = "F",
                         c("female = 0"
                         ),
                         vcov = vcovHC, type = "HC1")
hyp2 <- linearHypothesis(probit4_intfam, 
                         test = "F",
                         c("female = 0",
                           "not_white = 0"
                         ),
                         vcov = vcovHC, type = "HC1")
hyp3 <- linearHypothesis(probit4_intfam, 
                         test = "F",
                         c("female = 0",
                           "not_white = 0",
                           "female:drunkmom = 0"
                         ),
                         vcov = vcovHC, type = "HC1")

hyp4 <- linearHypothesis(probit4_intfam, 
                         test = "F",
                         c("female = 0",
                           "not_white = 0",
                           "female:drunkmom = 0",
                           "female:drunkdad = 0",
                           "female:not_white = 0"),
                         vcov = vcovHC, type = "HC1")
stargazer(hyp1, hyp2, hyp3, hyp4,
          type = "html",
          out = "Hypothesis2.html",
          align = TRUE
          
)





