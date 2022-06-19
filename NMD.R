#set working directory
setwd("C:/Users/santosh.kunwar01/Documents/R/EUR/MOA107")
gc()
rm(list = ls(all.names = TRUE))

#load required library


library(haven)
library(AER)
library(stargazer)
library(ggplot2)
library(GGally)
library(data.table)
library(tidyverse)
library(reshape2)
library(lme4)
library(compiler)
library(parallel)
library(boot)
library(lattice)
library(colortools)

#import data

NMDD <- haven::read_sav("Norsk_Monitor_Total_1985-2019_SPSS_31mars.sav")

#have a look at data structure
str(NMDD)
head(NMDD)
class(NMDD)
class(NMDD$Year)

#coerce survey Year as Date class

NMDD$s_year <- as.Date(NMDD$Year, format = "%Y")

#Check class of s_year

class(NMDD$s_year)


#check if respondent are repeated over the years

n_occur <- data.frame(table(NMDD$rspnr))

n_occur[n_occur$Freq > 1,]

mrtt <- data.frame(NMDD[NMDD$rspnr %in% n_occur$Var1[n_occur$Freq > 1],])

mrtt

#[create new table by comprising only the years where the question
#Willingness to pay more for local food was introduced (2013 onwards
#in this case) and subset with variables of interest]

#NMD <- dplyr::filter(NMDD, NMDD$Year >= "2013" )

NMD <- subset(NMDD, Year >= "2013",
              select = c(
                rspnr,
                Year,
                Kjonn,
                Alder,
                V34,
                Q227_5,
                Q227_6,
                Q244,
                Q245_1,
                Q245_2,
                Q245_3,
                Q245_4,
                Q245_5,
                Q245_6,
                Q245_7,
                Q251_24,
                Q258_5,
                Q269_17))


#check if respondent were repeated
noccur <- data.frame(table(NMD$rspnr))
noccur[noccur$Freq > 1,]

#rename variable labels of variables of interest for convenient reading

setnames(NMD, skip_absent = TRUE,
         old = c("Kjonn",
                 "Alder",
                 "V34",
                 "Q227_5",
                 "Q227_6",
                 "Q244",
                 "Q245_1",
                 "Q245_2",
                 "Q245_3",
                 "Q245_4",
                 "Q245_5",
                 "Q245_6",
                 "Q245_7",
                 "Q251_24",
                 "Q258_5",
                 "Q269_17"
         ), 
         new = c("gender",
                 "age",
                 "edu_level",
                 "pay_more",
                 "env_con_food",
                 "buy_often",
                 "place_regular_grocery",
                 "place_farmers_market",
                 "place_farm_shop",
                 "place_special_store",
                 "place_food_festival",
                 "place_elsewhere",
                 "not_purchased",
                 "freq_ost",
                 "freq_beer",
                 "good_selection_avail"
         ))
NMD

#know value labels

library(expss)
expss::val_lab(NMD$gender)


expss::val_lab(NMD$edu_level)

expss::val_lab(NMD$pay_more)

expss::val_lab(NMD$env_con_food)

expss::val_lab(NMD$Year)

expss::val_lab(NMD$buy_often)

expss::val_lab(NMD$place_regular_grocery)

expss::val_lab(NMD$place_farmers_market)

expss::val_lab(NMD$place_farm_shop)

expss::val_lab(NMD$place_special_store)

expss::val_lab(NMD$place_food_festival)

expss::val_lab(NMD$place_elsewhere)

expss::val_lab(NMD$not_purchased)

expss::val_lab(NMD$freq_ost)

expss::val_lab(NMD$freq_beer)

expss::val_lab(NMD$good_selection_avail)

#check scatter plot and corelation

#stargazer(NMD,type = "text")

#stargazer(NMD,type = "html", title = "Summary Table" , 
#align = TRUE,
#out = "summary_table_1.html")

#Ag_summary <-aggregate(NMD$age, 
#by = list(Year = NMD$Year), mean, rm.na = True)

#Ag_summary

#stargazer(Ag_summary, type = "html", align = TRUE,
#out = "summary_table_2.html")


#plot the time series

#(time_plot <- ggplot(NMD, aes(x = s_year, 
#                                   y = pay_more)) +
#   geom_line() +
#  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
# theme_classic())

#all.summary <- Blackmoor %>%
#  select(-subject) %>%  # Remove the subject column
#  group_by(group) %>%  # Group by patient and control
#  # Calculate summary statistics for each group
#  summarise_each(funs(mean, sd, min, max, length)) %>%  
#  mutate(prop = age_length / sum(age_length)) %>%  # Calculate proportion
#  gather(variable, value, -group, -prop) %>%  # Convert to long
#  # Split variable column
#  separate(variable, c("variable", "statistic")) %>%  
#  mutate(statistic = ifelse(statistic == "length", "n", statistic)) %>%
#  spread(statistic, value) %>%  # Make the statistics be actual columns
#  select(group, variable, n, mean, sd, min, max, prop)  # Reorder columns

#install.packages("pander")
#library(pander)
#all.summary <- NMD %>%
#  select(-rspnr) %>%  # Remove the subject column
#  group_by(gender) %>%  # Group by patient and control
#  # Calculate summary statistics for each group
#  summarise_each(list(mean = mean, sd = sd, min = min, max = max,
#                      length = length)) %>%  
#  mutate(prop = age_length / sum(age_length)) %>%  # Calculate proportion
#  gather(variable, value, -gender, -prop) %>%  # Convert to long
#  # Split variable column
#  separate(variable, c("variable", "statistic")) %>%  
#  mutate(statistic = ifelse(statistic == "length", "n", statistic)) %>%
#  spread(statistic, value) %>%  # Make the statistics be actual columns
#  select(group, variable, n, mean, sd, min, max, prop)  # Reorder columns

#change to factors 
#NMD$pay_more <- haven::as_factor(NMD$pay_more) 
#NMD$edu_level <- haven::as_factor(NMD$edu_level)
#NMD$gender <- haven::as_factor(NMD$gender)
#NMD$agegroups <- haven::as_factor(NMD$agegroups)
#NMD$Year <- haven::as_factor(NMD$Year)

library(expss)
expss::val_lab(NMD$pay_more)

#agebreaks <- c(15, 25, 35, 45, 55, 65, 75, 85, 96)

agebreaks <- c(15, 35, 55, 75, 96)

#agelabels <- c("15-25", "26-35", "36-45", "46-55", "56-65",
#"66-75", "76-85", "86-96")

agelabels <- c("15-35", "36-55", "56-75",
               "76-96")

setDT(NMD)[, agegroups := cut(age,
                              breaks = agebreaks,
                              right = FALSE,
                              lables = agelabels)]
expss::val_lab(NMD$agegroups)

head(NMD)

str(NMD)
#library(expss)
#label_list <- list(ls(NMD))

#write.csv(label_list, file = "label_list.csv")




# Create a colour palette using the `colortools` package
#pay_more_pal <- sequential(color = "darkturquoise", percentage = 11, 
#what = "value")
#(seasonal <- ggplot(NMD, aes(x = Year, y = pay_more),
#group = agegroups, 
#rm.na = TRUE) +
#geom_bar((stat = "identity"), (aes( colour = pay_more))) +
#theme_classic() +
#scale_color_manual(values = pay_more_pal))


########################
#drop_NA <- subset(NMD, pay_more != "NA")

#table(drop_NA$pay_more)

#table(NMD$pay_more)

#table(droplevels(drop_NA)$pay_more)

#NMD %>%
# drop_na(pay_more)%>%
#ggplot(NMD, aes_(x = agegroups,
#          fill = pay_more )) +
#geom_density(alpha = 0.3) +
#   labs(title = "Salary distribution by rank")



#NMD %>%
#filter(!is.na(pay_more)) %>%
#ggplot(aes(x= agegroups, fill= pay_more)) + 
#geom_density(alpha = 0.3)
#geom_bar(stat = "count")


#NMD %>%
#filter(!is.na(pay_more)) %>%
#ggplot(aes(x= pay_more, fill= agegroups)) + 
#geom_density(alpha = 0.6)


#NMD %>%
#filter(!is.na(pay_more)) %>%
#ggplot(aes(x= pay_more, fill= agegroups)) + 
#geom_stripped_cols(stat = "identity") #(alpha = 0.6)



#geom_bar(stat = "count")


mean(NMD$age)
range(NMD$age)

table1 <-  table(
  NMD$gender,
  
  NMD$edu_level,
  
  NMD$pay_more,
  
  NMD$env_con_food,
  
  NMD$Year,
  
  NMD$buy_often,
  
  NMD$place_regular_grocery,
  
  NMD$place_farmers_market,
  
  NMD$place_farm_shop,
  
  NMD$place_special_store,
  
  NMD$place_food_festival,
  
  NMD$place_elsewhere,
  
  NMD$not_purchased,
  
  NMD$freq_ost,
  
  NMD$freq_beer,
  
  NMD$good_selection_avail)

#stargazer(table1, type= "Text")
stargazer(as.data.frame(NMD),type = "text", out = "summary.txt",
          align = TRUE)
stargazer(as.data.frame(NMD),type = "html", title = "Summary Table" , 
          align = TRUE,
          out = "summary_table_1.html")


ggpairs(NMD[,c("gender",
               "age",
               "edu_level",
               "pay_more",
               "env_con_food",
               "buy_often",
               "place_regular_grocery",
               "place_farmers_market",
               "place_farm_shop",
               "place_special_store",
               "place_food_festival",
               "place_elsewhere",
               "not_purchased",
               "freq_ost",
               "freq_beer",
               "good_selection_avail"
)])

#correlationmatrix <- cor(NMD[,c("gender",
#                  "age",
#                 "edu_level",
#                 "pay_more",
#                 "env_con_food",
#                 "buy_often",
#                 "place_regular_grocery",
#                 "place_farmers_market",
#                 "place_farm_shop",
#                 "place_special_store",
#                 "place_food_festival",
#                 "place_elsewhere",
#                 "not_purchased",
#                 "freq_ost",
#                 "freq_beer",
#                 "good_selection_avail",
#                 use="pairwise.complete.obs"
#)])
#correlationmatrix
class(NMD)

#stargazer(correlationmatrix, 
#         title="Correlation Matrix",
#         type = "html",
#         out = "correlation.html",
#         align = TRUE,
#         column.sep.width = "1pt"
#         )


#Estimation of model (1) using Logit
logit1_NMD <- glm (   pay_more ~
                        gender+
                        age+
                        edu_level+
                        env_con_food+
                        buy_often+
                        place_regular_grocery+
                        place_farmers_market+
                        place_farm_shop+
                        place_special_store+
                        place_food_festival+
                        place_elsewhere+
                        not_purchased+
                        freq_ost+
                        freq_beer+
                        good_selection_avail,
                      family = binomial (link= "logit"),
                      data = NMD)

logit1_NMD


#Estimation of model(2) using probit

probit1_NMD <- glm(pay_more ~
                     env_con_food+
                     buy_often+
                     place_regular_grocery+
                     place_farmers_market+
                     place_farm_shop+
                     place_special_store+
                     place_food_festival+
                     place_elsewhere+
                     not_purchased+
                     freq_ost+
                     freq_beer+
                     good_selection_avail,
                   family = binomial (link= "probit"),
                   data = NMD)


probit1_NMD

#Estimation of model(3) using probit with control for age, gender
#and education level

probit2_NMD <- glm (   pay_more ~
                         gender+
                         age+
                         edu_level+
                         env_con_food+
                         buy_often+
                         place_regular_grocery+
                         place_farmers_market+
                         place_farm_shop+
                         place_special_store+
                         place_food_festival+
                         place_elsewhere+
                         not_purchased+
                         freq_ost+
                         freq_beer+
                         good_selection_avail,
                       family = binomial (link= "probit"),
                       data = NMD)

probit2_NMD

#Estimation of model(4) using probit with control for  gender
#education level and agegroup

probit3_NMD <- glm (   pay_more ~
                         gender+
                         agegroups+
                         edu_level+
                         env_con_food+
                         buy_often+
                         place_regular_grocery+
                         place_farmers_market+
                         place_farm_shop+
                         place_special_store+
                         place_food_festival+
                         place_elsewhere+
                         not_purchased+
                         freq_ost+
                         freq_beer+
                         good_selection_avail,
                       family = binomial (link= "probit"),
                       data = NMD)

probit3_NMD

#load heteroskedasticity-robust standard errors of coefficient 
#estimators in a list

for_print_se <- list(sqrt(diag(vcovHC(logit1_NMD, type = "HC1"))),
                     sqrt(diag(vcovHC(probit1_NMD, type = "HC1"))),  
                     sqrt(diag(vcovHC(probit2_NMD, type = "HC1"))),
                     sqrt(diag(vcovHC(probit3_NMD, type = "HC1")))
                     
)

for_print_se

stargazer(logit1_NMD, probit1_NMD, probit2_NMD, 
          probit3_NMD,
          digits = 3,
          type = "text",
          title = "NMD Regression Table" , align = TRUE, 
          out = "Regression_Table.txt",
          header = FALSE,
          se = for_print_se,
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)","(3)", "(4)"))

#Hypothesis Test


linearHypothesis(probit3_NMD, 
                 test = "F",
                 c("gender = 0"
                 ),
                 vcov = vcovHC, type = "HC1")


linearHypothesis(probit3_NMD, 
                 test = "F",
                 c("age = 0",
                   "Year = 0"
                 ),
                 vcov = vcovHC, type = "HC1")


#linearHypothesis(probit4_intfam, 
#                test = "F",
#                c("edu_level = 0",
#                  "........ = 0",
#                  "............. = 0"
#                ),
#                vcov = vcovHC, type = "HC1")

#linearHypothesis(probit3_NMD, 
#                test = "F",
#                c("age = 0",
#                  "gender = 0",
#                  "age:Year = 0",
#                  ),
#                vcov = vcovHC, type = "HC1")



wilcox.test(pay_more~gender,data = NMD)


#Ordinal_Regression

## one at a time, table apply, pared, and public

lapply(NMD[, c("pay_more", "agegroups", "gender", "edu_level", 
               "env_con_food")], table)
## three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ pay_more + agegroups + gender + edu_level +
               env_con_food, data = NMD))


library(MASS)
model_1<-polr(as.factor(pay_more)~ as.factor(gender) + 
                as.factor(edu_level)
              + agegroups + as.factor(env_con_food), data= NMD, Hess=T)
summary(model_1)
#store the coefficient table, then calculate
#the p-values and combine back with the table.
# store coeff table
coeff_table <- coef(summary(model_1))
coeff_table
#calculate and store p values

p <- pnorm(abs(coeff_table[, "t value"]), lower.tail = FALSE) * 2

## combined table
coeff_table <- cbind(coeff_table, "p value" = round(p,3))

coeff_table



(ci <- confint(model_1))

exp(coef(model_1))

#Interpreting these Odds ratios we are essentially comparing the people 
#who are in groups greater than x versus those who are in groups less 
#than or equal to x, where x is the level of the response variable. 
#Hence for a one unit change in the predictor variable, the odds for 
#cases in a group that is greater than x versus less than or equal to
#x are the proportional odds times larger. So for say the \income" 
#variable a one unit increase in this variable, the
#odds of high \Answer" versus the combined adjacent \Answer" 
#categories are 0.8499098 times greater, given the
#other variables are held constant in the model.

is.factor(NMD$pay_more)
expss::val_lab(NMD$edu_level)


class(NMD$pay_more)
barplot(table(NMD$edu_level,NMD$pay_more),
        beside=T,args.legend=list(cex=0.5),
        cex.names=0.7,legend.text=c("Master",
                                    "Bachelor",
                                    "11-13 års skolegang",
                                    "9-10 års skolegang",
                                    "8 års skolegang"
        ))
kruskal.test(pay_more~edu_level,data = NMD)          

boxplot(pay_more~gender,data=NMD,names=c("Mann","Kvinne"),
        ylab="Willingness to Pay")

#one way ANOVA
anova(lm(edu_level~pay_more,data=NMD))

t.test(pay_more~gender,data=NMD)

#chisq_test
chisq.test(table(NMD$pay_more,NMD$gender))

chisq.test(table(NMD$pay_more,NMD$edu_level))

chisq.test(table(NMD$pay_more, NMD$place_regular_grocery))

chisq.test(table(NMD$pay_more, NMD$buy_often))

chisq.test(table(NMD$pay_more, NMD$place_special_store))




boxplot(pay_more~edu_level,data=NMD,
        names=c("8 års ",
                "9-10 års",
                "11-13 års",
                "Bachelor",
                "Master"),
        ylab="Willingness to pay more")
#Two-way ANOVA

anova(lm(pay_more~gender+edu_level+agegroups,data=NMD))





#############################LMER Model#######
install.packages("dummies")
library(dummies)
NMD <- cbind(NMD, dummy(NMD$pay_more, sep = "_"))

expss::val_lab(NMD$pay_more)

#ifelse(NMD$pay_more ==1 | NMD$pay_more ==2, 1, NMD$pay_more)
NMD$dummy <- ifelse(NMD$pay_more ==1 | NMD$pay_more ==2, 1,
                    NMD$pay_more)
NMD$dummy <- ifelse(NMD$pay_more ==3 | NMD$pay_more ==4, 0,
                    NMD$dummy)

class(NMD$dummy)
as.factor(NMD$dummy)


pay_more_Breaks <- c(1, 2, 4)

pay_more_labels <- c("Yes", "NO")

setDT(NMD)[, WTP_pay_more := cut(pay_more,
                                 breaks = pay_more_Breaks,
                                 right = FALSE,
                                 lables = pay_more_labels)]
expss::val_lab(NMD$WTP_pay_more)

#ggplot(NMD, aes(x = pay_more, y = edu_level)) +
#  stat_sum(aes(size = ..n.., group = 1)) +
#  scale_size_area(max_size=10)

#tmp <- melt(NMD[, c("pay_more", "edu_level", "age", "buy_often")],
#            id.vars="pay_more", na.rm = TRUE)
#ggplot(tmp, aes(factor(pay_more), y = value, na.rm = TRUE, fill=factor(pay_more))) +
# geom_boxplot() +
#facet_wrap(~variable, scales="free_y")

expss::val_lab(NMD$pay_more)

library(lme4)

m <- glmer(pay_more ~ age + gender + edu_level + buy_often + 
             (1 | rspnr), data = NMD, family = binomial, 
           control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)

# print the mod results without correlations among fixed effects
print(m, corr = FALSE)

#this is the end