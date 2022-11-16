# personality-traits-and-blood-donation-
Blood donation and personality traits
# Personality-blood_donation_project
Personality-blood_donation_project
## preparing programming
library(foreign)
library(psych)
library(GPArotation)
library(plyr) 
library(lavaan)
library(readr)
library(corrplot)
library(RColorBrewer)
library(tidyverse)
library(naniar)
library(Hmisc)
library(tidySEM)
library(ppcor)
library(boot)
library(dplyr)
library(ggplot2)
library(scales)

#### preparing the dataset
setwd("C:/Ting/Ting_2021_work/Other_cohorts_for_Prosocial_behavior/WLS/R_analysis")

#### read WLS cohort version 13_08
WLS<-read.spss("wls_bl_13_08.sav", use.value.labels=F, to.data.frame=TRUE)
dim(WLS)

#####----------######################
##### Variables in R4. Variables related to personality traits
#### z_rh001rec	R4 Summary score for Extraversion.
#### z_rh002rec	R4 Number of Extraversion items answered.
#### z_rh003rec	R4 Summary score for Openness.
#### z_rh004rec+D1400:E2374	R4 Number of Openness items answered.
#### z_rh005rec	R4 Summary score for Neuroticism.
#### z_rh006rec	R4 Number of Neuroticism items answered.
#### z_rh007rec	R4 Summary score for Conscientiousness.
#### z_rh008rec	R4 Number of Conscientiousness items answered.
#### z_rh009rec	R4 Summary score for Agreeableness.
#### z_rh010rec	R4 Number of Agreeableness items answered.
#### z_rh011re	R4 To what extent do you agree that you see yourself as someone who is outgoing and sociable?
#### z_rh012re	R4 To what extent do you agree that you see yourself as someone who is inventive?
#### z_rh013re	R4 To what extent do you agree that you see yourself as someone who worries a lot?
#### z_rh014re	R4 To what extent do you agree that you see yourself as someone who has a forgiving nature?
#### z_rh015re	R4 To what extent do you agree that you see yourself as someone who can be somewhat careless?
#### z_rh016re	R4 To what extent do you agree that you see yourself as someone who tends to be quiet?
#### z_rh017re	R4 To what extent do you agree that you see yourself as someone who prefers work that is routine and simple?
#### z_rh018re	R4 To what extent do you agree that you see yourself as someone who tends to find fault with others?
#### z_rh019re	R4 To what extent do you agree that you see yourself as someone who is easily distracted?
#### z_rh020re	R4 To what extent do you agree that you see yourself as someone who is relaxed and handles stress well?
#### z_mh001rec	R4 Summary score for extraversion.
#### z_mh001rei	R4 Summary score for extraversion, mean of valid items substituted for missing items.
#### z_mh002re	R4 Number of extraversion items answered.
#### z_mh003rer	R4 To what extent do you agree that you see yourself as someone who is talkative?
#### z_mh004rer	R4 To what extent do you agree that you see yourself as someone who is reserved?
#### z_mh005rer	R4 To what extent do you agree that you see yourself as someone who is full of energy?
#### z_mh006rer	R4 To what extent do you agree that you see yourself as someone who tends to be quiet?
#### z_mh007rer	R4 To what extent do you agree that you see yourself as someone who is sometimes shy, inhibited?
#### z_mh008rer	R4 To what extent do you agree that you see yourself as someone who generates a lot of enthusiasm?
#### z_mh009rec	R4 Summary score for agreeableness.
#### z_mh009rei	R4 Summary score for agreeableness, mean of valid items substituted for missing items.
#### z_mh010re	R4 Number of agreeableness items answered.
#### z_mh011rer	R4 To what extent do you agree that you see yourself as someone who tends to find fault with others?
#### z_mh012rer	R4 To what extent do you agree that you see yourself as someone who is sometimes rude to others?
#### z_mh013rer	R4 To what extent do you agree that you see yourself as someone who is generally trusting?
#### z_mh014rer	R4 To what extent do you agree that you see yourself as someone who can be cold and aloof?
#### z_mh015rer	R4 To what extent do you agree that you see yourself as someone who is considerate to almost everyone?
#### z_mh016rer	R4 To what extent do you agree that you see yourself as someone who likes to cooperate with others
#### z_mh017rec	R4 Summary score for conscientiousness.
#### z_mh017rei	R4 Summary score for conscientiousness,mean of valid items substituted for missing items.
#### z_mh018re	R4 Number of conscientiousness items answered.
#### z_mh019rer	R4 To what extent do you agree that you see yourself as someone who does a thorough job?
#### z_mh020rer	R4 To what extent do you agree that you see yourself as someone who is a reliable worker?
#### z_mh021rer	R4 To what extent do you agree that you see yourself as someone who tends to be disorganized?
#### z_mh022rer	R4 To what extent do you agree that you see yourself as someone who is lazy at times?
#### z_mh023rer	R4 To what extent do you agree that you see yourself as someone who does things efficiently?
#### z_mh024rer	R4 To what extent do you agree that you see yourself as someone who is easily distracted?
#### z_mh025rec	R4 Summary score for neuroticism.
#### z_mh025rei	R4 Summary score for neuroticism, mean of valid items substituted for missing items.
#### z_mh026re	R4 Number of neuroticism items answered.
#### z_mh027rer	R4 To what extent do you agree that you see yourself as someone who can be tense?
#### z_mh028rer	R4 To what extent do you agree that you see yourself as someone who is emotionally stable, not easily upset?
#### z_mh029rer	R4 To what extent do you agree that you see yourself as someone who worries a lot?
#### z_mh030rer	R4 To what extent do you agree that you see yourself as someone who remains calm in tense situations?
#### z_mh031rer	R4 To what extent do you agree that you see yourself as someone who gets nervous easily?
#### z_mh032rec	R4 Summary score for openness.
#### z_mh032rei	R4 Summary score for openness, mean of valid items substituted for missing items.
#### z_mh033re	R4 Number of openness items answered.
#### z_mh034rer	R4 To what extent do you agree that you see yourself as someone who prefers the conventional, traditional?
#### z_mh035rer	R4 To what extent do you agree that you see yourself as someone who prefers work that is routine and simple?
#### z_mh036rer	R4 To what extent do you agree that you see yourself as someone who values artistic, aesthetic experiences?
#### z_mh037rer	R4 To what extent do you agree that you see yourself as someone who has an active imagination?
#### z_mh038rer	R4 To what extent do you agree that you see yourself as someone who wants things to be simple and clear-cut?
#### z_mh039rer	R4 To what extent do you agree that you see yourself as someone who is sophisticated in art, music, or literature?
#### Suvery in 2003 - 2005
#### z_ih001rec	R5 Summary score for extraversion.
#### z_ih001rei	R5 Summary score for extraversion, mean imputed for missing components.
#### z_ih002re	R5 Number of extraversion items answered.
#### z_ih009rec	R5 Summary score for agreeableness.
#### z_ih009rei	R5 Summary score for agreeableness, mean imputed for missing components.
#### z_ih010re	R5 Number of agreeableness items answered.
#### z_ih017rec	R5 Summary score for conscientiousness.
#### z_ih017rei	R5 Summary score for conscientiousness, mean imputed for missing components.
#### z_ih018re	R5 Number of conscientiousness items answered.
#### z_ih025rec	R5 Summary score for neuroticism.
#### z_ih025rei	R5 Summary score for neuroticism, mean imputed for missing components.
#### z_ih026re	R5 Number of neuroticism items answered.
#### z_ih032rec	R5 Summary score for openness.
#### z_ih032rei	R5 Summary score for openness, imputed mean for missing components.
#### z_ih033re	R5 Number of openness items answered.

##### select variables in R5. Variables related to blood donation, education, volunteering behavior, social participantion, self healthy rate..... 
## z_ga003re	R5 Respondent's age at time of interview

names(WLS)
dput(names(WLS))

vva<-list()
vva$items<-c("idpub","selsibidpub","rtype", "personid","selsibtype", "z_brdxdy", "z_sexrsp","z_mh001rec",
             "z_mh009rec", "z_mh017rec", "z_mh025rec","z_mh032rec", "z_mu001rec","z_ga003re", "z_gb103red","z_ic001rer","z_gp226re",
             "z_gx201re","z_gr020rp","z_gv111re","z_gv101re","z_gv102re",	"z_gv110rea","z_gv115re", "z_gv116re","z_gv117re","z_gv118re",
             "gv112reb","gv113re", "z_gq700r", "z_gq701r","z_mh001rei","z_mh009rei","z_mh017rei","z_mh025rei","z_mh032rei",
             "z_ih001rec","z_ih009rec","z_ih017rec","z_ih025rec","z_ih032rec", "z_ih001rei","z_ih009rei","z_ih017rei","z_ih025rei","z_ih032rei")
Personality_screen = WLS[,vva$items]
dim(Personality_screen)

#### Descriptive analysis of the dataset
#### Select interesting variables: 
## idpub        ID
## z_ga003re   R5 Respondent's age at time of interview; 
## z_sexrsp    RS Sex of participant; 
### Education
## z_gb103red	 R5 How many years of education does R have based on his or her highest degree?
## z_gx201re   R5 In general, would you say your health is excellent, very good, good, fair, or poor?
## z_gp226re   R5 To what extent are you satisfied with your present financial situation?
## z_gr020rp	 R5 Do you own your own home, or are you renting?
### General information of volunteering behavior
## z_gv101re	R5 Did R do volunteer work in last 10 years?
## z_gv111re	R5 How many hours did participant volunteer during a typical month in the last 12 months? 
## gv113re	  R5 Number of hours per month graduate spent volunteering in a typical month during the last year.  
## z_gv102re	R5 How regularly did graduate volunteer in the last 10 years?
## gv112reb	  R5 For seasonal volunteers, typically how many hours did graduate spend volunteering in the last 12 months?
### Blood donation 
## z_gv115re	R5 Has R ever given a unit of blood for own use?
## z_gv116re	R5 Has R ever given blood for use by others?
## z_gv117re	R5 How many times has R given blood for use by others (over R's lifetime)?
## z_gv118re	R5 Has R given blood in the last 12 months (for use by others)?

#### data cleaning  #### 
### rename the variables

Personality_screen<-Personality_screen%>%
  rename(
    age_at_R5 = z_ga003re,
    marrige_state=z_ic001rer,
    gender = z_sexrsp,
    Financial_Satisfaction = z_gp226re,
    Home_ownship = z_gr020rp,
    Years_of_Education = z_gb103red,
    Health_statement = z_gx201re,
    Ever_volunteering = z_gv101re,
    Regular_Volunteer_last_10_years = z_gv102re,
    Hours_Volunteering = z_gv111re,
    Hours_Volunteering_1_year = gv112reb,
    Hours_Volunteering_1_month = gv113re,	
    Ever_blood_donation = z_gv116re, 
    Blood_donation_last_year = z_gv118re,
    Blood_for_own_use = z_gv115re,
    Number_blood_donation = z_gv117re,
    Types_of_volunteering = z_gv110rea,
    Charitable_contribution  = z_gq700r,
    Amount_of_charitable_contribution = z_gq701r,
    M_Extraversion_score_1992=z_mh001rec, 
    M_Openness_score_1992 = z_mh032rec,
    M_Neuroticism_score_1992 = z_mh025rec,
    M_Conscientiousness_score_1992=z_mh017rec,
    M_Agreeableness_score_1992 = z_mh009rec,
    M_depression_1992=z_mu001rec,
    extraversion_1992 = z_mh001rei,
    agreeableness_1992 = z_mh009rei,
    conscientiousness_1992 =z_mh017rei,
    neuroticism_1992 = z_mh025rei,
    openness_1992 = z_mh032rei,
    extraversion_2003= z_ih001rec,
    agreeableness_2003= z_ih009rec,
    conscientiousness_2003 = z_ih017rec,
    neuroticism_2003 = z_ih025rec,
    openness_2003 = z_ih032rec,
    extraversion_2003_imputed= z_ih001rei,
    agreeableness_2003_imputed= z_ih009rei,
    conscientiousness_2003_imputed = z_ih017rei,
    neuroticism_2003_imputed = z_ih025rei,
    openness_2003_imputed = z_ih032rei)


#### Select the participants Graduators, not the sibling
Graduators_Personality<-Personality_screen[which(Personality_screen$rtype=="g"),]
dim(Graduators_Personality)

###  Remove missing varibles
vec <- c(-1, -2, -3, -4, -5)
Graduators_Personality<- Graduators_Personality%>% replace_with_na_all(condition = ~.x %in% vec)

#### Reverse the coding 
Graduators_Personality= Graduators_Personality %>%
  mutate_at(c( "marrige_state","Ever_volunteering", "Ever_blood_donation", "Blood_for_own_use",
               "Blood_donation_last_year", "Charitable_contribution","Amount_of_charitable_contribution"),
            ~ recode(., "1" = 1, "2" = 0 ))

Graduators_Personality= Graduators_Personality%>%
  mutate_at(c( "Health_statement","Financial_Satisfaction"),
            ~ recode(., "1" = 5, "2" = 4, "3" = 3,  "4" = 2, "5" = 1))
Graduators_Personality = Graduators_Personality%>%
  mutate_at(c( "Home_ownship"), ~ recode(.,  "2" = 0, "3" = 0, 
                                         "4" = 0, "5" = 0, "6" = 0 , "7"=0, "8"=0,"9"=0, "19"=0,"20"=0))

### transform these never donate blood/ involved in help/emotional support/ house_work_help. babysitting as 0 

Graduators_Personality<-transform(Graduators_Personality, Blood_donation_last_year = ifelse(Ever_blood_donation == 0, 0, Blood_donation_last_year))
Graduators_Personality<-transform(Graduators_Personality, Number_blood_donation = ifelse(Ever_blood_donation == 0, 0, Number_blood_donation))
Graduators_Personality<-transform(Graduators_Personality, Amount_of_charitable_contribution  = ifelse(Charitable_contribution == 0, 0, Amount_of_charitable_contribution))

############################---------------------##########################################
#### Select the participants Siblings
Siblings_Personality<-Personality_screen[which(Personality_screen$rtype=="s"),]
dim(Siblings_Personality)

Siblings_PRS<-PRS_multiple_traits[which(PRS_multiple_traits$rtype=="s"),]
dim(Siblings_PRS)

###  Remove missing varibles
vec <- c(-1, -2, -3, -4, -5)
Siblings_Personality<- Siblings_Personality%>% replace_with_na_all(condition = ~.x %in% vec)

#### Reverse the coding 

Siblings_Personality= Siblings_Personality %>%
  mutate_at(c( "marrige_state","Ever_volunteering", "Ever_blood_donation", "Blood_for_own_use",
               "Blood_donation_last_year", "Charitable_contribution","Amount_of_charitable_contribution"),
            ~ recode(., "1" = 1, "2" = 0 ))

Siblings_Personality= Siblings_Personality%>%
  mutate_at(c( "Health_statement","Financial_Satisfaction"),
            ~ recode(., "1" = 5, "2" = 4, "3" = 3, 
                     "4" = 2, "5" = 1))
Siblings_Personality= Siblings_Personality%>%
  mutate_at(c( "Home_ownship"),
            ~ recode(.,  "2" = 0, "3" = 0, 
                     "4" = 0, "5" = 0, "6" = 0 , "7"=0, "8"=0,"9"=0, "19"=0,"20"=0))

### transform these never donate blood/ involved in help/emotional support/ house_work_help. babysitting as 0 

Siblings_Personality<-transform(Siblings_Personality, Blood_donation_last_year = ifelse(Ever_blood_donation == 0, 0, Blood_donation_last_year))
Siblings_Personality<-transform(Siblings_Personality, Number_blood_donation = ifelse(Ever_blood_donation == 0, 0, Number_blood_donation))
Siblings_Personality<-transform(Siblings_Personality, Amount_of_charitable_contribution  = ifelse(Charitable_contribution == 0, 0, Amount_of_charitable_contribution))

##############---------------------------------------------------------------########

### Additional work on multiple traits files for WLS 
#### read Polygenic score for multiple traits file for WLS cohort 

PRS_multiple_traits<-read_dta("PGIrepo_v1.0_idpub_shuffled.dta")
dim(PRS_multiple_traits)

###### Select polygenic score for personality traitsin Multiple PRS data 
vvva<-list()
vvva$items<-c( "idpub","rtype","pgi_extrasingle","pgi_neurosingle","pgi_opensingle","pgi_extramulti","pgi_neuromulti","pc1_PGI_shuffled", 
               "pc2_PGI_shuffled","pc3_PGI_shuffled","pc4_PGI_shuffled", "pc5_PGI_shuffled","pc6_PGI_shuffled",
               "pc7_PGI_shuffled" ,"pc8_PGI_shuffled","pc9_PGI_shuffled", "pc10_PGI_shuffled", "pc11_PGI_shuffled",
               "pc12_PGI_shuffled","pc13_PGI_shuffled","pc14_PGI_shuffled", "pc15_PGI_shuffled", "pc16_PGI_shuffled",
               "pc17_PGI_shuffled","pc18_PGI_shuffled","pc19_PGI_shuffled","pc20_PGI_shuffled" )
PRS_personality_traits = PRS_multiple_traits[,vvva$items]

#### Select the participants Graduators, not the sibling
G_PRS<-PRS_personality_traits[which(PRS_personality_traits$rtype=="g"),]
dim(G_PRS)

### Merging with Personality dataset R5 dataset: data=R5_G_volunteering_S
G_Personality_PRS_scores<-merge(Graduators_Personality,G_PRS,by=c("idpub"))
dim(G_Personality_PRS_scores)

#### Select the participants Siblings, not the graduators
Siblings_PRS<-PRS_personality_traits[which(PRS_personality_traits$rtype=="s"),]
dim(Siblings_PRS)
S_Personality_PRS_scores<-merge(Siblings_Personality,Siblings_PRS,by=c("idpub"))
dim(S_Personality_PRS_scores)
##### Merging G and S subsamples
G_Personality_PRS_scores$G_S_type<-1
S_Personality_PRS_scores$G_S_type<-2
All_cohort<-rbind(G_Personality_PRS_scores,S_Personality_PRS_scores)
All_cohort0<-rbind(G_Personality_PRS_scores,S_Personality_PRS_scores)

#####-----------------------------------------------------------------------------####################
#### Delete missings in personality traits and blood donation

All_cohort<-subset(All_cohort,All_cohort$Ever_blood_donation>=0 & All_cohort$extraversion_2003>=0 & All_cohort$agreeableness_2003>=0
                            & All_cohort$conscientiousness_2003>=0 & All_cohort$neuroticism_2003>=0 & All_cohort$openness_2003>=0)
##### Description of variables 
dim(All_cohort)
summary(All_cohort)
describe(All_cohort)
##### Calculate z-scores
#### Personality traits

All_cohort$Neuroticism_zscore<-(All_cohort$neuroticism_2003-mean(All_cohort$neuroticism_2003))/sd(All_cohort$neuroticism_2003)
All_cohort$extraversion_zscore<-(All_cohort$extraversion_2003-mean(All_cohort$extraversion_2003))/sd(All_cohort$extraversion_2003)
All_cohort$agreeableness_zscore<-(All_cohort$agreeableness_2003-mean(All_cohort$agreeableness_2003))/sd(All_cohort$agreeableness_2003)
All_cohort$conscientiousness_zscore<-(All_cohort$conscientiousness_2003-mean(All_cohort$conscientiousness_2003))/sd(All_cohort$conscientiousness_2003)
All_cohort$openness_zscore<-(All_cohort$openness_2003-mean(All_cohort$openness_2003))/sd(All_cohort$openness_2003)

#### polygenic scores
All_cohort$extraversion_ps<-(All_cohort$pgi_extramulti-mean(All_cohort$pgi_extramulti))/sd(All_cohort$pgi_extramulti)
All_cohort$neuroticism_ps<-(All_cohort$pgi_neuromulti-mean(All_cohort$pgi_neuromulti))/sd(All_cohort$pgi_neuromulti)
All_cohort$openess_ps<-(All_cohort$pgi_opensingle-mean(All_cohort$pgi_opensingle))/sd(All_cohort$pgi_opensingle)

### get density plot for specific numeric columns: personality traits, polygenic score for personality traits
plotting<-list()
plotting$items<-c("extraversion_2003", "agreeableness_2003",  "conscientiousness_2003","neuroticism_2003", "openness_2003", "neuroticism_ps","openess_ps","extraversion_ps")
data_for_plot= All_cohort[,plotting$items]
data_for_plot%>% keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value))+
  facet_wrap (~key, scales="free")+
  geom_density(color="blue", fill="red")

#### Pie table for blood donation variables
Ever_Donation <- c("Yes", "No")
Numbers <- c(3052,2676)
blood_donation <- data.frame(Ever_Donation, Numbers)
pie(blood_donation$Numbers, labels = Ever_Donation,main="Pie Chart of blood donation (n=5728)" )
pct <- round(100*blood_donation$Numbers/sum(blood_donation$Numbers))
pie(blood_donation$Numbers, labels = paste(Ever_Donation, sep = " ",pct,"%"),
                             main="Pie Chart of blood donation (n=5728)" )

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

ggplot(data = blood_donation, aes(x = "", y = Numbers, fill = Ever_Donation)) + 
  geom_bar(width = 1,stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+ blank_theme + 
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = Numbers + c(1, cumsum(Numbers)[-length(Numbers)]), 
                label = percent(Numbers/5728)), size=8,nudge_x=0.2, nudge_y = 0.2) + theme_minimal()

#####%%%%%%%%%%%%%%%%%%%%%%%%%%#############################

##### correlation analysis for R5_Multiple_traits_G
#### 25: Ever_blood_donation; 26: The number of blood donation;27: Bloo donation in the last year; 21: ever-volunteering
#### 32-36 Big five personality traits in  1992; 37-41 Big five personality traits in 2003; 42-46 Big five personality traits imputed in 2003;
###  48:"pgi_extrasingle"  49, "pgi_neurosingle" , 50,"pgi_opensingle"; 51: pgi_extramulti ; 52: pgi_neuromulti; 

mat1<- All_cohort[, c(25,26,27,79,80,81)]
mydata.rcorr1 = rcorr(as.matrix(mat1))
mydata.rcorr1
corrplot(mydata.rcorr2$r, type = "upper", tl.col = "black", tl.srt = 45)

mat2<- All_cohort[, c(25,26,27,37,38,39,40,41)]
mydata.rcorr2 = rcorr(as.matrix(mat2))
mydata.rcorr2
corrplot(mydata.rcorr3$r, type = "upper", tl.col = "black", tl.srt = 45)


##### Regression model
### Extraversion model
#### fit1 for Ever_blood donation
fit1_extraversion<-glm(Ever_blood_donation ~ extraversion_2003+age_at_R5+gender+Years_of_Education+Health_statement+G_S_type,family = "binomial", data=All_cohort)
summary(fit1_extraversion)
fit1_extraversion_ps_only<-glm(Ever_blood_donation ~ extraversion_ps
                         +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                         +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                           pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit1_extraversion_ps_only)

fit1_extraversion_ps<-glm(Ever_blood_donation ~ extraversion_2003+extraversion_ps
                         +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                         +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                           pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit1_extraversion_ps)

#### fit2 for numbers blood donation

fit2_extraversion<-glm(Number_blood_donation ~ extraversion_2003+age_at_R5+gender+Years_of_Education+Health_statement+G_S_type,family = "gaussian", data=All_cohort)
summary(fit2_extraversion)
fit2_extraversion_ps_only<-glm(Number_blood_donation ~ extraversion_ps
                              +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                              +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                                pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "gaussian", data=All_cohort)
summary(fit2_extraversion_ps_only)
fit2_extraversion_ps<-glm(Number_blood_donation ~ extraversion_2003+extraversion_ps
                         +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                         +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                           pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "gaussian", data=All_cohort)
summary(fit2_extraversion_ps)

#### fit3 for Recent blood donation
fit3_extraversion<-glm(Blood_donation_last_year ~ extraversion_2003+age_at_R5+gender+Years_of_Education+Health_statement+G_S_type,family = "binomial", data=All_cohort)
summary(fit3_extraversion)
fit3_extraversion_ps_only<-glm(Blood_donation_last_year ~ extraversion_ps
                               +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                               +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                                 pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit3_extraversion_ps_only)

fit3_extraversion_ps<-glm(Blood_donation_last_year ~ extraversion_2003+extraversion_ps
                          +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                          +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                            pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit3_extraversion_ps)

######--------------------------------#######################################
### Neuroticism model#####
#### fit1 for Ever_blood donation

fit1_neuroticism<-glm(Ever_blood_donation ~ neuroticism_2003+age_at_R5+gender+Years_of_Education+Health_statement+G_S_type,family = "binomial", data=All_cohort)
summary(fit1_neuroticism)

fit1_neuroticism_ps_only<-glm(Ever_blood_donation ~ neuroticism_ps
                         +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                         +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                           pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit1_neuroticism_ps_only)
confint(fit1_neuroticism_ps_only)
fit1_neuroticism_ps<-glm(Ever_blood_donation ~ neuroticism_2003+neuroticism_ps
                        +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                        +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                          pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit1_neuroticism_ps)

#### Mediational model for neuroticism-ps, meurotcism and blood donation

process (data=All_cohort,y="Ever_blood_donation",x="neuroticism_ps",m=c("neuroticism_2003"),
         cov=c("age_at_R5","gender","Years_of_Education", "Health_statement", "pc1_PGI_shuffled","pc2_PGI_shuffled", 
               "pc3_PGI_shuffled","pc4_PGI_shuffled","pc5_PGI_shuffled","pc6_PGI_shuffled", 
               "pc7_PGI_shuffled", "pc8_PGI_shuffled", "pc9_PGI_shuffled","pc10_PGI_shuffled"),
         model=4,contrast=1,normal=1,conf=90,save=1, stand=1)        
####### fit2  for numbers of blood donation

fit2_neuroticism<-glm(Number_blood_donation ~ neuroticism_2003+age_at_R5+gender+Years_of_Education+Health_statement+G_S_type,family = "gaussian", data=All_cohort)
summary(fit2_neuroticism)
fit2_neuroticism_ps_only<-glm(Number_blood_donation ~ neuroticism_ps+age_at_R5+gender+Years_of_Education+Health_statement
                               +G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled +pc3_PGI_shuffled+pc4_PGI_shuffled
                              +pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+pc8_PGI_shuffled+pc9_PGI_shuffled
                              +pc10_PGI_shuffled,family = "gaussian", data=All_cohort)
summary(fit2_neuroticism_ps_only)

fit2_neuroticism_ps<-glm(Number_blood_donation ~ neuroticism_2003+neuroticism_ps
                          +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                          +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                            pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "gaussian", data=All_cohort)
summary(fit2_neuroticism_ps)

#### fit3 for Recent blood donation
fit3_neuroticism<-glm(Blood_donation_last_year ~ neuroticism_2003+age_at_R5+gender+Years_of_Education+Health_statement
                      +G_S_type,family = "binomial", data=All_cohort)
summary(fit3_neuroticism)
fit3_neuroticism_ps_only<-glm(Blood_donation_last_year ~ neuroticism_ps
                               +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                               +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                                 pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit3_neuroticism_ps_only)
fit3_neuroticism_ps<-glm(Blood_donation_last_year ~ neuroticism_2003+neuroticism_ps
                          +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                          +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                            pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit3_neuroticism_ps)
######--------------------------------#######################################
######--------------------------------#######################################
#### Openness model
#### fit1 for Ever_blood donation

fit1_openness<-glm(Ever_blood_donation ~ openness_2003+age_at_R5+gender+Years_of_Education+Health_statement+G_S_type,family = "binomial", data=All_cohort)
summary(fit_openness)
fit1_openness_ps_only<-glm(Ever_blood_donation ~ openess_ps
                        +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                        +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                          pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit1_openness_ps_only)
fit1_openness_ps<-glm(Ever_blood_donation ~ openness_2003+openess_ps
                     +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                     +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                       pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit1_openness_ps)
####### fit2  for numbers of blood donation

fit2_openness<-glm(Number_blood_donation ~ openness_2003+age_at_R5+gender+Years_of_Education+Health_statement+G_S_type,family = "gaussian", data=All_cohort)
summary(fit2_openness)

fit2_openness_ps_only<-glm(Number_blood_donation ~ openess_ps+age_at_R5+gender+Years_of_Education+Health_statement
                              +G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled +pc3_PGI_shuffled+pc4_PGI_shuffled
                              +pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+pc8_PGI_shuffled+pc9_PGI_shuffled
                              +pc10_PGI_shuffled,family = "gaussian", data=All_cohort)
summary(fit2_openness_ps_only)

fit2_openness_ps<-glm(Number_blood_donation ~ openness_2003+openess_ps
                         +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                         +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                           pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "gaussian", data=All_cohort)
summary(fit2_openness_ps)

#### fit3 for Recent blood donation
fit3_openness<-glm(Blood_donation_last_year ~ openness_2003+age_at_R5+gender+Years_of_Education+Health_statement
                      +G_S_type,family = "binomial", data=All_cohort)
summary(fit3_openness)

fit3_openness_ps_only<-glm(Blood_donation_last_year ~ openess_ps
                              +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                              +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                                pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit3_openness_ps_only)
fit3_openness_ps<-glm(Blood_donation_last_year ~ openness_2003+openess_ps
                         +age_at_R5+gender+Years_of_Education+Health_statement+G_S_type+pc1_PGI_shuffled+pc2_PGI_shuffled
                         +pc3_PGI_shuffled+pc4_PGI_shuffled+pc5_PGI_shuffled+pc6_PGI_shuffled+pc7_PGI_shuffled+
                           pc8_PGI_shuffled+pc9_PGI_shuffled+pc10_PGI_shuffled,family = "binomial", data=All_cohort)
summary(fit3_openness_ps)

############################-----------------------------------------------###############################################
#### Agreeableness model
#### fit1 for Ever_blood donation
fit1_agreeableness<-glm(Ever_blood_donation ~ agreeableness_2003+age_at_R5+gender+Years_of_Education+Health_statement+G_S_type,family = "binomial", data=All_cohort)
summary(fit_agreeableness)
####### fit2  for numbers of blood donation ######
fit2_agreeableness<-glm(Number_blood_donation ~ agreeableness_2003+age_at_R5+gender+Years_of_Education+Health_statement+G_S_type,family = "gaussian", data=All_cohort)
summary(fit2_agreeableness)

####### fit3 for Recent blood donation #######
fit3_agreeableness<-glm(Blood_donation_last_year ~ agreeableness_2003+age_at_R5+gender+Years_of_Education+Health_statement
                   +G_S_type,family = "binomial", data=All_cohort)
summary(fit3_agreeableness)

############################-----------------------------------------------###############################################
### concientiousness model
#######fit1 for Ever_blood donation #######
fit1_conscientiousness<-glm(Ever_blood_donation ~ conscientiousness_2003+age_at_R5+gender+Years_of_Education+Health_statement+G_S_type,family = "binomial", data=All_cohort)
summary(fit1_conscientiousness)
####### fit2  for numbers of blood donation #######
fit2_conscientiousness<-glm(Number_blood_donation ~ conscientiousness_2003+age_at_R5+gender+Years_of_Education+Health_statement+G_S_type,family = "gaussian", data=All_cohort)
summary(fit2_conscientiousness)

#### fit3 for Recent blood donation
fit3_conscientiousness<-glm(Blood_donation_last_year ~ conscientiousness_2003+age_at_R5+gender+Years_of_Education+Health_statement
                        +G_S_type,family = "binomial", data=All_cohort)
summary(fit3_conscientiousness)

### subset into active-donors, donors and non-donors

All_cohort$Types_donors<-All_cohort$Ever_blood_donation
All_cohort<-transform(All_cohort, Types_donors = ifelse(Ever_blood_donation == 0, "Non-donors", Types_donors))
All_cohort<-transform(All_cohort, Types_donors = ifelse(Ever_blood_donation== 1, "Donors", Types_donors))
All_cohort<-transform(All_cohort, Types_donors = ifelse(Blood_donation_last_year == 1, "Active-donors", Types_donors))
All_cohort$Types_donors2<-All_cohort$Types_donors
All_cohort<-transform(All_cohort, Types_donors2 = ifelse(Ever_blood_donation == 0, 0, Types_donors2))
All_cohort<-transform(All_cohort, Types_donors2 = ifelse(Ever_blood_donation== 1, 1, Types_donors2))
All_cohort<-transform(All_cohort, Types_donors2 = ifelse(Blood_donation_last_year == 1, 2, Types_donors2))

#### Subset into repeated donors
All_cohort$Repeated_donor<-All_cohort$Number_blood_donation
All_cohort<-transform(All_cohort, Repeated_donor= ifelse(Number_blood_donation == 0, "Non_donors", Repeated_donor))
All_cohort<-transform(All_cohort, Repeated_donor= ifelse(Number_blood_donation == 1, "Non_repeated_donors", Repeated_donor))
All_cohort<-transform(All_cohort, Repeated_donor= ifelse(Number_blood_donation >= 2, "Repeated_donors", Repeated_donor))


##### 
All_cohort$Repeated_donor_last_year<-All_cohort$Blood_donation_last_year
All_cohort<-transform(All_cohort, Repeated_donor_last_year= ifelse(All_cohort$Number_blood_donation>=2 & All_cohort$Blood_donation_last_year ==1, 2, Repeated_donor_last_year))

Donors <- c("Active-donors", "Donors", "No-donors")
Numbers <- c(594,2458,2676)
Blood_Donors <- data.frame(Donors, Numbers)
pie(Blood_Donors$Numbers, labels = Donors="Pie Chart- Types of donors (n=5728)" )

pct <- round(100*Blood_Donors$Numbers/sum(Blood_Donors$Numbers))
pie(Blood_Donors$Numbers, labels = paste(Donors, sep = " ",pct,"%"),
    main="Pie Chart- Types of donors (n=5728)" )

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

ggplot(data = Blood_Donors, aes(x = "", y = Numbers, fill = Donors)) + 
  geom_bar(width = 1,stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values=c("red", "green", "blue"))+ blank_theme + 
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = Numbers + c(1, cumsum(Numbers)[-length(Numbers)]), 
                label = percent(Numbers/5728)), size=8,nudge_x=0.2, nudge_y = 0.2) + theme_minimal()


ggplot(data = Blood_Donors, aes(x = "", y = Numbers, fill = Donors)) + 
  geom_bar(width = 1,stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  blank_theme + 
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = Numbers + c(1, cumsum(Numbers)[-length(Numbers)]), 
                label = round(Numbers), size=8,nudge_x=0.2, nudge_y = 0.2)) + theme_minimal()


ggplot(data = Blood_Donors, aes(x = "", y = Numbers, fill = Donors)) + 
  geom_bar(width = 1,stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  blank_theme + 
  theme(axis.text.x=element_blank()) +
   theme_minimal()


###Extraversion #######
aggregate(All_cohort$extraversion_2003,list(All_cohort$Types_donors), FUN=mean)
res1.aov <- aov(extraversion_2003 ~ Types_donors+age_at_R5+gender+Years_of_Education+G_S_type, data =All_cohort,type = 3, detailed = TRUE)
summary(res1.aov)
res1.aov
##### posthoc analysis #####
res1_posthoc <- aov(extraversion_2003 ~ Types_donors, data =All_cohort)
summary(res1_posthoc)
tukeyfit1<-TukeyHSD(res1_posthoc, conf.level=.95)
tukeyfit1

### Agreeableness ####
aggregate(All_cohort$agreeableness_2003,list(All_cohort$Types_donors), FUN=mean)
res2.aov <- aov(agreeableness_2003 ~ Types_donors+age_at_R5+gender+Years_of_Education+G_S_type, data =All_cohort)
summary(res2.aov)
## posthoc analysis##
res2_posthoc <- aov(agreeableness_2003 ~ Types_donors, data =All_cohort)
summary(res2_posthoc)
tukeyfit2<-TukeyHSD(res2_posthoc, conf.level=.95)
tukeyfit2
### Conscientiousness ###
aggregate(All_cohort$conscientiousness_2003,list(All_cohort$Types_donors), FUN=mean)
res3.aov <- aov(conscientiousness_2003 ~ Types_donors+age_at_R5+Years_of_Education+G_S_type, data =All_cohort)
summary(res3.aov)

## posthoc analysis##
res3_posthoc <- aov(conscientiousness_2003 ~ Types_donors, data =All_cohort)
summary(res3_posthoc)
tukeyfit3<-TukeyHSD(res3_posthoc, conf.level=.95)
tukeyfit3

### Neuroticism ###
aggregate(All_cohort$neuroticism_2003,list(All_cohort$Types_donors), FUN=mean)
res4.aov <- aov(neuroticism_2003 ~ Types_donors+age_at_R5+gender+Years_of_Education+G_S_type, data =All_cohort)
summary(res4.aov)
res4.aov

## posthoc analysis ##
res4_posthoc <- aov(neuroticism_2003 ~ Types_donors, data =All_cohort)
summary(res4_posthoc)
tukeyfit4<-TukeyHSD(res4_posthoc, conf.level=.95)
tukeyfit4

#### Openness ###
aggregate(All_cohort$openness_2003,list(All_cohort$Types_donors), FUN=mean)
res5.aov <- aov(openness_2003 ~ Types_donors+age_at_R5+gender+Years_of_Education+G_S_type, data =All_cohort)
summary(res5.aov)

## posthoc analysis ##
res5_posthoc <- aov(openness_2003 ~ Types_donors, data =All_cohort)
summary(res5_posthoc)
tukeyfit5<-TukeyHSD(res5_posthoc, conf.level=.95)
tukeyfit5

#### Openness polygenic score
aggregate(All_cohort$openess_ps,list(All_cohort$Types_donors), FUN=mean)
res6.aov <- aov(openess_ps ~ Types_donors+age_at_R5+gender+Years_of_Education+G_S_type, data =All_cohort)
summary(res6.aov)
## posthoc analysis ##
res6_posthoc <- aov(openess_ps ~ Types_donors, data =All_cohort)
summary(res6_posthoc)
tukeyfit6<-TukeyHSD(res6_posthoc, conf.level=.95)
tukeyfit6

aggregate(All_cohort$extraversion_ps,list(All_cohort$Types_donors), FUN=mean)
res7.aov <- aov(extraversion_ps ~ Types_donors+age_at_R5+gender+Years_of_Education+G_S_type, data =All_cohort)
summary(res7.aov)
## posthoc analysis ###
res7_posthoc <- aov(extraversion_ps ~ Types_donors, data =All_cohort)
summary(res7_posthoc)
tukeyfit7<-TukeyHSD(res7_posthoc, conf.level=.95)
tukeyfit7

aggregate(All_cohort$neuroticism_ps,list(All_cohort$Types_donors), FUN=mean)
res8.aov <- aov(neuroticism_ps ~ Types_donors+age_at_R5+gender+Years_of_Education+G_S_type, data =All_cohort)
summary(res8.aov)
## posthoc analysis###
res8_posthoc <- aov(neuroticism_ps ~ Types_donors, data =All_cohort)
summary(res8_posthoc)
tukeyfit8<-TukeyHSD(res8_posthoc, conf.level=.95)
tukeyfit8

ggplot(All_cohort, aes(x=Types_donors, y = extraversion_zscore,fill=Types_donors)) + 
  geom_boxplot(position = "dodge")+ylim(-4,4)+
  labs(title="Extraversion",x = "Types of donors", y="Extraversion")

ggplot(All_cohort, aes(x=Types_donors, y= agreeableness_zscore, fill=Types_donors)) + 
  geom_boxplot(position = "dodge")+ylim(-4,4)+
  labs(title="Agreeableness",x = "Types of donors", y="Agreeableness")

ggplot(All_cohort, aes(x=Types_donors, y = conscientiousness_zscore,fill=Types_donors)) +
  geom_boxplot(position = "dodge")+ylim(-4,4)+
  labs(title="Conscientiousness",x="Types of donors", y="Conscientiousness")

ggplot(All_cohort, aes(x=Types_donors, y = Neuroticism_zscore,fill=Types_donors)) + 
  geom_boxplot(position = "dodge")+ylim(-4,4)+
  labs(title=" Neuroticism",x = "Types of donors", y="Neuroticism")

ggplot(All_cohort, aes(x=Types_donors, y = openness_zscore,fill=Types_donors)) + 
  geom_boxplot(position = "dodge")+ylim(-4,4)+
  labs(title="Openness",y = "Types of donors", y="Openness")

ggplot(All_cohort, aes(x=Types_donors, y = neuroticism_ps,fill=Types_donors)) + 
  geom_boxplot(position = "dodge")+ylim(-4,4)+
  labs(title=" Neuroticism polygenic score",x = "Types of donors", y="Neuroticism polygenic score")

ggplot(All_cohort, aes(x=Types_donors, y =extraversion_ps,fill=Types_donors)) + 
  geom_boxplot(position = "dodge")+ylim(-4,4)+
  labs(title="Extraversion polygenic score",x = "Types of donors", y="Extraversion polygenic score")

ggplot(All_cohort, aes(x=Types_donors, y = openess_ps,fill=Types_donors)) + 
  geom_boxplot(position = "dodge")+ylim(-4,4)+
  labs(title="Openness polygenic score",x = "Types of donors", y="Openness polygenic score")

###### save results ###

sink("Personality_traits_blood_donation.txt")
print(describe(All_cohort))
print("######## Correlation coefficients")
print(mydata.rcorr1)
print(mydata.rcorr2)
print(ggplot(All_cohort, aes(x=Types_donors, y = extraversion_zscore,fill=Types_donors)) + 
        geom_boxplot(position = "dodge")+ylim(-4,4)+
        labs(title="Extraversion by the types of donors",x = "Types of donors", y="Extraversion"))

print(ggplot(All_cohort, aes(x=Types_donors, y= agreeableness_zscore, fill=Types_donors)) + 
        geom_boxplot(position = "dodge")+ylim(-4,4)+
        labs(title="Agreeableness by the types of donors",x = "Types of donors", y="Agreeableness"))

print(ggplot(All_cohort, aes(x=Types_donors, y = conscientiousness_zscore,fill=Types_donors)) +
        geom_boxplot(position = "dodge")+ylim(-4,4)+
        labs(title="Conscientiousness by the types of donors",x="Types of donors", y="Conscientiousness"))

print(ggplot(All_cohort, aes(x=Types_donors, y = Neuroticism_zscore,fill=Types_donors)) + 
        geom_boxplot(position = "dodge")+ylim(-4,4)+
        labs(title=" Neuroticism by the types of donors",x = "Types of donors", y="Neuroticism"))

print(ggplot(All_cohort, aes(x=Types_donors, y = openness_zscore,fill=Types_donors)) + 
        geom_boxplot(position = "dodge")+ylim(-4,4)+
        labs(title="Openness by the types of donors",y = "Types of donors", y="Openness"))

print(ggplot(All_cohort, aes(x=Types_donors, y = neuroticism_ps,fill=Types_donors)) + 
        geom_boxplot(position = "dodge")+ylim(-4,4)+
        labs(title=" Neuroticism polygenic score by the types of donors",x = "Types of donors", y="Neuroticism polygenic score"))
      
print(ggplot(All_cohort, aes(x=Types_donors, y =extraversion_ps,fill=Types_donors)) + 
              geom_boxplot(position = "dodge")+ylim(-4,4)+
              labs(title="Extraversion polygenic score by the types of donors",x = "Types of donors", y="Extraversion polygenic score"))
      
 print(ggplot(All_cohort, aes(x=Types_donors, y = openess_ps,fill=Types_donors)) + 
              geom_boxplot(position = "dodge")+ylim(-4,4)+
              labs(title="Openness polygenic score by the types of donors",x = "Types of donors", y="Openness polygenic score"))
      
sink()
