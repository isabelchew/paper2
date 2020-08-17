install.packages("readxl")
install.packages("here")
install.packages("stringi")
install.packages("mlogit")
install.packages("margins")
install.packages("cjoint")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
install.packages("ggpubr")
install.packages("likert")

library(readxl)
library(here)
library(dplyr)
library(tidyverse)
library(stringi)
library(mlogit)
library(margins)
library(cjoint)
library(ggplot2)
library(ggpubr)
library(scales)
library(likert)

# load my datasets from excel 

raw1<-read_excel("/Users/isabelchew/Desktop/Dissertation/SURVEY APRIL 2020/DATA FROM THE FIRST PRE-TEST/Kachin State survey - Burmese (Zawgyi and Unicode) - 27 March 2020 - Copy_April 10, 2020_10.20 2 ENGLISH_2.xlsx")

raw2<-read_excel("/Users/isabelchew/Desktop/Dissertation/SURVEY APRIL 2020/DATA FROM THE FIRST PRE-TEST/Jinghpaw OR Burmese CHOICE_April 10, 2020_10.21 ENGLISH_2.xlsx")

raw3<-read_excel("/Users/isabelchew/Desktop/Dissertation/SURVEY APRIL 2020/DATA FROM THE FIRST PRE-TEST/Kachin State survey - Burmese - 27 March 2020 - Jingphaw_April 10, 2020_10.20 ENGLISH_2.xlsx")

# Merge the files into 1 df

raw4<-bind_rows(raw2, raw3)
survey<-bind_rows(raw1, raw4)
survey<-survey[-37,] # drop empty row

colnames(survey) # check on the column names 

duplicated(survey$`IP Address`) # check for duplicated IP addresses

# Data cleaning 

survey$Age<-as.factor(survey$Age)
survey$Age[survey$Age=="< 21"] <- "<21"
table(survey$Age) # <21: 14, 21-29: 28, 30-39: 2, 40-49: 2
survey$Age<-factor(survey$Age) # drop un-used survey

survey$Gender<-as.factor(survey$Gender)
table(survey$Gender) # 25 F vs. 21 M

survey$`Educaton level`<-as.factor(survey$`Educaton level`)
table(survey$`Educaton level`) # high sch: 3, some/grad uni: 43
survey$`Educaton level`[survey$`Educaton level`=="Some/Completed University or more"]<-"Some/Completed university or more"
survey$`Educaton level`<-factor(survey$`Educaton level`)

# clean up the columns needed for conjoint analysis

survey<-rename(survey,c(indvltrust_1=`Which individual are you more likely to trust...33`,
                indvltrust_2=`Which individual are you more likely to trust...34`))

survey$indvltrust_1[survey$indvltrust_1=="B လူ"]<-"B"
survey$indvltrust_1[survey$indvltrust_1=="A လူ"]<-"A"
survey$indvltrust_2[survey$indvltrust_2=="B လူ"]<-"B"
survey$indvltrust_2[survey$indvltrust_2=="A လူ"]<-"A"

survey$Name_1A<- ifelse(grepl("လဖိုင် ဇော်လက်",survey$`1A`),"Lahpai Zau Lat",ifelse(grepl("အောင်ကျောင်ကို",survey$`1A`),"Aung Kyaung Ko", "Sai Maung Maung"))
survey$Name_1B<- ifelse(grepl("လဖိုင် ဇော်လက်",survey$`1B`),"Lahpai Zau Lat",ifelse(grepl("အောင်ကျောင်ကို",survey$`1B`),"Aung Kyaung Ko", "Sai Maung Maung"))
survey$Name_2A<- ifelse(grepl("အောင်မင်းဦး",survey$`2A`),"Aung Min Oo",ifelse(grepl("စိုင်းစိုးဝင်",survey$`2A`),"Sai Soe Win", "Maran Naw Seng"))
survey$Name_2B<- ifelse(grepl("အောင်မင်းဦး",survey$`2B`),"Aung Min Oo",ifelse(grepl("စိုင်းစိုးဝင်",survey$`2B`),"Sai Soe Win", "Maran Naw Seng"))

survey$Name_1A[grepl("Lahpai Zau Lat", survey$`1A`)]<-"Lahpai Zau Lat"
survey$Name_1B[grepl("Lahpai Zau Lat", survey$`1B`)]<-"Lahpai Zau Lat"
survey$Name_1A[grepl("Aung Kyaung Ko", survey$`1A`)]<-"Aung Kyaung Ko"
survey$Name_1B[grepl("Aung Kyaung Ko", survey$`1B`)]<-"Aung Kyaung Ko"

survey$Name_2A[grepl("Aung Min Oo", survey$`2A`)]<-"Aung Min Oo"
survey$Name_2B[grepl("Aung Min Oo", survey$`2B`)]<-"Aung Min Oo"
survey$Name_2A[grepl("Sai Soe Win", survey$`2A`)]<-"Sai Soe Win"
survey$Name_2B[grepl("Sai Soe Win", survey$`2B`)]<-"Sai Soe Win"

survey$Hometown_1A<-ifelse(grepl("လှည်းကူးမြို့နယ်",survey$`1A`),"Hlegu, Yangon","Waingmaw, Kachin")
survey$Hometown_1B<-ifelse(grepl("လှည်းကူးမြို့နယ်",survey$`1B`),"Hlegu, Yangon","Waingmaw, Kachin")
survey$Hometown_2A<-ifelse(grepl("လှည်းကူးမြို့နယ်",survey$`2A`),"Hlegu, Yangon","Waingmaw, Kachin")
survey$Hometown_2B<-ifelse(grepl("လှည်းကူးမြို့နယ်",survey$`2B`),"Hlegu, Yangon","Waingmaw, Kachin")

survey$Hometown_1A[grep("Hlegu", survey$`1A`)]<-"Hlegu, Yangon"
survey$Hometown_1B[grep("Hlegu", survey$`1B`)]<-"Hlegu, Yangon" 
survey$Hometown_2A[grep("Hlegu", survey$`2A`)]<-"Hlegu, Yangon" 
survey$Hometown_2B[grep("Hlegu", survey$`2B`)]<-"Hlegu, Yangon" 

survey$indvlReligion_1A<-ifelse(grepl("ခရစ်ယာန်",survey$`1A`),"Christian","Buddhist")
survey$indvlReligion_1B<-ifelse(grepl("ခရစ်ယာန်",survey$`1B`),"Christian","Buddhist")
survey$indvlReligion_2A<-ifelse(grepl("ခရစ်ယာန်",survey$`2A`),"Christian","Buddhist")
survey$indvlReligion_2B<-ifelse(grepl("ခရစ်ယာန်",survey$`2B`),"Christian","Buddhist")

survey$indvlReligion_1A[grep("Hkristan", survey$`1A`)]<-"Christian"
survey$indvlReligion_1B[grep("Hkristan", survey$`1B`)]<-"Christian"
survey$indvlReligion_2A[grep("Hkristan", survey$`2A`)]<-"Christian"
survey$indvlReligion_2B[grep("Hkristan", survey$`2B`)]<-"Christian"

survey$occ_1A<-ifelse(grepl("လမ်းဘေးဈေးသည်",survey$`1A`),"Vendor", "Farmer")
survey$occ_1B<-ifelse(grepl("လမ်းဘေးဈေးသည်",survey$`1B`),"Vendor", "Farmer")
survey$occ_2A<-ifelse(grepl("လမ်းဘေးဈေးသည်",survey$`2A`),"Vendor", "Farmer")
survey$occ_2B<-ifelse(grepl("လမ်းဘေးဈေးသည်",survey$`2B`),"Vendor", "Farmer")

survey$occ_1A[grep("Gat dut", survey$`1A`)]<-"Vendor"
survey$occ_1B[grep("Gat dut", survey$`1B`)]<-"Vendor"
survey$occ_2A[grep("Gat dut", survey$`2A`)]<-"Vendor"
survey$occ_2B[grep("Gat dut", survey$`2B`)]<-"Vendor"

survey$edu_1A<-ifelse(grepl("မူလတန်းအောင်",survey$`1A`),"primary sch","secondary sch")
survey$edu_1B<-ifelse(grepl("မူလတန်းအောင်",survey$`1B`),"primary sch","secondary sch")
survey$edu_2A<-ifelse(grepl("မူလတန်းအောင်",survey$`2A`),"primary sch","secondary sch")
survey$edu_2B<-ifelse(grepl("မူလတန်းအောင်",survey$`2B`),"primary sch","secondary sch")

survey$edu_1A[grep("Lawu tsang hpaji ngut", survey$`1A`)]<-"primary sch"
survey$edu_1B[grep("Lawu tsang hpaji ngut", survey$`1B`)]<-"primary sch"
survey$edu_2A[grep("Lawu tsang hpaji ngut", survey$`2A`)]<-"primary sch"
survey$edu_2B[grep("Lawu tsang hpaji ngut", survey$`2B`)]<-"primary sch"

survey$JP_1A<-ifelse(grepl("ဂျင်းဖော့",survey$`1A`),1,0)
survey$JP_1B<-ifelse(grepl("ဂျင်းဖော့",survey$`1B`),1,0)
survey$JP_2A<-ifelse(grepl("ဂျင်းဖော့",survey$`2A`),1,0)
survey$JP_2B<-ifelse(grepl("ဂျင်းဖော့",survey$`2B`),1,0)

survey$JP_1A[grep("Jinghpaw", survey$`1A`)]<-1
survey$JP_1B[grep("Jinghpaw", survey$`1B`)]<-1
survey$JP_2A[grep("Jinghpaw", survey$`2A`)]<-1
survey$JP_2B[grep("Jinghpaw", survey$`2B`)]<-1

survey$Shan_1A<-ifelse(grepl("ရှမ်း",survey$`1A`),1,0)
survey$Shan_1B<-ifelse(grepl("ရှမ်း",survey$`1B`),1,0)
survey$Shan_2A<-ifelse(grepl("ရှမ်း",survey$`2A`),1,0)
survey$Shan_2B<-ifelse(grepl("ရှမ်း",survey$`2B`),1,0)

survey$Shan_1A[grep("Sam", survey$`1A`)]<-1
survey$Shan_1B[grep("Sam", survey$`1B`)]<-1
survey$Shan_2A[grep("Sam", survey$`2A`)]<-1
survey$Shan_2B[grep("Sam", survey$`2B`)]<-1

survey[40,109:136]<- NA
survey[44,109:136]<- NA

# create a dataset that only contains those who answered conjoint questions 

survey_conjoint<-survey%>%filter(!is.na(indvltrust_1))
save(survey_conjoint, file="survey_conjoint.Rda")
            
# create a long dataset

survey_long<-survey_conjoint%>%pivot_longer(cols=c("Name_1A":"Shan_2B"),
                                   names_to=c(".value","set"),
                                   names_sep="_",
                                   names_repair="minimal")

survey_long$alt<-substr(survey_long$set,2,2)
survey_long$set<-substr(survey_long$set,1,1)

# create a DV

survey_long$selected<-ifelse(survey_long$set=="1", ifelse(survey_long$indvltrust_1==survey_long$alt,1,0), NA)
survey_long$selected[survey_long$set=="2" & survey_long$indvltrust_2==survey_long$alt]<-1
survey_long$selected[survey_long$set=="2" & survey_long$indvltrust_2!=survey_long$alt]<-0

# Create dummy variables for each of the IV

survey_long$conjoint.kachin<-ifelse(survey_long$Name=="Lahpai Zau Lat"|survey_long$Name=="Maran Naw Seng", 1,0)
survey_long$conjoint.shan<-ifelse(survey_long$Name=="Sai Maung Maung"|survey_long$Name=="Sai Soe Win", 1,0)
survey_long$conjoint.bamar<-ifelse(survey_long$Name=="Aung Kyaung Ko"|survey_long$Name=="Aung Min Oo", 1,0)

survey_long$conjoint.Waingmaw<-ifelse(survey_long$Hometown=="Waingmaw, Kachin", 1,0)
survey_long$conjoint.YGN<-ifelse(survey_long$Hometown=="Hlegu, Yangon", 1,0)

survey_long$conjoint.Christian<-ifelse(survey_long$indvlReligion=="Christian", 1,0)
survey_long$conjoint.Buddhist<-ifelse(survey_long$indvlReligion=="Buddhist", 1,0)

survey_long$conjoint.farmer<-ifelse(survey_long$occ=="Farmer", 1,0)
survey_long$conjoint.vendor<-ifelse(survey_long$occ=="Vendor", 1,0)

survey_long$conjoint.prisch<-ifelse(survey_long$edu=="primary sch", 1,0)
survey_long$conjoint.secsch<-ifelse(survey_long$edu=="secondary sch", 1,0)

survey_long$selected<-as.integer(survey_long$selected)
survey_long<-survey_long%>%filter(!is.na(selected))

save(survey_long, file="survey_long.Rda")

###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################

# using cjoint package 

attribute_list <- list()
attribute_list[["cEthnicity"]] <- c("Kachin", "Bamar", "Shan")
attribute_list[["cHometown"]] <- c("Waingmaw", "Hlegu")
attribute_list[["cReligion"]] <- c("Christian", "Buddhist")
attribute_list[["cOcc"]] <- c("Farmer", "Vendor")
attribute_list[["cEdu"]] <- c("Primary", "Secondary")
attribute_list[["cLanguage"]] <- c("Bamar", "Jinghpaw and Shan and Bamar", "Jinghpaw and Bamar", "Shan and Bamar")

constraint_list<-list()

# constraints on ethnicity, region and languages understood

constraint_list[[1]]<- list()
constraint_list[[1]][["cEthnicity"]]<-c("Kachin")
constraint_list[[1]][["cHometown"]] <- c("Waingmaw")
constraint_list[[1]][["cLanguage"]] <- c("Bamar", "Shan and Bamar")

constraint_list[[2]]<- list()
constraint_list[[2]][["cEthnicity"]]<-c("Kachin")
constraint_list[[2]][["cHometown"]] <- c("Hlegu")
constraint_list[[2]][["cLanguage"]] <- c("Shan and Bamar", "Jinghpaw and Shan and Bamar")

constraint_list[[3]]<- list()
constraint_list[[3]][["cEthnicity"]]<-c("Shan")
constraint_list[[3]][["cHometown"]] <- c("Waingmaw")
constraint_list[[3]][["cLanguage"]] <- c("Jinghpaw and Bamar", "Bamar")

constraint_list[[4]]<- list()
constraint_list[[4]][["cEthnicity"]]<-c("Shan")
constraint_list[[4]][["cHometown"]] <- c("Hlegu")
constraint_list[[4]][["cLanguage"]] <- c("Jinghpaw and Bamar", 
                                        "Jinghpaw and Shan and Bamar")

constraint_list[[5]]<- list()
constraint_list[[5]][["cEthnicity"]]<-c("Bamar")
constraint_list[[5]][["cHometown"]] <- c("Hlegu")
constraint_list[[5]][["cLanguage"]] <- c("Jinghpaw and Bamar","Jinghpaw and Shan and Bamar", "Shan and Bamar")

surveydesign<-makeDesign(type="constraints", attribute.levels=attribute_list,
                         constraints=constraint_list)

# create correct dataframe for cjoint package

survey_cjoint<-survey_long%>%mutate(cEthnicity = case_when(Name=="Lahpai Zau Lat"|Name=="Maran Naw Seng" ~ "Kachin",
                                                           Name=="Sai Maung Maung"|Name=="Sai Soe Win" ~ "Shan",
                                                           Name== "Aung Min Oo"|Name=="Aung Kyaung Ko"  ~ "Bamar"))%>%
  mutate(cHometown = case_when(Hometown=="Hlegu, Yangon" ~ "Hlegu",
                                                          Hometown=="Waingmaw, Kachin" ~ "Waingmaw"))%>%
  rename("cReligion" = indvlReligion)%>%
  rename("cOcc" = occ)%>%
  mutate(cEdu = case_when(edu=="primary sch" ~ "Primary",
                            edu=="secondary sch" ~ "Secondary"))%>%
  mutate(cLanguage = case_when(Shan==1 & JP==1 ~ "Jinghpaw and Shan and Bamar",
                               Shan==1 & JP==0 ~ "Shan and Bamar",
                               Shan==0 & JP==1 ~ "Jinghpaw and Bamar",
                               Shan==0 & JP==0 ~ "Bamar"))

cols=c("cEthnicity", "cHometown", "cReligion","cOcc", "cEdu", "cLanguage")
survey_cjoint[cols]<-lapply(survey_cjoint[cols], factor)

survey_cjoint<-rename(survey_cjoint, "state"=`Which state/region were you born in? `)
survey_cjoint<-survey_cjoint%>%filter(state!="Yangon region")

survey_cjoint$Jinghpaw<-ifelse(grepl("Jinghpaw", survey_cjoint$Ethnicity)|survey_cjoint$Ethnicity=="Kachin"&survey_cjoint$`First language`!="Rawang"|survey_cjoint$Ethnicity=="Lahpai", 1,0)

survey_cjoint$Jinghpaw<-replace_na(survey_cjoint$Jinghpaw,1)
survey_cjoint$Jinghpaw<-as.factor(survey_cjoint$Jinghpaw)

results<-amce(selected~ cEthnicity  + cHometown + cReligion + cOcc + cEdu + cLanguage, data=survey_cjoint, respondent.id = "`Response ID`", cluster=TRUE, design=surveydesign)

summary(results)

plot(results, main="AMCE of trust", xlab = "Change in e[Y]", ci=0.95, plot.display="all")

# Interaction effect with Jinghpaw individuals 

results1<-amce(selected~ Jinghpaw*cEthnicity + Jinghpaw*cHometown + Jinghpaw*cReligion + Jinghpaw*cOcc + Jinghpaw*cEdu + Jinghpaw*cLanguage, data=survey_cjoint, respondent.id = "`Response ID`", cluster=TRUE, respondent.varying="Jinghpaw", design=surveydesign)

summary(results1)

plot(results1, main="AMCE of trust", xlab = "Change in e[Y]", ci=0.95, plot.display="interaction")

# Interaction effect with Hometown

baselines<-list()
baselines$cEthnicity<-"Kachin"
survey_cjoint$conjoint.Waingmaw<-as.factor(survey_cjoint$conjoint.Waingmaw)

results2<-amce(selected~ cEthnicity + conjoint.Waingmaw*cEthnicity + cHometown + cReligion + cOcc + cEdu + cLanguage, data=survey_cjoint, respondent.id = "`Response ID`", baselines = baselines, cluster=TRUE, respondent.varying="conjoint.Waingmaw", design=surveydesign)

summary(results2)

plot(results2, main="AMCE of trust", xlab = "Change in e[Y]", ci=0.95, plot.display="interaction")

survey_cjoint%>%select(c("set", "alt", "conjoint.Waingmaw", "selected", "cHometown", "cEthnicity"))%>%View()

results3<-amce(selected~ conjoint.Waingmaw*cEthnicity + conjoint.Waingmaw*cReligion + conjoint.Waingmaw*cOcc + conjoint.Waingmaw*cEdu + conjoint.Waingmaw*cLanguage, data=survey_cjoint, respondent.id = "`Response ID`", baselines = baselines, cluster=TRUE, respondent.varying="conjoint.Waingmaw", design=surveydesign)

plot(results3, main="AMCE of trust", xlab = "Change in e[Y]", ci=0.95, plot.display="interaction")

# Interaction effect with co-ethnicity
survey_Kachin<-filter(survey_cjoint, survey_cjoint$Ethnicity!="Bamar"&survey_cjoint$Ethnicity!="Gurkha"&
                               survey_cjoint$Ethnicity!="Kachin Bamar"&survey_cjoint$Ethnicity!="Kachin Chin Shan"&survey_cjoint$Ethnicity!="Taileng"&survey_cjoint$Ethnicity!="Naga")

table(survey_Kachin$Ethnicity)

survey_Kachin$coethnic<-ifelse(survey_Kachin$cEthnicity=="Kachin",1,0)

survey_Kachin$coethnic<-as.factor(survey_Kachin$coethnic)

results4<-amce(selected~ coethnic*cEthnicity + coethnic*cHometown + coethnic*cReligion + coethnic*cOcc + coethnic*cEdu + coethnic*cLanguage, data=survey_Kachin, respondent.id = "`Response ID`", baselines = baselines, cluster=TRUE, respondent.varying="coethnic", design=surveydesign)

summary(results4)

plot(results4, main="AMCE of trust", xlab = "Change in e[Y]", ci=0.95, plot.display="interaction")

# survey_cjoint%>%select(c("Ethnicity", "First language", "Jinghpaw"))%>%View() # check 

###############################################
# Descriptive Stats
###############################################

survey%>%View()
survey<-rename(survey, "state"=`Which state/region were you born in? `)
survey<-filter(survey, state!="Yangon region")

# Let's first look at trust levels across the different entities

cols1=c("Trust - family","Trust - ward/village","Trust - same religion",
        "Trust - different religion", "Trust - same ethnicity","Trust - different ethnicity",
        "Trust - Myanmar government","Trust - Kachin state government","Trust - Tatmadaw")       

survey[cols1]<-lapply(survey[cols1], factor)

ggplot(survey, aes(x=factor(`Trust - family`)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))

save(survey, file="survey.Rda")

# Create a dataset just for trust to convert into long dataset
trust<-survey[,c(9,62:70)]
trust<-rename(trust, "Trust_family"=`Trust - family`,
              "Trust_village"=`Trust - ward/village`,
              "Trust_samereligion"=`Trust - same religion`,
              "Trust_diffreligion"=`Trust - different religion`,
              "Trust_sameethnicity"=`Trust - same ethnicity`,
              "Trust_diffethnicity"=`Trust - different ethnicity`,
              "Trust_MMgovt"=`Trust - Myanmar government`,
              "Trust_stategovt"=`Trust - Kachin state government`,
              "Trust_Tatmadaw"=`Trust - Tatmadaw`,
              )

trust_long<-pivot_longer(trust, cols=c("Trust_family":"Trust_Tatmadaw"), 
                         names_to = c(".value", "Entity"),
                         names_sep = "_",
                         names_repair="minimal")

trust_long$Trust<-fct_relevel(trust_long$Trust, c("Do not trust at all", "Do not trust very much",
                                                  "Trust somewhat", "Trust completely"))


trust_cons<-trust_long%>%group_by(Entity, Trust)%>%summarise(count=n())%>%
  mutate(perc=count/sum(count))

save(trust_cons, file="trust_cons")
  
trust_cons%>%
  filter(Entity=="family"|Entity=="village"|Entity=="sameethnicity"|Entity=="diffethnicity"|Entity=="samereligion"|Entity=="diffreligion")%>%
  filter(!is.na(Trust))%>%
  ggplot(aes(x=Trust, y=perc*100, fill=Entity)) + geom_bar(stat='identity', position=position_dodge()) + labs(y="percent")

# create multiple graphs in 1 plot

family_village_graph<-trust_cons%>%
  filter(Entity=="family"|Entity=="village")%>%
  filter(!is.na(Trust))%>%
  ggplot(aes(x=Trust, y=perc*100, fill=Entity)) + geom_bar(stat='identity', position=position_dodge())+ labs(y="percent")+scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme(axis.title.x = element_blank())+scale_fill_manual(name = "Trust", values=c("brown","aquamarine4"))

ethnicity_graph<-trust_cons%>%
  filter(Entity=="village"|Entity=="sameethnicity"|Entity=="diffethnicity")%>%
  filter(!is.na(Trust))%>%
  ggplot(aes(x=Trust, y=perc*100, fill=Entity)) + geom_bar(stat='identity', position=position_dodge()) + labs(y="percent")+scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme(axis.title.x = element_blank())+scale_fill_manual(name = "Trust", values=c("steelblue3","steelblue4","aquamarine4"))

religion_graph<-trust_cons%>%
  filter(Entity=="village"|Entity=="samereligion"|Entity=="diffreligion")%>%
  filter(!is.na(Trust))%>%
  ggplot(aes(x=Trust, y=perc*100, fill=Entity)) + geom_bar(stat='identity', position=position_dodge())+ labs(y="percent")+scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme(axis.title.x = element_blank())+scale_fill_manual(name = "Trust", values=c("mediumpurple1","mediumpurple4","aquamarine4"))

govt_graph<-trust_cons%>%
  filter(Entity=="MMgovt"|Entity=="stategovt"|Entity=="Tatmadaw")%>%
  filter(!is.na(Trust))%>%
  ggplot(aes(x=Trust, y=perc*100, fill=Entity)) + geom_bar(stat='identity', position=position_dodge())+ labs(y="percent")+scale_x_discrete(guide = guide_axis(n.dodge = 2))+theme(axis.title.x = element_blank())+scale_fill_manual(name = "Trust", values=c("chocolate4","chocolate3", "chocolate1"))

ggarrange(family_village_graph,ethnicity_graph,religion_graph, govt_graph,
            labels = c("Village", "Ethnicity", "Religion", "Governments"), ncol=2, nrow=2)













###############################################
###############################################
###############################################
###############################################
###############################################
###############################################
###############################################

# using mlogit which is possibly WRONG 

survey_mlogit<-mlogit.data(survey_long, choice="selected", shape="long", alt.var="alt", id.var="`Response ID`")

ml1<-mlogit(selected ~ conjoint.bamar + conjoint.shan + conjoint.Waingmaw 
            + conjoint.Christian + conjoint.farmer + conjoint.secsch + JP + Shan, survey_mlogit)

summary(ml1)

ml2<-mlogit(selected ~ conjoint.bamar + conjoint.shan + conjoint.Waingmaw 
            + conjoint.Christian + conjoint.farmer + conjoint.secsch + JP + Shan 
            + conjoint.Waingmaw*conjoint.bamar, survey_mlogit) # why does the effect on ethnicity disappear when we interact with hometown?

summary(ml2)
summary(margins(ml2))

table(survey_long$`Which state/region were you born in? `) # probably need to remove those who are not from Kachin state
survey_long<-rename(survey_long, "state"=`Which state/region were you born in? `)

survey_Kachinstate<-subset(survey_long, state!="Yangon region")

survey_Kachinstate_mlogit<-mlogit.data(survey_Kachinstate, choice="selected", shape="long", alt.var="alt", id.var="`Response ID`")

ml1_Kachinstate<-mlogit(selected ~ conjoint.bamar + conjoint.shan + conjoint.Waingmaw 
            + conjoint.Christian + conjoint.farmer + conjoint.secsch + JP + Shan + conjoint.bamar*conjoint.Waingmaw 
            + conjoint.shan*conjoint.Waingmaw, survey_Kachinstate_mlogit)

summary(ml1_Kachinstate)

table(survey_long$Ethnicity) # split into two samples

survey_long%>%filter(Ethnicity=="Kachin")%>%view()
survey_long%>%filter(state!="Kachin state")%>%view()

# subsetting into JP and non-JP

# survey_JP<-survey_Kachinstate%>%filter(grepl("Jinghpaw", Ethnicity)|Ethnicity=="Kachin")
# survey_nonJP<-survey_Kachinstate%>%filter(!grepl("Jinghpaw", Ethnicity)&Ethnicity!="Kachin")

survey_JP<-survey_Kachinstate%>%filter(`First language`=="Jinghpaw")
survey_nonJP<-survey_Kachinstate%>%filter(`First language`!="Jinghpaw")

survey_JP_mlogit<-mlogit.data(survey_JP, choice="selected", shape="long", alt.var="alt", id.var="`Response ID`")
survey_nonJP_mlogit<-mlogit.data(survey_nonJP, choice="selected", shape="long", alt.var="alt", id.var="`Response ID`")

ml1_JP<-mlogit(selected ~ conjoint.bamar + conjoint.shan + conjoint.Waingmaw 
                        + conjoint.Christian + conjoint.farmer + conjoint.secsch + JP + Shan +conjoint.Waingmaw*JP, survey_JP_mlogit)

summary(ml1_JP)

ml1_nonJP<-mlogit(selected ~ conjoint.bamar + conjoint.shan + conjoint.Waingmaw 
               + conjoint.Christian + conjoint.farmer + conjoint.secsch + JP + Shan + conjoint.Waingmaw*JP, survey_nonJP_ml

summary(ml1_nonJP)

# look at the do you trust someone of the same ethnicity questions to see the answer - is there a difference with the conjoint answer


