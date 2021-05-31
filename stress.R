---
  title: "Covid19-stress-related"
subtitle: "Analyzing stress and psychological effects of Covid-19"
output:
  html_document:
  df_print: paged
toc: yes
editor_options: 
  chunk_output_type: inline
---
  
  ``` {r setup, include=FALSE}
knitr::opts_chunk$set(cache = TRUE, message = FALSE, warning = FALSE)
```

```{r}
library(pacman)
p_load(qualtRics, tidyverse,ggrepel,stringr, multicon, psych,
       dplyr,plyr,modelr,plotly,sf,rworldmap,RColorBrewer,data.table)
```

# load csv survey
```{r}
d <- read_survey("csv-choice.csv")
```

# filter out participants who did not provide their age here!
```{r}
d <- filter(d, Consent == "Yes") %>% 
  filter(Dem_age >= 18)
```

#The marital status variable was mixed up, with the exception of English. The variable was recoded to correct for that problem.
```{r}
# Load function 'recode_if'(Aden-Buie & Gerke, 2018)
recode_if <- function(x, condition, ...) {
  if_else(condition, recode(x, ...), x)
}

# Fix differences in scoring between English and other languages 
d <- d %>%
  mutate(Dem_maritalstatus = 
           recode_if(Dem_maritalstatus, UserLanguage != "EN", 
                     "Single" = "Other or would rather not say",
                     "Married/cohabiting" = "Single",
                     "Divorced/widowed"= "Married/cohabiting",
                     "Other or would rather not say" = "Divorced/widowed"))

```

#Remove dashes in front of the response options
```{r}
d$Dem_edu <- str_remove(d$Dem_edu, "- ")
d$Dem_edu_mom <- str_remove(d$Dem_edu_mom, "- ")
```

#There were some participants who had “1” in Dem_edu. These responses were recoded as “Uninformative response”
```{r}
#Recode 1 as Uninformative responses
d$Dem_edu <- str_replace(d$Dem_edu, "^1", "Uninformative response")
d$Dem_edu_mom <- str_replace(d$Dem_edu_mom, "^1", "Uninformative response")
```

#The variable Dem_gender in Spain & Mexico was inverted – Male was recorded as Female and vice versa. Thus, the variable was recoded for SSP (Espanol - Espana) and SME (Espanol - Mexico)
```{r}
d <- d %>% 
  mutate(Dem_gender = ifelse(UserLanguage %in% c("SSP", "SME"),
                             case_when(Dem_gender == "Female" ~ "Male",
                                       Dem_gender == "Male" ~ "Female",
                                       Dem_gender == "Other/would rather not say" ~ "Other/would rather not say"),
                             Dem_gender))
```

#Recode AD_Check, AD_gain and AD_loss (shorten the response and turn the character to factor)
```{r}
d <- d %>% 
  mutate(AD_gain = factor(recode(AD_gain, 
                                 "· If Program A is adopted, 200 people will be saved." = "Program A",
                                 "· If Program B is adopted, there is 1/3 probability that 600 people will be saved, and 2/3 probability that no people will be saved" = "Program B")),
         AD_loss = factor(recode(AD_loss, 
                                 "· If Program C is adopted 400 people will die." = "Program C",
                                 "· If Program D is adopted there is 1/3 probability that nobody will die, and 2/3 probability that 600 people will die." = "Program D")),
         AD_check = factor(AD_check))
```

#Convert scale responses to numeric
#PSS10
```{r}
d <- d %>% mutate_at(
  .vars = vars(contains("PSS10")),
  .funs = recode, 
  "Never" = 1, 
  "Almost never" = 2,
  "Sometimes" = 3, 
  "Fairly often" = 4,
  "Very often" = 5
)
```

#Corona_concerns, Compliance, BFF, SPS, Coping, Expl_media
```{r}
d <- d %>% mutate_at(
  .vars = vars(matches("Corona_concerns|Compliance|BFF|SPS|Coping_\\d|Expl_media")),
  .funs = recode, 
  "Strongly disagree" = 1, 
  "Disagree" = 2,
  "Slightly disagree" = 3, 
  "Slightly agree" = 4,
  "Agree" = 5,
  "Strongly agree" = 6
)
```

#Distress scale
#note: 99 is “Does not apply to my current situation”
```{r}
d <- d %>% mutate_at(
  .vars = vars(matches("Distress_\\d")),
  .funs = recode, 
  "Strongly disagree" = 1, 
  "Disagree" = 2,
  "Slightly disagree" = 3, 
  "Slightly agree" = 4,
  "Agree" = 5,
  "Strongly agree" = 6,
  "Does not apply to my current situation" = 99
)
```

#Trust in the country’s measures
#Take care that this scale is coded in such a way that both 0 and 10 are defined as “not appropriate” (either too little or too much) and 5 is defined as appropriate.
```{r}
# Recoding Trust_country measure
d <- d %>% mutate(
  Trust_countrymeasure = recode(Trust_countrymeasure,
                                "Too little" = 0,
                                "1" = 1,
                                "2" = 2,
                                "3" = 3,
                                "4" = 4,
                                "Appropriate" = 5,
                                "6" = 6,
                                "7" = 7,
                                "8" = 8,
                                "9" = 9,
                                "Too much" = 10))
```

#Rename the PSS10_11 through PSS10_13 to Perceived loneliness (SLON-3)
#PSS10_11 to PSS10_13 are actually the Perceived loneliness items
```{r}
d <- d %>% 
  rename("Scale_SLON_1" = Scale_PSS10_UCLA_11,
         "Scale_SLON_2" = Scale_PSS10_UCLA_12,
         "Scale_SLON_3" = Scale_PSS10_UCLA_13) 
```

#cleaned dataset (beta)
```{r}
cleaned_d <- d[, c(5, 7, 10, 12:151)]
```


#let's start analyzing data

#what is the age of the respondents in eu?
```{r}
target <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic",
            "Denmark","Estonia","Finland","France","Germany","Greece","Hungary",
            "Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta",
            "Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia",
            "Spain","Sweden")

EU <- filter(cleaned_d, Country %in% target) 

#fixing date visualization
EU$RecordedDate <- as.Date(EU$RecordedDate , format= "%Y-%m-%d")

ggplot(EU)+
  stat_count(mapping=aes(Dem_age,fill=Country))
```

#what is the gender of the respondents? note: Missing values are omitted
```{r}
ggplot(EU %>%
         filter(!is.na(Dem_gender)), aes(x =Dem_gender))+
  stat_count()
```

#what type of work do the respondents? note: Missing values are omitted
```{r}
ggplot(EU %>%
         filter(!is.na(Dem_employment)), aes(Dem_employment))+
  stat_count()
```

#analyzing stress level in europe
#Scores are considered moderate above 2.4, and high above 3.7.

#PSS scores are obtained by reversing responses (e.g., 0 = 4, 1 = 3, 2 = 2, 3 = 1 & 4 = 0) to the four positively stated items (items 4, 5, 7, & 8) and then summing across all scale items. A short 4 item scale can be made from questions 2, 4, 5 and 10 of the PSS 10 item scale.
#note that the na values are omitted

```{r}
EU$Scale_PSS10_UCLA_4[EU$Scale_PSS10_UCLA_4 == 1] <- 5
EU$Scale_PSS10_UCLA_4[EU$Scale_PSS10_UCLA_4 == 2] <- 4
EU$Scale_PSS10_UCLA_4[EU$Scale_PSS10_UCLA_4 == 4] <- 2
EU$Scale_PSS10_UCLA_4[EU$Scale_PSS10_UCLA_4 == 5] <- 1
EU$Scale_PSS10_UCLA_5[EU$Scale_PSS10_UCLA_5 == 1] <- 5
EU$Scale_PSS10_UCLA_5[EU$Scale_PSS10_UCLA_5 == 2] <- 4
EU$Scale_PSS10_UCLA_5[EU$Scale_PSS10_UCLA_5 == 4] <- 2
EU$Scale_PSS10_UCLA_5[EU$Scale_PSS10_UCLA_5 == 5] <- 1
EU$Scale_PSS10_UCLA_7[EU$Scale_PSS10_UCLA_7 == 1] <- 5
EU$Scale_PSS10_UCLA_7[EU$Scale_PSS10_UCLA_7 == 2] <- 4
EU$Scale_PSS10_UCLA_7[EU$Scale_PSS10_UCLA_7 == 4] <- 2
EU$Scale_PSS10_UCLA_7[EU$Scale_PSS10_UCLA_7 == 5] <- 1
EU$Scale_PSS10_UCLA_8[EU$Scale_PSS10_UCLA_8 == 1] <- 5
EU$Scale_PSS10_UCLA_8[EU$Scale_PSS10_UCLA_8 == 2] <- 4
EU$Scale_PSS10_UCLA_8[EU$Scale_PSS10_UCLA_8 == 4] <- 2
EU$Scale_PSS10_UCLA_8[EU$Scale_PSS10_UCLA_8 == 5] <- 1


eu_stress <- select(EU,Country, Scale_PSS10_UCLA_1:Scale_PSS10_UCLA_10) %>%
  ddply( .(Country), summarize,
         Rate_PSS10_UCLA1=mean(Scale_PSS10_UCLA_1,na.rm=TRUE),
         Rate_PSS10_UCLA2=mean(Scale_PSS10_UCLA_2,na.rm=TRUE),
         Rate_PSS10_UCLA3=mean(Scale_PSS10_UCLA_3,na.rm=TRUE),
         Rate_PSS10_UCLA4=mean(Scale_PSS10_UCLA_4,na.rm=TRUE),
         Rate_PSS10_UCLA5=mean(Scale_PSS10_UCLA_5,na.rm=TRUE),
         Rate_PSS10_UCLA6=mean(Scale_PSS10_UCLA_6,na.rm=TRUE),
         Rate_PSS10_UCLA7=mean(Scale_PSS10_UCLA_7,na.rm=TRUE),
         Rate_PSS10_UCLA8=mean(Scale_PSS10_UCLA_8,na.rm=TRUE),
         Rate_PSS10_UCLA9=mean(Scale_PSS10_UCLA_9,na.rm=TRUE),
         Rate_PSS10_UCLA10=mean(Scale_PSS10_UCLA_10,na.rm=TRUE))%>%
  mutate(total_stress = rowMeans(select(., -Country)))
  

ggplot(eu_stress, aes(reorder(Country, total_stress), total_stress)) +
  geom_col(fill= "#00abff")+
  ylim(0,5)+
  coord_flip()

```

#are men or women more stressed?
```{r}
eu_stress_gender <- select(filter(EU,Dem_gender=="Male" | Dem_gender=="Female"), Dem_gender,
                           Scale_PSS10_UCLA_1:Scale_PSS10_UCLA_10) %>%
  ddply( .(Dem_gender), summarize,
         Rate_PSS10_UCLA1=mean(Scale_PSS10_UCLA_1,na.rm=TRUE),
         Rate_PSS10_UCLA2=mean(Scale_PSS10_UCLA_2,na.rm=TRUE),
         Rate_PSS10_UCLA3=mean(Scale_PSS10_UCLA_3,na.rm=TRUE),
         Rate_PSS10_UCLA4=mean(Scale_PSS10_UCLA_4,na.rm=TRUE),
         Rate_PSS10_UCLA5=mean(Scale_PSS10_UCLA_5,na.rm=TRUE),
         Rate_PSS10_UCLA6=mean(Scale_PSS10_UCLA_6,na.rm=TRUE),
         Rate_PSS10_UCLA7=mean(Scale_PSS10_UCLA_7,na.rm=TRUE),
         Rate_PSS10_UCLA8=mean(Scale_PSS10_UCLA_8,na.rm=TRUE),
         Rate_PSS10_UCLA9=mean(Scale_PSS10_UCLA_9,na.rm=TRUE),
         Rate_PSS10_UCLA10=mean(Scale_PSS10_UCLA_10,na.rm=TRUE))%>%
  mutate(total_stress = rowMeans(select(., -Dem_gender)))

ggplot(eu_stress_gender, aes(Dem_gender, total_stress)) + 
  geom_point(size=7) + 
  geom_segment(aes(x=Dem_gender, 
                   xend=Dem_gender, 
                   y=0, 
                   yend=total_stress)) + 
  ylim(0,4)+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
```


#How stress level changed in Europe during the first weeks of Covid pandemic?
```{r}
eu_stress_2months <- select(filter(EU,RecordedDate=="2020-03-30"|
                                     RecordedDate=="2020-04-07"|
                                     RecordedDate=="2020-04-14"|
                                     RecordedDate=="2020-04-21"|
                                     RecordedDate=="2020-04-28"|
                                     RecordedDate=="2020-05-05"|
                                     RecordedDate=="2020-05-12"|
                                     RecordedDate=="2020-05-20"|
                                     RecordedDate=="2020-05-26"|
                                     RecordedDate=="2020-06-5"|
                                     RecordedDate=="2020-06-10"), 
                            RecordedDate,Scale_PSS10_UCLA_1:Scale_PSS10_UCLA_10) %>%
  ddply( .(RecordedDate), summarize,
         Rate_PSS10_UCLA1=mean(Scale_PSS10_UCLA_1,na.rm=TRUE),
         Rate_PSS10_UCLA2=mean(Scale_PSS10_UCLA_2,na.rm=TRUE),
         Rate_PSS10_UCLA3=mean(Scale_PSS10_UCLA_3,na.rm=TRUE),
         Rate_PSS10_UCLA4=mean(Scale_PSS10_UCLA_4,na.rm=TRUE),
         Rate_PSS10_UCLA5=mean(Scale_PSS10_UCLA_5,na.rm=TRUE),
         Rate_PSS10_UCLA6=mean(Scale_PSS10_UCLA_6,na.rm=TRUE),
         Rate_PSS10_UCLA7=mean(Scale_PSS10_UCLA_7,na.rm=TRUE),
         Rate_PSS10_UCLA8=mean(Scale_PSS10_UCLA_8,na.rm=TRUE),
         Rate_PSS10_UCLA9=mean(Scale_PSS10_UCLA_9,na.rm=TRUE),
         Rate_PSS10_UCLA10=mean(Scale_PSS10_UCLA_10,na.rm=TRUE))%>%
  mutate(total_stress = rowMeans(select(., -RecordedDate)))

mod = lm( total_stress ~RecordedDate, data = eu_stress_2months)
summary(mod)
mod$coefficients

ggplot(eu_stress_2months, aes(RecordedDate, total_stress)) + 
  geom_point(size=7) + 
  geom_segment(aes(x=RecordedDate, 
                   xend=RecordedDate, 
                   y=0, 
                   yend=total_stress)) + 
  geom_abline(intercept = mod$coefficients[1], 
              slope = mod$coefficients[2], 
              color = "red")+
  ylim(0,3)
```
#the stress level in the first two weeks was higher than the predictive model, 
#in the third week it dropped and then increased and repeated 
#the cycle until the end of May. With the start of the summer 
#season, in June, the stress level then dropped


#Visualizing a detailed map of stress level in Europe
```{r}
worldMap <- getMap()
## identify EU countries
show <- which(worldMap$NAME %in% eu_stress$Country)

## WORLD coordinates
## this will be used as the background and will include non EU countries
plotCoords <-
  lapply(seq(worldMap$NAME),
         function(x) {
           ## collect long/lat in dataframe
           df <- lapply(worldMap@polygons[[x]]@Polygons,
                        function(x) x@coords)
           df <- do.call("rbind", as.list(df))
           df <- data.frame(df)
           
           ## add geographical name
           df$region <- as.character(worldMap$NAME[x])
           if (is.na(worldMap$NAME[x])) df$region <- "NONAME"
           
           ## add unique polygon identifier
           id <-
             rep(seq_along(worldMap@polygons[[x]]@Polygons),
                 sapply(worldMap@polygons[[x]]@Polygons,
                        function(x) nrow(x@coords)))
           df$group <- paste0(df$region, id)
           
           ## add column names and return dataframe
           colnames(df) <- list("long", "lat", "region", "group")
           return(df)
         })
plotCoords <- do.call("rbind", plotCoords)

## add EU identifier
plotCoords$EU <- 0
plotCoords$EU[which(plotCoords$region %in% eu_stress$Country)] <- 1

## for some reason, this group gives a horizontal segment across Europe
plotCoords <- plotCoords[plotCoords$group != "United States4", ]

## EU coordinates
showCoords <-
  lapply(show,
         function(x) {
           ## collect long/lat in dataframe
           df <- lapply(worldMap@polygons[[x]]@Polygons,
                        function(x) x@coords)
           df <- do.call("rbind", as.list(df))
           df <- data.frame(df)
           
           ## add geographical name
           df$region <- as.character(worldMap$NAME[x])
           if (is.na(worldMap$NAME[x])) df$region <- "NONAME"
           
           ## add unique polygon identifier
           id <-
             rep(seq_along(worldMap@polygons[[x]]@Polygons),
                 sapply(worldMap@polygons[[x]]@Polygons,
                        function(x) nrow(x@coords)))
           df$group <- paste0(df$region, id)
           
           ## add column names and return dataframe
           colnames(df) <- list("long", "lat", "region", "group")
           return(df)
         })
showCoords <- do.call("rbind", showCoords)

## add total stress level category
showCoords$total_stress<-
  eu_stress$total_stress[match(showCoords$region, eu_stress$Country)]
showCoords$total_stress <-as.numeric(showCoords$total_stress, unique(showCoords$total_stress))

#ggploting
ggplot() +
  geom_polygon(
    data = plotCoords,
    aes(x = long, y = lat, group = group),
    fill = "lightgrey", colour = "darkgrey", size = 0.1) +
  geom_polygon(
    data = showCoords,
    aes(x = long, y = lat, group = group, fill = total_stress),
    colour = "black", size = 0.1) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7", name="Mean")+
  scale_x_continuous(element_blank(), breaks = NULL) +
  scale_y_continuous(element_blank(), breaks = NULL) +
  coord_map(xlim = c(-26, 47),  ylim = c(32.5, 73)) 
```

#Sources of Distress among Europeans during the COVID-19 Pandemic
```{r}
eu_stress_source <- select(EU, Expl_Distress_1:Expl_Distress_24)%>%
                    na.omit()

eu_stress_source = data.frame(apply(eu_stress_source,2,function(x)mean(x[x<99]))) %>%
  tibble::rownames_to_column(var = "col1") %>%
  `colnames<-`(c("stress_source", "mean")) 
 

ggplot(eu_stress_source, aes(reorder(stress_source,mean), mean)) +
  geom_col(fill= "#00abff")+
  ylim(0,6)+
  coord_flip()

```
#trust in insititutions
#OECD guidelines on measuring insititutions trust:
#https://www.oecd.org/governance/oecd-guidelines-on-measuring-trust-9789264278219-en.htm
```{r}

eu_trust <- select(EU, OECD_insititutions_1:OECD_insititutions_6)%>%
  na.omit()

eu_trust = data.frame(apply(eu_trust,2,function(x)mean(x))) %>%
  tibble::rownames_to_column(var = "col1") %>%
  `colnames<-`(c("trust", "mean")) 


ggplot(eu_trust, aes(reorder(trust,mean), mean)) +
  geom_col(fill= "#00abff")+
  ylim(0,10)+
  coord_flip()


eu_mean_oecd4 <- select(EU,Country,OECD_insititutions_4) %>%
  ddply( .(Country), summarize,
         Rate_OECD_insititutions_4=mean(OECD_insititutions_4,na.rm=TRUE))%>%
        arrange(desc( Rate_OECD_insititutions_4))


ggplot(eu_mean_oecd4, aes(reorder(Country,Rate_OECD_insititutions_4),
                          Rate_OECD_insititutions_4 )) +
  geom_col(fill= "#00abff")+
  ylim(0,10)+
  coord_flip()


```
# in fact we see that Poland is the most stressed nation but also 
# one of the Country in which the population trust least in their own government
# Denmark, on the other hand, is the nation with the least stress related to Covid,
# in fact it is also one of the nations in which the population trust
# most in their own government



#Trust in Country Measure: it was also asked participants to judge the appropriateness
#of the countries’ measures in response to the COVID-19 on a scale from 0 (too little),
#to 5 (appropriate, the black dashed line in the graph), to 10 (too much).

```{r}
eu_trustinc <- select(EU,Country,Trust_countrymeasure) %>%
  group_by(Country) %>%
  dplyr::summarize(Mean = mean(Trust_countrymeasure, na.rm=TRUE))


ggplot(eu_trustinc, aes(reorder(Country, Mean), Mean)) +
  geom_col(fill= "#00abff")+
  ylim(0,8)+
  geom_hline(yintercept=5)+
  coord_flip()

```


#another important observation concerns the correlation between confidence 
#in the measures taken by governments to tackle covid and the actions 
#that the population has taken to reduce transmission;
#from what emerges Poland not having confidence in the measures taken
#by governments does not even try to reduce transmission,
#while Denmark is in the opposite situation
```{r}

eu_conf_corr <- select(EU,Country,OECD_insititutions_6,Compliance_1:Compliance_6) %>%
                ddply( .(Country), summarize,
         Rate_OECD=mean(OECD_insititutions_6,na.rm=TRUE),
         Rate_Compliance_1=mean(Compliance_1,na.rm=TRUE),
         Rate_Compliance_2=mean(Compliance_2,na.rm=TRUE),
         Rate_Compliance_3=mean(Compliance_3,na.rm=TRUE),
         Rate_Compliance_4=mean(Compliance_4,na.rm=TRUE),
         Rate_Compliance_5=mean(Compliance_5,na.rm=TRUE))%>%
  mutate(total_Compl = rowMeans(select(., -Country,-Rate_OECD)))


mod = lm( total_Compl ~Rate_OECD, data = eu_conf_corr)
summary(mod)
mod$coefficients

ggplot(eu_conf_corr, aes(Rate_OECD,total_Compl)) + 
  geom_point() +
  geom_label_repel(aes(label = Country),
                   size=2,
                   box.padding   = unit(0.5, "lines"), 
                   point.padding = 0.5,
                   min.segment.length = 0,
                   segment.color = 'grey50') +
  theme_classic()+
  geom_abline(intercept = mod$coefficients[1], 
              slope = mod$coefficients[2], 
              color = "red")

```
#In countries with higher trust towards governments ’efforts, citizens were also more 
#likely to report higher levels of compliance with directives aimed at controlling the 
#spread of the virus. Notably, Portugal reported higher levels of compliance than the model 
#would predict based on the acceptance of government efforts. An outlier in the other 
#direction, Latvia reported lower levels of compliance than those the model predicted, 
#given levels of trust.



#will be government’s effort to handle Coronavirus more trusted over the time?
```{r}
eu_trust2m <- select(filter(EU,RecordedDate=="2020-03-30"|
                       RecordedDate=="2020-04-07"|
                       RecordedDate=="2020-04-14"|
                       RecordedDate=="2020-04-21"|
                       RecordedDate=="2020-04-28"|
                       RecordedDate=="2020-05-05"|
                       RecordedDate=="2020-05-12"|
                       RecordedDate=="2020-05-20"|
                       RecordedDate=="2020-05-26"|
                       RecordedDate=="2020-06-5"|
                       RecordedDate=="2020-06-10"),RecordedDate,OECD_insititutions_6) %>%
  ddply( .(RecordedDate), summarize,
         Rate_OECD=mean(OECD_insititutions_6,na.rm=TRUE))

ggplot(eu_trust2m, aes(RecordedDate, Rate_OECD)) + 
  geom_point(size=7) + 
  geom_segment(aes(x=RecordedDate, 
                   xend=RecordedDate, 
                   y=0, 
                   yend=Rate_OECD)) + 
  ylim(0,10)

```
#in the first four weeks the trust for government’s effort to handle Coronavirus 
#appears to increase, to then decrease in the fifth week and remain stable in the following three,
#with the beginning of June and consequently of the summer season the population 
#does not seem to like the countermeasures taken by the governments to handle the pandemic






