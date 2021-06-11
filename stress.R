---
  title: "Covid19-stress-related"
subtitle: "Analyzing stress and psychological effects of Covid-19"
---

```{r}
library(pacman)
p_load(qualtRics, tidyverse,ggrepel,stringr, multicon, psych,ggthemes,
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

#filtering by EU Countries
```{r}
target <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic",
            "Denmark","Estonia","Finland","France","Germany","Greece","Hungary",
            "Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta",
            "Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia",
            "Spain","Sweden")

EU <- filter(cleaned_d, Country %in% target) 

#fixing date visualization
EU$RecordedDate <- as.Date(EU$RecordedDate , format= "%Y-%m-%d")
```
#what is the age of the respondents in eu? note: Missing values are omitted
```{r}
ggplot(subset(EU, !is.na(Dem_age)),aes(Dem_age))+
  geom_bar(fill= "#006978")+
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  labs(x = "Age")+
  xlim(18,99)+
  theme(axis.title.y=element_blank(),axis.title.x = element_text(margin = margin(t=5)),
        axis.text.y = element_text(size=10, hjust = 1),
        plot.title = element_text(margin=margin(0,0,5,0), hjust = 0.5, size=15))+
  ggtitle("Age of the Respondents")
```

#what is the gender of the respondents? note: Missing values are omitted
```{r}

gnd <-count(EU$Dem_gender)
gnd = gnd[-4, ] 

plot_ly(gnd, labels = ~x, values = ~freq, type = 'pie') %>%
  layout(title = "Gender of Respondents",          
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
```

#what type of work do the respondents? note: Missing values are omitted
```{r}
empl <-count(EU$Dem_employment)
empl =empl[-7, ] 

plot_ly(empl, labels = ~x, values = ~freq, type = 'pie') %>%
  layout(title = "Types of Employment of the Respondents",          
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

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
  geom_col(fill= "#006978")+
  ylim(0,5)+
  coord_flip()+
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  labs(x = "Mean of stress levels")+
  theme(axis.title.y=element_blank(),
        axis.text.x = element_text( size = 8), axis.line.y = element_line(size = 0.5),
        axis.text.y = element_text(size=10, hjust = 1),
        plot.title = element_text(margin=margin(0,0,5,0), hjust = 0.5, size=15))+
  ggtitle("Levels of stress by Country")

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
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  labs(y = "Mean of stress levels")+
  theme(axis.title.y = element_text(margin=margin(r=5), hjust = 0.5, size=10),axis.title.x=element_blank(),
        axis.text.x = element_text(angle=65, vjust=0.6, size = 10), axis.line.y = element_line(size = 0.5),
        axis.text.y = element_text(size=10,vjust=0.5,hjust = 1.25),
        plot.title = element_text(margin=margin(0,0,5,0), hjust = 0.5, size=15))+
  ggtitle("Levels of stress by Gender")
```
#stress level based on employment
```{r}
eu_stress_empl <- select(EU, Dem_employment,Scale_PSS10_UCLA_1:Scale_PSS10_UCLA_10) %>%
  ddply( .(Dem_employment), summarize,
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
  mutate(total_stress = rowMeans(select(., -Dem_employment)))

ggplot(data=subset(eu_stress_empl, !is.na(Dem_employment)),
       aes(Dem_employment, total_stress)) + 
  geom_point(size=7) + 
  geom_segment(aes(x=Dem_employment, 
                   xend=Dem_employment, 
                   y=0, 
                   yend=total_stress)) + 
  ylim(0,4)+
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  labs(y = "Mean of stress levels")+
  theme(axis.title.y = element_text(margin=margin(r=5), hjust = 0.5, size=10),axis.title.x=element_blank(),
        axis.text.x = element_text(angle=50, vjust=0.6, size = 10, margin = margin(t=15)), axis.line.y = element_line(size = 0.5),
        axis.text.y = element_text(size=10,vjust=0.5,hjust = 1.25))
#How stress level changed in Europe during the first weeks of Covid pandemic?
```{r}
eu_stress_2months <- select(filter(EU,RecordedDate=="2020-03-30"|
                                     RecordedDate=="2020-04-06"|
                                     RecordedDate=="2020-04-12"|
                                     RecordedDate=="2020-04-18"|
                                     RecordedDate=="2020-04-24"|
                                     RecordedDate=="2020-04-30"|
                                     RecordedDate=="2020-05-05"|
                                     RecordedDate=="2020-05-11"|
                                     RecordedDate=="2020-05-17"|
                                     RecordedDate=="2020-05-23"|
                                     RecordedDate=="2020-05-30"), 
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
  ylim(0,3)+
  scale_x_date(date_breaks = "1 months", date_labels = "%B") +
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  labs(y = "Mean of stress levels")+
  theme(axis.title.y = element_text(margin=margin(r=5), hjust = 0.5, size=10),axis.title.x=element_blank(),
        axis.text.x=element_text(size=10,vjust=0.5,hjust = 1.25),axis.ticks = element_blank(),
        axis.line.y = element_line(size = 0.5),axis.text.y = element_text(size=10,vjust=0.5,hjust = 1.25),
        plot.title = element_text(margin=margin(0,0,5,0), hjust = 0.5, size=10))+
  ggtitle("Levels of stress during the first weeks of the pandemic")

```
#the stress level in the first two weeks was higher than the predictive model, 
#in the third week it decreased a bit and then increased and repeated 
#the cycle until the end of May. Unexpectedly in the end of the May the stress level 
#increased again, it is weird because typically with the start of summer people should
#be less stress; unfortunately we don't have any data that will confirm this trend.

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
  scale_fill_continuous(high = "#132B43", low = "#56B1F7", name="Mean of stress levels")+
  scale_x_continuous(element_blank(), breaks = NULL) +
  scale_y_continuous(element_blank(), breaks = NULL) +
  coord_map(xlim = c(-26, 47),  ylim = c(32.5, 73))+ 
  labs(title = "Levels of stress during the first weeks of the pandemic",
       caption = "Based on PSS Cohen Scale ")+
  theme(plot.title = element_text(face = "bold",size = 15),
        plot.caption = element_text(size = 10, hjust = 2.5))
```

#Sources of Distress among Europeans during the COVID-19 Pandemic
```{r}
eu_stress_source <- select(EU, Expl_Distress_1:Expl_Distress_24)%>%
  na.omit()

eu_stress_source = data.frame(apply(eu_stress_source,2,function(x)mean(x[x<99]))) %>%
  tibble::rownames_to_column(var = "col1") %>%
  `colnames<-`(c("stress_source", "mean")) 

listlab<-(c("No religious activities", "Not knowing how to stop covid-19",
            "Feeling ashamed for acting differently","Adapt work to Digital Platforms",
            "Access to necessities(food etc..)","Behavior of adults I live with",
            "Behavior of childrens I live with","Adapt to social life on digital platforms",
            "No trovels outside my Country","Loneliness","Time i spend in proximity to others",
            "Children's education","Civil services(Police,sanitations..)","Income",
            "Not knowing about developments with Covid-19","Time I spend inside","Work",
            "Job prospect","No social activities","Worry over friends and relatives who live far away",
            "Not knowing how long the measures will last","Risk of catching covid-19",
            "Risk of being hospedalized or dying","National economy"))

ggplot(eu_stress_source, aes(reorder(stress_source,mean), mean)) +
  geom_col(fill= "#006978")+
  ylim(0,6)+
  coord_flip()+
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  ggtitle("Sources of Distress among Europeans during the COVID-19 Pandemic")+
  scale_x_discrete(label  = listlab)+
  labs(y="Mean" )+
  theme(axis.line.y = element_line(size = 0.5), axis.text.x=element_text(size=10),axis.title.y=element_blank(),
        axis.title.x =  element_text(margin = margin(t = 5),hjust = 0.53),
        axis.text.y=element_text(size=8), plot.title = element_text(margin=margin(0,0,5,0), hjust = 0.5,size=10))
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
lablist<-(c("Country's Governments","Country's civil Service","Country's Police",
            "Governments effort against covid-19","WHO","Country's Healthcare system"))

ggplot(eu_trust, aes(reorder(trust,mean), mean)) +
  geom_col(fill= "#006978")+
  ylim(0,10)+
  coord_flip()+
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  ggtitle("Trust in Institutions")+
  scale_x_discrete(label  = lablist)+
  labs(y="Mean" )+
  theme(axis.line.y = element_line(size = 0.5), axis.text.x=element_text(size=10),axis.title.y=element_blank(),
        axis.title.x =  element_text(margin = margin(t = 5)),
        axis.text.y=element_text(size=8), plot.title = element_text(margin=margin(0,0,5,0), hjust = 0.5))
```
## how much citizens trust their own governments?
```{r}
eu_mean_oecd1 <- select(EU,Country,OECD_insititutions_1) %>%
  ddply( .(Country), summarize,
         Rate_OECD_insititutions_1=mean(OECD_insititutions_1,na.rm=TRUE))%>%
  arrange(desc( Rate_OECD_insititutions_1))

ggplot(eu_mean_oecd1, aes(reorder(Country,Rate_OECD_insititutions_1),
                          Rate_OECD_insititutions_1 )) +
  geom_col(fill= "#006978")+
  ylim(0,10)+
  coord_flip()+
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  labs(y="Mean")+
  theme(axis.line.y = element_line(size = 0.5), axis.text.x=element_text(size=10),axis.title.y=element_blank(),
        axis.title.x =  element_text(margin = margin(t = 5)),
        axis.text.y=element_text(size=10))
```
#trust of the countrymeasure
```{r}
eu_mean_oecd4 <- select(EU,Country,OECD_insititutions_4) %>%
  ddply( .(Country), summarize,
         Rate_OECD_insititutions_4=mean(OECD_insititutions_4,na.rm=TRUE))%>%
  arrange(desc( Rate_OECD_insititutions_4))

ggplot(eu_mean_oecd4, aes(reorder(Country,Rate_OECD_insititutions_4),
                          Rate_OECD_insititutions_4 )) +
  geom_col(fill= "#006978")+
  ylim(0,10)+
  coord_flip()+
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  ggtitle("Trust towards governments")+
  labs(y="Mean")+
  theme(axis.line.y = element_line(size = 0.5), axis.text.x=element_text(size=10),axis.title.y=element_blank(),
        axis.title.x =  element_text(margin = margin(t = 5)),
        axis.text.y=element_text(size=10), plot.title = element_text(margin=margin(0,0,5,0), hjust = 0.5))
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
  geom_col(fill="#006978")+
  ylim(0,8)+
  geom_hline(yintercept=5)+
  coord_flip()+
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  ggtitle("Appropriateness of the countries’ measures")+
  labs(y="Country", x="Mean")+
  theme(axis.line.y = element_line(size = 0.5), axis.text.x=element_text(size=10),
        axis.title.y =  element_text(margin = margin(r = 5)),
        axis.title.x =  element_text(margin = margin(t = 5)),
        axis.text.y=element_text(size=10), plot.title = element_text(margin=margin(0,0,5,0), hjust = 0.5))

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
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  ggtitle("Trust towards the governments’handling of the pandemic")+
  labs(y="Mean of Compliances", x="Mean of trust towards governemts efforts")+
  theme(axis.line.y = element_line(size = 0.5), axis.text.x=element_text(size=10),
        axis.title.y =  element_text(margin = margin(r = 5)),
        axis.title.x =  element_text(margin = margin(t = 5)),
        axis.text.y=element_text(size=10), plot.title = element_text(hjust = 0.5))+
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
                              RecordedDate=="2020-04-06"|
                              RecordedDate=="2020-04-12"|
                              RecordedDate=="2020-04-18"|
                              RecordedDate=="2020-04-24"|
                              RecordedDate=="2020-04-30"|
                              RecordedDate=="2020-05-05"|
                              RecordedDate=="2020-05-11"|
                              RecordedDate=="2020-05-17"|
                              RecordedDate=="2020-05-23"|
                              RecordedDate=="2020-05-30"),RecordedDate,OECD_insititutions_6) %>%
  ddply( .(RecordedDate), summarize,
         Rate_OECD=mean(OECD_insititutions_6,na.rm=TRUE))

ggplot(eu_trust2m, aes(RecordedDate, Rate_OECD)) + 
  geom_point(size=7) + 
  geom_segment(aes(x=RecordedDate, 
                   xend=RecordedDate, 
                   y=0, 
                   yend=Rate_OECD)) + 
  ylim(0,10)+
  scale_x_date(date_breaks = "1 months", date_labels = "%B") +
  theme_economist(dkpanel=TRUE) +
  scale_colour_economist()+
  labs(y = "Mean of trust towards governemts efforts")+
  theme(axis.title.y = element_text(size = 10),axis.title.x=element_blank(),
        axis.text.x=element_text(size=10,vjust=0.5,hjust = 1.25),axis.ticks = element_blank(),
        axis.line.y = element_line(size = 0.5),axis.text.y = element_text(size=10,vjust=0.5,hjust = 1.25),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Trust towards the governments’handling of the pandemic")

```
#in the first four weeks the trust for government’s effort to handle Coronavirus 
#appears to increase, to then decrease in the fifth week and remain stable in the following three,
#with the beginning of June and consequently of the summer season the population 
#does not seem to like the countermeasures taken by the governments to handle the pandemic

#####OBSOLETE, CHECK STRESS.RMD########
