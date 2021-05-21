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
p_load(qualtRics, tidyverse, stringr, multicon, psych,
       dplyr,plyr,plotly,sf,rworldmap,RColorBrewer,data.table)
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
         filter(!is.na(Dem_employment)), aes(x =Dem_employment))+
  stat_count()
```

#analyzing stress level in europe

#PSS scores are obtained by reversing responses (e.g., 0 = 4, 1 = 3, 2 = 2, 3 = 1 & 4 = 0) to the four positively stated items (items 4, 5, 7, & 8) and then summing across all scale items. A short 4 item scale can be made from questions 2, 4, 5 and 10 of the PSS 10 item scale.
#note that the na values are obmitted
```{r}
eu_stress <- select(EU,Country,Scale_PSS10_UCLA_1:Scale_PSS10_UCLA_10) %>%
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

ggplot(eu_stress, aes(x=total_stress, y=Country)) +
  geom_col(fill= "#00abff")+
  xlim(0,5)
```
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
  scale_fill_distiller(name = "Mean", palette = "Greens") +
  scale_x_continuous(element_blank(), breaks = NULL) +
  scale_y_continuous(element_blank(), breaks = NULL) +
  coord_map(xlim = c(-26, 47),  ylim = c(32.5, 73)) 

```{r}
#Sources of Distress among Europeans during the COVID-19 Pandemic

eu_stress_source <- select(EU, Expl_Distress_1:Expl_Distress_24)%>%
                    na.omit()

eu_stress_source = data.frame(apply(eu_stress_source,2,function(x)mean(x[x<99]))) %>%
  tibble::rownames_to_column(var = "col1") %>%
  `colnames<-`(c("stress_source", "mean")) 
 

ggplot(eu_stress_source, aes(stress_source, mean)) +
  geom_col(fill= "#00abff")+
  ylim(0,6)+
  coord_flip()

```





  


   
