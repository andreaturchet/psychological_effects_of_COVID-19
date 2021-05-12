
---
  title: "Covid19-stress-related"
subtitle: Analyzing stress and psychological effects of Covid-19
output:
  html_notebook:
  toc: yes
toc_float: yes
html_document:
  df_print: paged
toc: yes
---
  
 ```{r, include = FALSE}
library(pacman)
p_load(qualtRics, tidyverse, stringr, multicon, psych,
       dplyr)
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
# How many people answer the survey in IT?
```{r}
d %>% 
  filter(UserLanguage == "IT") %>% 
  count(Country) %>% 
  arrange(desc(n))
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




