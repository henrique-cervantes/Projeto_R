# IMPORTAÇÃO DAS BIBLIOTECAS
library(readxl)
library(ggplot2)
library(tidyverse)
library(infer)
library(data.table)

#### HATE CRIME ####

# IMPORTAÇÃO DATASET HATE_CRIME
hate_crime_dataset_raw <- read.csv('hate_crime.csv')
hate_crime_dataset <- hate_crime_dataset_raw %>%
  select(-c('INCIDENT_ID', 'ORI', 'PUB_AGENCY_NAME',
            'PUB_AGENCY_UNIT', 'STATE_ABBR',
            'DIVISION_NAME', 'POPULATION_GROUP_CODE',
            'POPULATION_GROUP_DESC')) %>%
  filter(DATA_YEAR <= 2016) 
names(hate_crime_dataset)[names(hate_crime_dataset) == "DATA_YEAR"] <- "YEAR"
names(hate_crime_dataset)[names(hate_crime_dataset) == "STATE_NAME"] <- "STATE"


#### ANÁLISE PRELIMINAR DOS DADOS HATE_CRIME ####

# Total de crimes por ANO (sorted)
total_crime_per_year <- hate_crime_dataset %>%
  count(YEAR)
#total_crime_per_year
total_crime_per_year[order(-total_crime_per_year$n),]



# Total de crimes por ESTADO (sorted)
total_crime_per_state <- hate_crime_dataset %>%
  count(STATE)
total_crime_per_state <- total_crime_per_state[-c(12),]
total_crime_per_state
total_crime_per_state[order(-total_crime_per_state$n),]


# Crimes por ESTADO por ANO
crime_per_state_per_year <- hate_crime_dataset %>%
  group_by(YEAR) %>%
  count(STATE)
crime_per_state_per_year

# Geom_line CRIME por ESTADO por ANO
ggplot(crime_per_state_per_year, aes(x = YEAR, y = n)) +
  geom_line() +
  facet_wrap(~ STATE)



# Total de CRIMES por TIPO
total_crime_per_type <- hate_crime_dataset %>%
  #  group_by(BIAS_DESC) %>%
  count(BIAS_DESC)
total_crime_per_type




#### DATASET GUNS ####

# IMPORTAÇÃO 
guns_dataset_raw <- read_excel('TL-354-State-Level Estimates of Household Firearm Ownership.xlsx', sheet = 2)
guns_dataset <- guns_dataset_raw %>%
  filter(Year >= 1991) %>%
  select(-c("FIP"))
names(guns_dataset)[names(guns_dataset) == "Year"] <- "YEAR"


# ANÁLISE PRELIMINAR DOS DADOS DE ARMAS

# HFR por ANO
guns_per_year <- guns_dataset %>%
  select(c(YEAR, HFR)) %>%
  group_by(YEAR) %>%
  mutate(mean_HFR_per_year = mean(HFR))
guns_per_year <- guns_per_year[1:26, c(1, 3)]
guns_per_year
guns_per_year[order(-guns_per_year$mean_HFR_per_year),]


# Gráfico HFR por ANO
ggplot(guns_per_year, aes(x = YEAR, y = mean_HFR_per_year)) +
  geom_line()


# HFR por ESTADO 
guns_per_state <- guns_dataset %>%
  select(c(YEAR, STATE, HFR)) %>%
  group_by(STATE) %>%
  mutate(mean_HFR_per_state = mean(HFR))
guns_per_state <- guns_per_state[c(1:1300 %% 26 == 0), c(2, 4)]
guns_per_state
guns_per_state[order(-guns_per_state$mean_HFR_per_state),]


# Gráfico de HFR por ANO por ESTADO
ggplot(guns_dataset, aes(x = YEAR, y = HFR)) +
  geom_line() +
  facet_wrap(~ STATE)


#### REGRESSÕES ####


# REGRESSÃO ANOS MAIS CRIMES ~ ANOS COM MAIS ARMAS 
df_reg_1 <- data.frame(Year = c(1991:2016),
                       Crime_per_year = total_crime_per_year$n,
                       Mean_HFR_per_year = guns_per_year$mean_HFR_per_year)



lm(Crime_per_year ~ Mean_HFR_per_year, data = df_reg_1)


ggplot(df_reg_1, aes(x = Mean_HFR_per_year, y = Crime_per_year)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")



# REGRESSÃO ESTADOS CRIMES ~ ESTADOS ARMAS
df_reg_2 <- data.frame(STATES = total_crime_per_state$STATE,
                       Crime_per_state = total_crime_per_state$n,
                       Mean_HFR_per_state = guns_per_state$mean_HFR_per_state)

ggplot(df_reg_2, aes(x = Mean_HFR_per_state, y = Crime_per_state)) +
  geom_point() + 
  stat_smooth(method = "lm", col = "red")

lm(Crime_per_state ~ Mean_HFR_per_state, df_reg_2)



### DESCRIÇÃO DOS DADOS DE GUNS ###


# Direct measures of household firearm ownership #
# BRFSS 2001, 2002, 2004
#    1980, 1983, 1985, 1986, 1988, 1989, 1990, 1991, 1993, 1996, 1997, 1999, 2000, 2012
# GSS 1980, 1982, 1984, 1985, 1987, 1988, 1989, 1990, 1991, 1993, 1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016
# PEW 1997, 2000, 2003, 2004, 2007, 2009, 2010, 2011, 2012, 2013, 2015, 2016


# Universl !Universal background checks state indicator
# Permit !Permit to purchase state indicator
# Fem_FS_S !Female firearm suicides/total suicides
# Male_FS_S !Male firearm suicides/total suicides
# BRFSS GALLUP GSS PEW !BRFSS, Gallup, GSS, and Pew survey estimates
# HuntLic !Square root of state resident hunting license rate
# GunsAmmo !Square root of Guns & Ammo subscription rate and standardized within year
# BackChk !Background checks rate standardized within year
# PewQChng !Binary indicator for Pew surveys that changed ownership question
# BS1 !First blended linear spline-represents roughly 1980-1992
# BS2 !Second blended linear spline-represents roughly 1993-2004
# BS3 !Third blended linear spline-represents roughly 1993-2004

###########################################################################



## DATASET PREFERÊNCIAS POLÍTICAS ##
data_pol_pref_raw <- read_xlsx('Partisan_Balance_For_Use2011_06_09b.xlsx')
names(data_pol_pref_raw)[names(data_pol_pref_raw) == "year"] <- "YEAR"
names(data_pol_pref_raw)[names(data_pol_pref_raw) == "state"] <- "STATE"
data_pol_pref <- data_pol_pref_raw %>%
  filter(YEAR >= 1980)



########## DATASET POPULAÇÃO ############
data_pop <- read_csv("total_pop.csv") 
data_pop <- data_pop %>%
  select(-c("X1")) 


setDT(data_pop)
data_pop_arranged <- melt(data_pop)
names(data_pop_arranged) <- c("STATE", "YEAR", "POPULATION")
data_pop_arranged <- as.data.frame(data_pop_arranged)
data_pop_arranged <- data_pop_arranged[-c(1:50, 1351:1500),]


prop_crime_per_state_per_year <- merge(crime_per_state_per_year, data_pop_arranged) %>%
  mutate(PROP_CRIME = n / POPULATION)









##### TRATAMENTO DADOS DA POP DE 2000s #####
#data_pop_raw <- read.csv("county_population.csv")

#data_pop_1990s <- read_csv("1990s_pop.csv")
#data_pop_2000s <- read_csv("2000s_pop.csv")
#data_pop_2010s <- read.csv("2010s_pop.csv")

#data_pop_1990_2000 <- merge(data_pop_1990s, data_pop_2000s)

#data_pop_all <- merge(data_pop_1990_2000, data_pop_2010s)

#data_pop_all <- data_pop_all[,-c(22)]

#colnames(data_pop_all) <- c("STATE", 1990, 1991, 1992, 1993, 1994,
#                            1995, 1996, 1997, 1998, 1999, 2000, 
#                            2001, 2002, 2003, 2004, 2005, 2006, 
#                            2007, 2008, 2009, 2010, 2011, 2012,
#                            2013, 2014, 2015, 2016, 2017, 2018, 2019)
#write.csv(data_pop_all, "total_pop.csv")



#range(1:10)


#help(range)

#data_guns_crime <- merge(guns_dataset, hate_crime_dataset)

#data_guns_crime_2 <- merge(hate_crime_dataset, guns_dataset)


#data_prop_2000s <- read_xls("2000_2010.xls")
#colnames(data_prop_2000s) <- c("STATE", "Census","2000", "2001",
#                               "2002", "2003", "2004", "2005",
#                               "2006", "2007", "2008", "2009", "2010")
#data_prop_2000s <- data_prop_2000s %>%
#  select(-c("Census", "2010"))

#data_prop_2000s <- data_prop_2000s[-c(1, 2, 14, 54:63),]
#data_prop_2000s$STATE <- c(total_crime_per_state$STATE)

#write.csv(data_prop_2010s, "2010s_pop.csv")

##### TRATAMENTO DADOS DA POP DE 2010s ######
#data_prop_2010s <- read_xlsx("2010_2020.xlsx")
#colnames(data_prop_2010s) <- c("STATE", "Census", "Estimates Base", "2010", "2011",
                         #      "2012", "2013", "2014", "2015",
                       #        "2016", "2017", "2018", "2019")
#data_prop_2010s <- data_prop_2010s %>%
#  select(-c("Census", "Estimates Base"))

#data_prop_2010s <- data_prop_2010s[-c(1:8, 20, 60:66),]
#data_prop_2010s$STATE <- c(total_crime_per_state$STATE)