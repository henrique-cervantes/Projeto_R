# IMPORTAÇÃO DAS BIBLIOTECAS
library(readxl)
library(ggplot2)
library(tidyverse)
library(infer)
library(data.table)
library(stargazer)
library(stringr)


#### HATE CRIME ####

# IMPORTAÇÃO DATASET HATE_CRIME
hate_crime_dataset <- read.csv('hate_crime.csv') %>%
  select(-c('INCIDENT_ID', 'ORI', 'PUB_AGENCY_NAME',
            'PUB_AGENCY_UNIT', 'STATE_ABBR',
            'DIVISION_NAME', 'POPULATION_GROUP_CODE',
            'POPULATION_GROUP_DESC')) %>%
  filter(DATA_YEAR <= 2016) 
names(hate_crime_dataset)[names(hate_crime_dataset) == "DATA_YEAR"] <- "YEAR"
names(hate_crime_dataset)[names(hate_crime_dataset) == "STATE_NAME"] <- "STATE"


#### ANÁLISE INICIAL DOS DADOS HATE_CRIME ####

# Crimes por ANO 
total_crime_per_year <- hate_crime_dataset %>%
  count(YEAR)
# Ordernado decrescente:
total_crime_per_year[order(-total_crime_per_year$n),]



# Crimes por ESTADO 
total_crime_per_state <- hate_crime_dataset %>%
  count(STATE)
total_crime_per_state <- total_crime_per_state[-c(12),]
# Ordernado decrescente:
total_crime_per_state[order(-total_crime_per_state$n),]


# Crimes por ESTADO por ANO
crime_per_state_per_year <- hate_crime_dataset %>%
  group_by(YEAR) %>%
  count(STATE)
crime_per_state_per_year

# TRENDLINE CRIME por ESTADO por ANO
ggplot(crime_per_state_per_year, aes(x = YEAR, y = n)) +
  geom_line() +
  facet_wrap(~ STATE)



# Total de CRIMES por TIPO
total_crime_per_type <- hate_crime_dataset %>%
  count(OFFENSE_NAME)
total_crime_per_type[order(-total_crime_per_type$n),]


# Total de CRIMES por ETNIA por ESTADO por ANO
total_crime_per_etnia <- hate_crime_dataset %>%
  group_by(YEAR, STATE) %>%
  count(OFFENDER_RACE)
total_crime_per_etnia

# offender race (tá certo):
unique(total_crime_per_etnia$OFFENDER_RACE)


# Total de CRIMES por VÍTIMA por ESTADO por ANO
total_crime_per_victim <- hate_crime_dataset %>%
  group_by(YEAR, STATE) %>%
  count(BIAS_DESC)
total_crime_per_victim
sort(unique(total_crime_per_victim$BIAS_DESC))


# sem white
total_crime_per_victim_corrigido <- subset(total_crime_per_victim, BIAS_DESC != 'Anti-White')



######################## CONSTRUINDO A DISTRIBUIÇÃO POR VÍTIMA ##################################3

# ANTI_FEMALE
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_FEMALE = ifelse(str_detect(BIAS_DESC, "Anti-Female"), 1, 0))

# ANTI_MALE (n faz sentido)
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_MALE = ifelse(str_detect(BIAS_DESC, "Anti-Male"), 1, 0))



########################################## ETNIAS #########################################3

# ANTI_WHITE (n faz sentido)
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_WHITE = ifelse(str_detect(BIAS_DESC, "Anti-White"), 1, 0))


# ANTI-BLACK
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_BLACK = ifelse(str_detect(BIAS_DESC, 'Anti-Black or African American'), 1, 0))

# ANTI-ASIAN
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_ASIAN = ifelse(str_detect(BIAS_DESC, 'Anti-Asian'), 1, 0))

# ANTI-JEWISH 
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_JEWISH = ifelse(str_detect(BIAS_DESC, 'Anti-Jewish'), 1, 0))

# ANTI-ARAB
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_ARAB = ifelse(str_detect(BIAS_DESC, "Anti-Arab"), 1, 0))

# ANTI-HISPANIC
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_HISPANIC = ifelse(str_detect(BIAS_DESC, "Anti-Hispanic or Latino"), 1, 0))

# ANTI-NATIVE
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_NATIVE = ifelse(str_detect(BIAS_DESC, "Anti-American Indian or Alaska Native"), 1, 0))

# ANTI-HAWAIIAN
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_HINDU = ifelse(str_detect(BIAS_DESC, "Anti-Native Hawaiian or Other Pacific Islander"), 1, 0))

# ANTI_HINDU
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_HINDU = ifelse(str_detect(BIAS_DESC, "Anti-Hindu"), 1, 0))

# ANTI-OTHER-RACE
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_OTHER_RACE = ifelse(str_detect(BIAS_DESC, "Anti-Other Race/Ethnicity/Ancestry"), 1, 0))

# ANTI_MULTIPLE_RACES
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_MULT_RACE = ifelse(str_detect(BIAS_DESC, "Anti-Multiple Races, Group"), 1, 0))



######################################## RELIGIÕES ########################################

# ANTI-ISLAMIC
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_ISLAMIC = ifelse(str_detect(BIAS_DESC, "Anti-Islamic (Muslim)"), 1, 0))

mean(hate_crime_dataset$ANTI_ISLAMIC)

# ANTI-SIKH
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_SIKH = ifelse(str_detect(BIAS_DESC, "Anti-Sikh"), 1, 0))

# ANTI-BUDDHIST
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_BUDDHIST = ifelse(str_detect(BIAS_DESC, "Anti-Buddhist"), 1, 0))

# ANTI-JEHOVAH'S WITNESS
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_JEHOVAHS_WITNESS = ifelse(str_detect(BIAS_DESC, "Anti-Jehovah's Witness"), 1, 0))

# ANTI-CATHOLIC
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_CATHOLIC = ifelse(str_detect(BIAS_DESC, "Anti-Catholic"), 1, 0))

# ANTI-PROTESTANT
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_PROTESTANT = ifelse(str_detect(BIAS_DESC, "Anti-Protestant"), 1, 0))

# ANTI_MORMON
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_MORMON = ifelse(str_detect(BIAS_DESC, "Anti-Mormon"), 1, 0))

# ANTI_ORTHODOX
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_ORTHODOX = ifelse(str_detect(BIAS_DESC, "Anti-Eastern Orthodox (Russian, Greek, Other)"), 1, 0))

# ANTI-ATHEISM
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_ATHEISM = ifelse(str_detect(BIAS_DESC, "Anti-Atheism/Agnosticism"), 1, 0))

# ANTI_OTHER_CHRISTIAN
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_OTHER_CHRISTIAN = ifelse(str_detect(BIAS_DESC, "Anti-Other Christian"), 1, 0))

# ANTI_OTHER_RELIGION
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_OTHER_RELIGION = ifelse(str_detect(BIAS_DESC, "Anti-Other Religion"), 1, 0))

# ANTI_MULTIPLE_RELIGIONS
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_MULT_RELIGIONS = ifelse(str_detect(BIAS_DESC, "Anti-Multiple Religions, Group"), 1, 0))



############################################## LGBT+ ##############################

# ANTI_HETEROSEXUAL (n faz sentido)
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_HETEROSEXUAL = ifelse(str_detect(BIAS_DESC, "Anti-Heterosexual"), 1, 0))

# ANTI_GAY.1
#hate_crime_dataset <- hate_crime_dataset %>% 
#  mutate(ANTI_GAY = ifelse(str_detect(BIAS_DESC, "Anti-Gay (Male)"), 1, 0))

# ANTI_GAY.2
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_GAY = ifelse(str_detect(BIAS_DESC, "Gay"), 1, 0))

# ANTI_LESBIAN.1
#hate_crime_dataset <- hate_crime_dataset %>% 
#  mutate(ANTI_LESBIAN = ifelse(str_detect(BIAS_DESC, "Anti-Lesbian (Female)"), 1, 0))

# ANTI_LESBIAN.2
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_LESBIAN = ifelse(str_detect(BIAS_DESC, "Lesbian"), 1, 0))

# ANTI_BISEXUAL.1
#hate_crime_dataset <- hate_crime_dataset %>% 
#  mutate(ANTI_BISEXUAL = ifelse(str_detect(BIAS_DESC, "Anti-Bisexual"), 1, 0))

# ANTI_BISEXUAL.2
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_BISEXUAL = ifelse(str_detect(BIAS_DESC, "Bisexual"), 1, 0))

# ANTI_TRANSGENDER.1
#hate_crime_dataset <- hate_crime_dataset %>% 
#  mutate(ANTI_TRANSGENDER = ifelse(str_detect(BIAS_DESC, "Anti-Transgender"), 1, 0))

# ANTI_TRANSGENDER.2
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_TRANSGENDER = ifelse(str_detect(BIAS_DESC, "Transgender"), 1, 0))


# ANTI_GENDER_NON_CONFORMING
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_GENDER_NON_CONF = ifelse(str_detect(BIAS_DESC, "Anti-Gender Non-Conforming"), 1, 0))


# ANTI-LGBT+ GENÉRICO
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_OTHER_LGBT = ifelse(str_detect(BIAS_DESC, "Anti-Lesbian, Gay, Bisexual, or Transgender (Mixed Group)"), 1, 0))



####################################### CAPACITISMO ######################################

# ANTI_MENTAL_DISABILITY
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_MENTAL_DIS = ifelse(str_detect(BIAS_DESC, "Anti-Mental Disability"), 1, 0))

# ANTI_PHYSICAL_DISABILITY
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_PHYSICAL_DIS = ifelse(str_detect(BIAS_DESC, "Anti-Physical Disability"), 1, 0))

############################ FIM DAS CORREÇÕES DE VICTIM BIAS ################ 

sum(hate_crime_dataset$ANTI_BLACK)


ggplot(hate_crime_dataset, aes(x = YEAR, y = ANTI_BLACK)) +
    geom_line() +
    facet_wrap(~ STATE)











########################### GUNS DATASET ###############################

# Importação dos dados
guns_dataset <- read_excel('TL-354-State-Level Estimates of Household Firearm Ownership.xlsx', sheet = 2) %>%
  filter(Year >= 1991) %>%
  select(-c("FIP"))
names(guns_dataset)[names(guns_dataset) == "Year"] <- "YEAR"


##### ANÁLISE INICIAL DOS DADOS DE ARMAS #####

# HFR (proporção de armas por pessoa por estado) por ANO 
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
guns_per_state; guns_per_state[order(-guns_per_state$mean_HFR_per_state),]


# HFR POR ANO POR ESTADO
guns_per_state_per_year <- guns_dataset %>%
  select(c("YEAR", "STATE", "HFR"))
names(guns_per_state_per_year) <- c("YEAR", "STATE", "Prop_guns_p_y_s")

# Gráfico de HFR por ANO por ESTADO
ggplot(guns_dataset, aes(x = YEAR, y = HFR)) +
  geom_line() +
  facet_wrap(~ STATE)



######## DADOS AGREGADOS CRIME feat.GUNS ########
data_crime_guns <- merge(hate_crime_dataset, guns_dataset)

data_crime_guns_2 <- merge(guns_dataset, hate_crime_dataset)



######## DATASET PREFERÊNCIAS POLÍTICAS ######## PARA FAZER!!!
data_pol_pref_raw <- read_xlsx('Partisan_Balance_For_Use2011_06_09b.xlsx')
names(data_pol_pref_raw)[names(data_pol_pref_raw) == "year"] <- "YEAR"
names(data_pol_pref_raw)[names(data_pol_pref_raw) == "state"] <- "STATE"
data_pol_pref <- data_pol_pref_raw %>%
  filter(YEAR >= 1980)



########################### DATASET POPULAÇÃO ###############################3
# IMPORTAÇÃO DOS DADOS
data_pop <- read_csv("total_pop.csv") %>%
  select(-c("X1")) 
setDT(data_pop)
data_pop_arranged <- melt(data_pop)
names(data_pop_arranged) <- c("STATE", "YEAR", "POPULATION")
data_pop_arranged <- as.data.frame(data_pop_arranged[-c(1:50, 1351:1500),])

# Adicionando uma coluna com a proporção de crimes na populaçao por estado ao longo do tempo:
prop_crime_per_state_per_year <- merge(crime_per_state_per_year, data_pop_arranged) %>%
  mutate(PROP_CRIME = n / POPULATION)


#### Construindo o DataFrame com freq e prop de armas e crimes ####
df_intermediario <- merge(prop_crime_per_state_per_year, guns_per_state_per_year)
df_reg <- merge(df_intermediario, guns_per_state)
names(df_reg) <- c("STATE", "YEAR", "CRIME_PER_STATE_PER_YEAR", "POPULATION",
                     "PROP_CRIME", "PROP_GUNS_PER_STATE_PER_YEAR",
                     "MEAN_HFR_PER_STATE")
df_reg$NUMBER_GUNS <- round(df_reg$POPULATION * df_reg$PROP_GUNS_PER_STATE_PER_YEAR, 0)


# Gráfico com as proporções
ggplot(df_reg, aes(x = PROP_GUNS_PER_STATE_PER_YEAR, y = PROP_CRIME)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Prop Guns vs Crimes", subtitle = "Year and State", 
       x = "Prop guns", y = "Prop crimes")

# Gráfico com os totais
ggplot(df_reg, aes(x = CRIME_PER_STATE_PER_YEAR, y = NUMBER_GUNS)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Guns vs Crimes", subtitle = "Year and State", x = "Number of guns", y = "Number of crimes")

# Reg 1.a: total crimes ~ total armas
lm1a <- lm(CRIME_PER_STATE_PER_YEAR ~ NUMBER_GUNS, data = df_reg)
summary(lm1a)


# Reg 1.b: prop crimes ~ prop armas
lm1b <- lm(PROP_CRIME ~ PROP_GUNS_PER_STATE_PER_YEAR + factor(STATE), data = df_reg)
summary(lm1b)


# Reg 2: total crimes ~ total armas + população
lm2 <- lm(CRIME_PER_STATE_PER_YEAR ~ NUMBER_GUNS + POPULATION, data = df_reg)
summary(lm2)


# Reg 3: crime ~ armas + estados 
lm3 <- lm(CRIME_PER_STATE_PER_YEAR ~ NUMBER_GUNS + factor(STATE), data = df_reg)
summary(lm3)


# STARGAZER para as regressões
stargazer(list(lm1a,lm1b, lm2,lm3),
          keep.stat = c("n","rsq"),
          float = FALSE, font.size = "small", digits=2,
          type = "text")


# Gráfico número de crimes vs armas por ESTADO
ggplot(df_reg, aes(x = CRIME_PER_STATE_PER_YEAR, y = NUMBER_GUNS)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  facet_wrap(~ STATE) +
  labs(title = "Crimes vs Guns", subtitle = "States", x = "Number of guns", y = "Number of crimes")

# Gráfico número de crimes vs armas por ANO
ggplot(df_reg, aes(x = CRIME_PER_STATE_PER_YEAR, y = NUMBER_GUNS)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  facet_wrap(~ YEAR) +
  labs(title = "Guns vs Crimes", subtitle = "Year and State", x = "Number of guns", y = "Number of crimes")


# REG 4: HFR ~ UNIVERSL -> como universl background checks impacta HFR 
lm4 <- lm(HFR ~ universl + factor(YEAR) + STATE, guns_dataset)
summary(lm4)

# REG 5 -> HFR ~ PERMIT -> como o permit impacta HFR 
lm5 <- lm(HFR ~ permit + factor(YEAR) + STATE, guns_dataset)
summary(lm5)






############### DATASET PREFS POLÍTICAS #############
# IMPORTAÇÃO DOS DADOS E FILTRAGEM INICIAL
elections_data <- get(load("1976-2020-president.RData")) %>%
  filter(year >= 1980, party_simplified != "OTHER")
         
         

# Total de votos por partido por estado por ano:
president <- elections_data %>%
  select(year, state, party_simplified, candidatevotes) %>%
  group_by(year, state)
president


length(president$candidatevotes)

# for loop pra achar os candidatos mais votados por estado por ano:

republican_votes <- president %>%
  filter(party_simplified == "REPUBLICAN")
republican_votes

democrat_votes <- president %>%
  filter(party_simplified == "DEMOCRAT")
democrat_votes

i <- 1
most_voted_party <- as.double(c())
votes_winner <- as.double(c())
votes_loser <- as.double(c())
while (i <= 562) {
  if (republican_votes[i, 4] > democrat_votes[i, 4]) {
    most_voted_party <- c(most_voted_party, "REPUBLICAN")
    votes_winner <- as.double(c(votes_winner, republican_votes[i, 4]))
    votes_loser <- as.double(c(votes_loser, democrat_votes[i, 4]))
    i = i + 1
  }
  if (democrat_votes[i, 4] > republican_votes[i, 4]) {
    most_voted_party <-c(most_voted_party, "DEMOCRAT")
    votes_winner <- as.double(c(votes_winner, democrat_votes[i,4]))
    votes_loser <- as.double(c(votes_loser, republican_votes[i, 4]))
    i = i + 1
  } 
}

most_voted_party <- as.data.frame(most_voted_party)
votes_winner <- as.data.frame(votes_winner)
votes_loser <- as.data.frame(votes_loser)


year <- sort(c(rep(seq(1980, 2020, 4), 51)))
year <- c(year, NaN)

state <- rep(unique(president$state), 11)

state <- c(state, NaN)


# df com ganhadores e numero de votos por ano por estado 
df_elections <- data.frame(YEAR = year,
                           STATE = state,
                           MOST_VOTED_PARTY = most_voted_party,
                           WINNER_VOTES = votes_winner,
                           LOSER_VOTES = votes_loser)


# tem um dado a mais (mas parece estar certo (?))









# daqui pra baixo é só caos e tristeza

votes_winner <- data.frame(WINNER_VOTES = votes_winner)


president_2 <- president[c(seq(1, length(president$year), 3)),] %>%
  select("year", 'state')

year <- rep(seq(1980, 2020, 4), 51)
state <- unique(president$state)

df_elections <- data.frame(YEAR = president_2$year,
                           STATE = president_2$state,
                           MOST_VOTED_PARTY = most_voted_party,
                           WINNER_VOTES = votes_winner,
                           LOSER_VOTES = votes_loser)


president <- merge(president, most_voted_party)
president


i <- 1
lista <- c() 
while (i <= 562) {
  if (republican_votes[i,4] > democrat_votes[i,4] & republican_votes[i,4] > libertarian_votes[i,4]) {
    lista <- c(lista, "REPUBLICAN")
    i = i + 1
  }
  if (libertarian_votes[i,4] > republican_votes[i,4] & libertarian_votes[i,4] > democrat_votes[i,4]){
    lista <- c(lista, "LIBERTARIAN")
    i = i + 1
  }
  if (democrat_votes[i,4] > republican_votes[i,4] & democrat_votes[i,4] > libertarian_votes[i,4]) {
    lista <- c(lista, "DEMOCRAT")
    i = i + 1
  } 
}




setDT(president)
elections_data_arranged <- melt(president)

elections_data_arranged


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




hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_BLACK = ifelse(BIAS_DESC == 'Anti-Black or African American', 1, 0))

mean(hate_crime_dataset$ANTI_BLACK)

hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_BLACK = ifelse(grep('Anti-Black or African American', BIAS_DESC), 1, 0)))





hate_crime_dataset$ANTI_BLACK <- ifelse(grep('Anti-Black or African American', hate_crime_dataset$BIAS_DESC), 1, 0)

a <- ifelse(grep('Anti-Black or African American', hate_crime_dataset$BIAS_DESC), 2, 1)



hate_crime_dataset

# black/african american
anti_black <- data.frame()

# anti
anti_arab <- c()
anti_asian <-c()
anti_jewish <- c()

anti_islamic <- c()
anti_asianreligion <- c()
anti_christian <- c()

anti_female <- c()
anti_lgbt <- c()

anti_disability <- c()
anti_

n = 1
if (hate_crime_dataset[n, "BIAS_DESC"] == grepl("Anti_BLack")) {
  anti_black <- c(anti_black, hate_crime_dataset[n,])
  n = n + 1
}

anti_black <- subset(hate_crime_dataset, BIAS_DESC == "Anti-Black")
anti_black

anti_black <- hate_crime_dataset[hate_crime_dataset$BIAS_DESC == "Anti-Black",]


anti_black <- substring(hate_crime_dataset$BIAS_DESC, "Anti-White")


hate_crime_dataset %>%
  select(BIAS_DESC, contains("Anti-Black"))

anti_black <- hate_crime_dataset %>%
  filter(BIAS_DESC == "Anti-Black or African American")



anti_arab <- hate_crime_dataset %>%
  filter(c(BIAS_DESC == "Anti-Black or African American", 
  ))


### DADOS QUE FAZEM SENTIDO ###
hate_crime <- hate_crime_dataset %>%
  filter(BIAS_DESC != "Anti-White") # talvez seja bom continuar daqui


