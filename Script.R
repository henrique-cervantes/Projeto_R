# IMPORTAÇÃO DAS BIBLIOTECAS
library(readxl)
library(ggplot2)
library(tidyverse)
library(infer)
library(data.table)
library(stargazer)


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

# Total de CRIMES por VÍTIMA por ESTADO por ANO
total_crime_per_victim <- hate_crime_dataset %>%
  group_by(YEAR, STATE) %>%
  count(BIAS_DESC)
total_crime_per_victim_corrigido <- subset(total_crime_per_victim, BIAS_DESC != 'Anti-White')


### DADOS QUE FAZEM SENTIDO ###
hate_crime <- hate_crime_dataset %>%
  filter(BIAS_DESC != "Anti-White") # talvez seja bom continuar daqui




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
         
         

# Partidos que ganharam por ano por estado:
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
most_voted_party <- c() 
votes_winner <- c()
votes_loser <- c()
while (i <= 562) {
  if (republican_votes[i, 4] > democrat_votes[i, 4]) {
    most_voted_party <- c(most_voted_party, "REPUBLICAN")
    votes_winner <- c(votes_winner, republican_votes[i, 4])
    votes_loser <- c(votes_loser, democrat_votes[i, 4])
    i = i + 1
  }
  if (democrat_votes[i, 4] > republican_votes[i, 4]) {
    most_voted_party <- c(most_voted_party, "DEMOCRAT")
    votes_winner <- c(votes_loser, democrat_votes[i,4])
    votes_loser <- c(votes_loser, republican_votes[i, 4])
    i = i + 1
  } 
}

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
