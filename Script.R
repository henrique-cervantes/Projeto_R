# IMPORTAÇÃO DAS BIBLIOTECAS
library(readxl)
library(ggplot2)
library(tidyverse)
library(infer)
library(data.table)
library(stargazer)
library(stringr)
library(dygraphs)
library(plotly)

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
#### GUNS ####
# Importação dos dados
guns_dataset <- read_excel('TL-354-State-Level Estimates of Household Firearm Ownership.xlsx', sheet = 2) %>%
  filter(Year >= 1991) %>%
  select(-c("FIP"))
names(guns_dataset)[names(guns_dataset) == "Year"] <- "YEAR"

guns_dataset_older <- read_excel('TL-354-State-Level Estimates of Household Firearm Ownership.xlsx', sheet = 2) %>%
#  filter(Year >= 1991) %>%
  select(-c("FIP"))
names(guns_dataset_older)[names(guns_dataset_older) == "Year"] <- "YEAR"



#### POPULAÇÃO ####
# IMPORTAÇÃO DOS DADOS
data_pop <- read_csv("total_pop.csv") %>%
  select(-c("X1")) 
setDT(data_pop)
data_pop_arranged <- melt(data_pop)
names(data_pop_arranged) <- c("STATE", "YEAR", "POPULATION")
data_pop_arranged <- as.data.frame(data_pop_arranged[-c(1:50, 1351:1500),])

#### DF_REG ####

# CRIME POR ESTADO POR ANO
crime_per_state_per_year <- hate_crime_dataset %>%
  group_by(YEAR) %>%
  count(STATE) %>%
  arrange(desc(n))

# HFR por ESTADO 
guns_per_state <- guns_dataset %>%
  select(c(YEAR, STATE, HFR)) %>%
  group_by(STATE) %>%
  mutate(mean_HFR_per_state = mean(HFR))
guns_per_state <- guns_per_state[c(1:1300 %% 26 == 0), c(2, 4)]
guns_per_state; guns_per_state[order(-guns_per_state$mean_HFR_per_state),]


guns_per_state_per_year <- guns_dataset %>%
  select(c("YEAR", "STATE", "HFR"))
names(guns_per_state_per_year) <- c("YEAR", "STATE", "Prop_guns_p_y_s")


# Proporção de CRIME por ESTADO por ANO,
# será usado para compor o df_reg
# Adicionando uma coluna com a proporção de crimes na populaçao por estado ao longo do tempo:
prop_crime_per_state_per_year <- merge(crime_per_state_per_year, data_pop_arranged) %>%
  mutate(PROP_CRIME = n / POPULATION)



#### Construindo o DataFrame com freq e prop de armas e crimes
df_intermediario <- merge(prop_crime_per_state_per_year, guns_per_state_per_year)
df_reg <- merge(df_intermediario, guns_per_state)
names(df_reg) <- c("STATE", "YEAR", "CRIME_PER_STATE_PER_YEAR", "POPULATION",
                   "PROP_CRIME", "PROP_GUNS_PER_STATE_PER_YEAR",
                   "MEAN_HFR_PER_STATE")
df_reg$NUMBER_GUNS <- round(df_reg$POPULATION * df_reg$PROP_GUNS_PER_STATE_PER_YEAR, 0)

data_pop_arranged_2 <- data_pop_arranged %>%
  arrange(STATE)


#### certo
guns_dataset$NUMBER_GUNS <- round(data_pop_arranged_2$POPULATION * guns_dataset$HFR)

guns_dataset$POPULATION <- data_pop_arranged_2$POPULATION
#### certo

#########################################################################




########## ANÁLISE INICIAL DOS DADOS HATE_CRIME ####

# Crimes por ANO 
total_crime_per_year <- hate_crime_dataset %>%
  count(YEAR)
# Ordernado decrescente ou crescente:
arrange(total_crime_per_year, desc(n))

#ggplot(total_crime_per_year, aes(x=YEAR, y=n)) + geom_line()

dygraph(total_crime_per_year,
        main = "Número de crimes de ódio por ano (agregado)",
        ylab = "Crimes",
        xlab = "Anos")

predict(total_crime_per_year, n.ahead = 72, prediction.interval=TRUE)



# Crimes por ESTADO 
total_crime_per_state <- hate_crime_dataset %>%
  count(STATE) %>%
  filter(STATE != 'Guam') 
# Ordernado decrescente ou crescente:
arrange(total_crime_per_state, desc(n))

# Crimes por ESTADO normalizado
total_crime_per_state_norm <- b %>%
  group_by(STATE, YEAR, POPULATION) %>%
  summarise(across(starts_with("ANTI"), sum))





total_crime_per_state_norm <- hate_crime_dataset %>%
  group_by(STATE, YEAR) %>%
  summarise(across(starts_with("ANTI"), sum))



# Crimes por ESTADO por ANO
crime_per_state_per_year

# TRENDLINE CRIME por ESTADO por ANO:
ggplot(crime_per_state_per_year, aes(x = YEAR, y = n)) +
    geom_line() +
    facet_wrap(~ STATE)

# Plotando os 5 estados com mais crimes ao longo dos anos:
crime_per_state_per_year %>%
  filter(STATE == c(arrange(total_crime_per_state, desc(n))[1:5, 1])) %>%
  ggplot(aes(x = YEAR, y = n)) +
    geom_line() +
    facet_wrap(~ STATE)
  

########## Total de CRIMES por TIPO por ESTADO por ANO ###############
total_crime_per_type <- hate_crime_dataset %>%
  group_by(YEAR, STATE) %>%
  count(OFFENSE_NAME)
total_crime_per_type


total_crime_per_type %>%
  select(OFFENSE_NAME, n) %>%
  arrange(desc(n))

arrange(total_crime_per_type, desc(n))

#total_crime_per_type[order(-total_crime_per_type$n),]
# tipos de crimes
sort(unique(total_crime_per_type$OFFENSE_NAME))



########## Total de CRIMES por LUGAR por ESTADO por ANO ################

total_crime_per_location <- hate_crime_dataset %>%
  group_by(YEAR, STATE) %>%
  count(LOCATION_NAME)
total_crime_per_location

sort(unique(total_crime_per_location$LOCATION_NAME))

########## Total de CRIMES por ETNIA por ESTADO por ANO #############
total_crime_per_etnia <- hate_crime_dataset %>%
  group_by(YEAR, STATE) %>%
  count(OFFENDER_RACE)
total_crime_per_etnia

# offender race (tá certo):
unique(total_crime_per_etnia$OFFENDER_RACE)


########## Total de CRIMES por VÍTIMA por ESTADO por ANO #######
total_crime_per_victim <- hate_crime_dataset %>%
  group_by(YEAR, STATE) %>%
  count(BIAS_DESC)
total_crime_per_victim
sort(unique(total_crime_per_victim$BIAS_DESC))


# sem white
total_crime_per_victim_corrigido <- subset(total_crime_per_victim, BIAS_DESC != 'Anti-White')





######################## CONSTRUINDO A DISTRIBUIÇÃO POR VÍTIMA ######################

############################ ANTI MULHER/HOMEM ############

# ANTI_FEMALE
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_FEMALE = ifelse(str_detect(BIAS_DESC, "Anti-Female"), 1, 0))


# ANTI_MALE (n faz sentido)
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_MALE = ifelse(str_detect(BIAS_DESC, "Anti-Male"), 1, 0))


############################ ETNIAS ############################

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
  mutate(ANTI_HAWAIIAN = ifelse(str_detect(BIAS_DESC, "Anti-Native Hawaiian or Other Pacific Islander"), 1, 0))


# ANTI_HINDU
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_HINDU = ifelse(str_detect(BIAS_DESC, "Anti-Hindu"), 1, 0))


# ANTI-OTHER-RACE
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_OTHER_RACE = ifelse(str_detect(BIAS_DESC, "Anti-Other Race/Ethnicity/Ancestry"), 1, 0))


# ANTI_MULTIPLE_RACES
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_MULT_RACE = ifelse(str_detect(BIAS_DESC, "Anti-Multiple Races, Group"), 1, 0))

############################ RELIGIÕES ########################################

# ANTI-ISLAMIC
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_ISLAMIC = ifelse(str_detect(BIAS_DESC, "Anti-Islamic"), 1, 0))

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
  mutate(ANTI_ORTHODOX = ifelse(str_detect(BIAS_DESC, "Anti-Eastern Orthodox"), 1, 0))


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


############################ LGBT+ ##############################

# ANTI_HETEROSEXUAL (n faz sentido)
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_HETEROSEXUAL = ifelse(str_detect(BIAS_DESC, "Anti-Heterosexual"), 1, 0))


# ANTI_GAY.2
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_GAY = ifelse(str_detect(BIAS_DESC, "Gay"), 1, 0))


# ANTI_LESBIAN.2
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_LESBIAN = ifelse(str_detect(BIAS_DESC, "Lesbian"), 1, 0))


# ANTI_BISEXUAL.2
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_BISEXUAL = ifelse(str_detect(BIAS_DESC, "Bisexual"), 1, 0))


# ANTI_TRANSGENDER.2
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_TRANSGENDER = ifelse(str_detect(BIAS_DESC, "Transgender"), 1, 0))


# ANTI_GENDER_NON_CONFORMING
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_GENDER_NON_CONF = ifelse(str_detect(BIAS_DESC, "Anti-Gender Non-Conforming"), 1, 0))


# ANTI-LGBT+ GENÉRICO
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_OTHER_LGBT = ifelse(str_detect(BIAS_DESC, "Anti-Lesbian, Gay, Bisexual, or Transgender"), 1, 0))


############################ cAPACITISMO ######################################

# ANTI_MENTAL_DISABILITY
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_MENTAL_DIS = ifelse(str_detect(BIAS_DESC, "Anti-Mental Disability"), 1, 0))

# ANTI_PHYSICAL_DISABILITY
hate_crime_dataset <- hate_crime_dataset %>% 
  mutate(ANTI_PHYSICAL_DIS = ifelse(str_detect(BIAS_DESC, "Anti-Physical Disability"), 1, 0))



# reduce
a <- hate_crime_dataset %>%
  group_by(STATE, YEAR) %>%
  summarise(across(starts_with("ANTI"), sum))

# Dataframe mais importante de todos ->
b <- full_join(guns_dataset, a) 
b[is.na(b)]<-0


############################ FIM DAS CORREÇÕES DE VICTIM BIAS ################ 


# PLOTANDO VÍTIMAS NEGRAS POR ESTADO POR ANO 
cab_per_state_per_year <- hate_crime_dataset %>%
  filter(str_detect(BIAS_DESC, "Black")) %>%
  group_by(YEAR) %>%
  count(STATE)
cab_per_state_per_year

arrange(cab_per_state_per_year, desc(n))

ggplot(cab_per_state_per_year, aes(x = YEAR, y = n)) +
  geom_line() +
  facet_wrap(~STATE)




cab_per_state_per_year %>%
  filter(STATE == "California") %>%
  dygraph(main = "Número de crimes de ódio por ano (agregado)",
          ylab = "Crimes",
          xlab = "Anos")



cab_per_state_per_year %>%
  filter(n > 200) %>%
  ggplot(aes(x = YEAR, y = n)) +
    geom_point() + 
    stat_smooth(method = "lm", col = "red")
  



# PLOTANDO VÍTIMAS MULHERES POR ESTADO POR ANO
caf_per_state_per_year <- hate_crime_dataset %>%
  filter(str_detect(BIAS_DESC, "Female")) %>%
  group_by(YEAR) %>%
  count(STATE)
caf_per_state_per_year

ggplot(caf_per_state_per_year, aes(x = YEAR, n)) +
  geom_line() +
  facet_wrap(~STATE)

cab_per_state_per_year %>%
  filter(n > 200) %>%
  ggplot(aes(x = YEAR, y = n)) +
  geom_point() + 
  stat_smooth(method = "lm", col = "red")



b_new<- b %>%
  mutate(across(starts_with("ANTI"), .fns = ~./POPULATION)) 

b_new[which(is.nan(b_new))] <- 0
b_new[which(b_new == Inf)] <- 0
b_new[is.na(b_new)] <- 0



####################### REGRESSAO N_GUNS ###########################

# regressao - black ~ n_guns
reg_black <- summary(lm(ANTI_BLACK ~ NUMBER_GUNS + factor(YEAR) + factor(STATE), b))
reg_black
coef(reg_black)[1]

ggplot(b, aes(x = NUMBER_GUNS, y = ANTI_BLACK)) +
  geom_point()


reg_black <- summary(lm(ANTI_BLACK ~ NUMBER_GUNS + factor(YEAR) + factor(STATE), b_new))
reg_black
coef(reg_black)[1]

ggplot(b_new, aes(x = NUMBER_GUNS, y = ANTI_BLACK)) +
  geom_point()



# regressao - female ~ n_guns
reg_female <- summary(lm(ANTI_FEMALE ~ NUMBER_GUNS + factor(YEAR) + factor(STATE), b))
reg_female
coef(reg_female)[1]

ggplot(b, aes(x = NUMBER_GUNS, y = ANTI_FEMALE)) +
  geom_point()

# regressao - islamic ~ n_guns
reg_islamic <- summary(lm(ANTI_ISLAMIC ~ NUMBER_GUNS + factor(YEAR) + factor(STATE), b))
reg_islamic
coef(reg_islamic)[1]

ggplot(b, aes(x = NUMBER_GUNS, y = ANTI_ISLAMIC)) +
  geom_point()

# regressao - anti-lgbt ~ n_guns
reg_lgbt <- summary(lm(ANTI_GAY + ANTI_LESBIAN + ANTI_BISEXUAL + ANTI_TRANSGENDER + ANTI_GENDER_NON_CONF ~ NUMBER_GUNS + factor(YEAR) + factor(STATE), b))
reg_lgbt
coef(reg_lgbt)[1]

ggplot(b, aes(x = NUMBER_GUNS, y = ANTI_GAY)) +
  geom_point()

####################### REGRESSÃO HFR ######################

# regressao - black ~ n_guns
reg_black_hfr <- summary(lm(ANTI_BLACK ~ HFR + factor(YEAR) + factor(STATE), b))
reg_black_hfr
coef(reg_black_hfr)[1]

ggplot(b, aes(x = HFR, y = ANTI_BLACK)) +
  geom_point()

# regressao - female ~ n_guns
reg_female_hfr <- summary(lm(ANTI_FEMALE ~ HFR + factor(YEAR) + factor(STATE), b))
reg_female_hfr
coef(reg_female_hfr)[1]

ggplot(b, aes(x = HFR, y = ANTI_FEMALE)) +
  geom_point()

# regressao - islamic ~ n_guns
reg_islamic_hfr <- summary(lm(ANTI_ISLAMIC ~ HFR + factor(YEAR) + factor(STATE), b))
reg_islamic_hfr
coef(reg_islamic_hfr)[1]

ggplot(b, aes(x = HFR, y = ANTI_ISLAMIC)) +
  geom_point()

# regressao - anti-lgbt ~ n_guns
reg_lgbt_hfr <- summary(lm(ANTI_GAY + ANTI_LESBIAN + ANTI_BISEXUAL + ANTI_TRANSGENDER + ANTI_GENDER_NON_CONF ~ HFR + factor(YEAR) + factor(STATE), b))
reg_lgbt_hfr
coef(reg_lgbt_hfr)[1]

ggplot(b, aes(x = HFR, y = ANTI_GAY)) +
  geom_point()


####################### REGRESSÃO PERMIT #########################

# regressao - black ~ permit
reg_black_permit <- summary(lm(ANTI_BLACK ~ permit + factor(YEAR) + factor(STATE), b))
reg_black_permit
coef(reg_black_permit)[1]

# regressao - female ~ permit
reg_female_permit <- summary(lm(ANTI_FEMALE ~ permit + factor(YEAR) + factor(STATE), b))
reg_female_permit
coef(reg_female_permit)[1]

# regressao - islamic
reg_islamic_permit <- summary(lm(ANTI_ISLAMIC ~ permit + factor(YEAR) + factor(STATE), b))
reg_islamic_permit
coef(reg_islamic_permit)[1]


# regressao - anti-lgbt ~ permit
reg_lgbt_permit <- summary(lm(ANTI_GAY + ANTI_LESBIAN + ANTI_BISEXUAL + ANTI_TRANSGENDER + ANTI_GENDER_NON_CONF ~ permit + factor(YEAR) + factor(STATE), b))
reg_lgbt_permit
coef(reg_lgbt_permit)[1]

####################### REGRESSÃO UNIVERSL ############################

# regressao - anti_black ~ universl
reg_black_universl <- summary(lm(ANTI_BLACK ~ universl + factor(YEAR) + factor(STATE), b))
reg_black_universl
coef(reg_black_universl)[1]


reg_black_universl <- summary(lm(ANTI_BLACK ~ universl + factor(YEAR) + factor(STATE), b))
reg_black_universl
coef(reg_black_universl)[1]


# regressao - anti_female ~ universl 
reg_female_universl <- summary(lm(ANTI_FEMALE ~ universl + factor(YEAR) + factor(STATE), b))
reg_female_universl
coef(reg_female_universl)[1]

# regressao - anti_islamic ~ universl 
reg_islamic_universl <- summary(lm(ANTI_ISLAMIC ~ universl + factor(YEAR) + factor(STATE), b))
reg_islamic_universl
coef(reg_islamic_universl)[1]

# regressao - anti_islamic ~ universl 
reg_lgbt_universl <- summary(lm(ANTI_GAY + ANTI_LESBIAN + ANTI_BISEXUAL + ANTI_TRANSGENDER + ANTI_GENDER_NON_CONF ~ universl + factor(YEAR) + factor(STATE), b))
reg_lgbt_universl
coef(reg_lgbt_universl)[1]







# regressao - anti-black ~ mean_hfr
reg_black <- summary(lm(ANTI_BLACK ~ HFR + factor(YEAR) + factor(STATE), b))
reg_black
coef(reg_black)[1]

b %>%
  ggplot(aes(x = HFR, y = ANTI_BLACK)) +
  geom_point()


# regressao - anti_female ~ mean hfr
reg_female <- summary(lm(ANTI_FEMALE ~ HFR + factor(YEAR) + factor(STATE), b))
reg_female
coef(reg_female)[1]

b %>%
  ggplot(aes(x = HFR, y = ANTI_FEMALE)) +
  geom_point()


# regressao - anti_islamic ~ mean hfr
reg_islamic <- summary(lm(ANTI_ISLAMIC ~ HFR + factor(YEAR) + factor(STATE), b))
reg_islamic
coef(reg_islamic)[1]

b %>%
  ggplot(aes(x = HFR, y = ANTI_ISLAMIC)) +
  geom_point()


# regressao - anti_LGBT ~ mean hfr
reg_LGBT <- summary(lm(ANTI_GAY + ANTI_LESBIAN + ANTI_BISEXUAL + ANTI_TRANSGENDER + ANTI_GENDER_NON_CONF ~ HFR + factor(YEAR) + factor(STATE), b))
reg_LGBT
coef(reg_LGBT)[1]


b %>%
  ggplot(aes(x = HFR, y = c(ANTI_GAY + ANTI_LESBIAN + ANTI_BISEXUAL + ANTI_TRANSGENDER + ANTI_GENDER_NON_CONF))) +
  geom_point()





########################### ANÁLISE INICIAL DOS DADOS DE ARMAS #####

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

guns_per_year %>%
  dygraph(main = "Armas por pessoa nos EUA entre 19901 e 2016",
          ylab = "Armas",
          xlab = "Anos")

# HFR por ESTADO 
guns_per_state 


# HFR POR ESTADO POR ANO
guns_per_state_per_year 


# Gráfico de HFR por ANO por ESTADO
ggplot(guns_dataset, aes(x = YEAR, y = HFR)) +
  geom_line() +
  facet_wrap(~ STATE)


######## DADOS AGREGADOS CRIME feat.GUNS ########
data_crime_guns <- merge(hate_crime_dataset, guns_dataset)

data_crime_guns_2 <- merge(guns_dataset, hate_crime_dataset)

#### certo
guns_dataset$NUMBER_GUNS <- round(data_pop_arranged_2$POPULATION * guns_dataset$HFR)

guns_dataset$POPULATION <- data_pop_arranged_2$POPULATION
#### certo



guns_per_state_per_year$NUMBER_GUNS <- round(data_pop_arranged_2$population * df_)


########################### DATASET POPULAÇÃO ####################

prop_crime_per_state_per_year

#### Construindo o DataFrame com freq e prop de armas e crimes ####


# Gráfico com as proporções
ggplot(df_reg, aes(x = PROP_GUNS_PER_STATE_PER_YEAR, y = PROP_CRIME)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Prop Guns vs Crimes", subtitle = "Year and State", 
       x = "Prop guns", y = "Prop crimes")

# Gráfico com os totais
ggplot(df_reg, aes(x = NUMBER_GUNS, y = CRIME_PER_STATE_PER_YEAR)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Guns vs Crimes", subtitle = "Year and State agg", x = "Number of Crimes", y = "Number of guns")

# Reg 1.a: total crimes ~ total armas
lm1a <- lm(CRIME_PER_STATE_PER_YEAR ~ NUMBER_GUNS + factor(YEAR) + factor(STATE), data = df_reg)
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
lm4 <- summary(lm(HFR ~ universl + factor(YEAR) + factor(STATE), guns_dataset))
coefficients(lm4)[1]


# REG 5 -> HFR ~ PERMIT -> como o permit impacta HFR 
lm5 <- lm(HFR ~ permit + factor(YEAR) + factor(STATE), guns_dataset)
summary(lm5)
coefficients(lm5)[1]

# REG 6 -> crime ~ vítimas negras 
lm6 <- lm(df_reg$CRIME_PER_STATE_PER_YEAR ~ hb$n)


























#################### DIFF IN DIFF? ###################

analise <- data_pop[c(33, 46),]

# coletando a prop de armas em North Carolina e em Virginia
analise_hfr <- rbind(guns_per_state_per_year[c(833: 858),], guns_per_state_per_year[c(1171: 1196),])

# plotando a proporção de armas em cada estado
ggplot(analise_hfr, aes(x = YEAR, y = Prop_guns_p_y_s)) +
  geom_line() +
  facet_wrap(~STATE)

# coletando o numero absoluto de armas em North Carolina e em Virginia 
analise_armas <- rbind(b[c(833:858), c('YEAR', 'STATE', 'NUMBER_GUNS')], b[c(1171:1196), c('YEAR', 'STATE', 'NUMBER_GUNS')])

# plotando o numero absoluto de armas em cada estado 
ggplot(analise_armas, aes(x = YEAR, y = NUMBER_GUNS)) +
  geom_line() +
  facet_wrap(~STATE)


# coletando o numero de crimes de ódio em North Carolina e em Virginia
analise_crimes <- crime_per_state_per_year %>%
  select(STATE, YEAR, n) %>%
  filter(STATE == c('North Carolina', 'Virginia')) %>%
  group_by(YEAR) %>%
  arrange(YEAR) %>%
  arrange(STATE)

# plotando a quantidade de crimes em cada estado
ggplot(analise_crimes, aes(x = YEAR, y = n)) +
  geom_line() +
  facet_wrap(~STATE)


# D&D Crimes 

# antes do tratamento:
crime_nc_1993 <- analise_crimes %>%
  filter(STATE == 'North Carolina', YEAR == 1993) %>%
  select(n)
crime_nc_1993 <- crime_nc_1993[2]

crime_vi_1993 <- analise_crimes %>%
  filter(STATE=='Virginia', YEAR == 1993) %>%
  select(n)

crime_vi_1993 <- crime_vi_1993[2]

# pós tratamento:
crime_nc_1996 <- analise_crimes %>%
  filter(STATE=='North Carolina', YEAR == 1996) %>%
  select(n)
crime_nc_1996 <- crime_nc_1996[2]


crime_vi_1996 <- analise_crimes %>%
  filter(STATE=='Virginia', YEAR == 1996) %>%
  select(n)
crime_vi_1996 <- crime_vi_1996[2]


# primeira diferença
d_crime_1993 <- crime_nc_1993 - crime_vi_1993

# segunda diferença
d_crime_1996 <- crime_vi_1996 - crime_vi_1996

# DiD
did_crime <- d_crime_1996 - d_crime_1993
did_crime



# D&D - prop de armas
# antes do tratamento:
hfr_nc_1994 <- analise_hfr %>%
  filter(STATE=='North Carolina', YEAR == 1994) %>%
  select(Prop_guns_p_y_s)

hfr_vi_1994 <- analise_hfr %>%
  filter(STATE=='Virginia', YEAR == 1994) %>%
  select(Prop_guns_p_y_s)

# pós tratamento:
hfr_nc_1996 <- analise_hfr %>%
  filter(STATE=='North Carolina', YEAR == 1996) %>%
  select(Prop_guns_p_y_s)

hfr_vi_1996 <- analise_hfr%>%
  filter(STATE=='Virginia', YEAR == 1996) %>%
  select(Prop_guns_p_y_s)


# primeira diferença
d_hfr_1994 <- hfr_nc_1994 - hfr_vi_1994

# segunda diferença
d_hfr_1996 <- hfr_nc_1996 - hfr_vi_1996

# DiD
did_hfr <- d_hfr_1996 - d_hfr_1994




# D&D - n absoluto de armas

# antes do tratamento :
armas_nc_1994 <- analise_armas%>%
  filter(STATE=='North Carolina', YEAR == 1994) %>%
  select(NUMBER_GUNS)

armas_vi_1994 <- analise_armas%>%
  filter(STATE=='Virginia', YEAR == 1994) %>%
  select(NUMBER_GUNS)

# pós tratamento:
armas_nc_1996 <- analise_armas%>%
  filter(STATE=='North Carolina', YEAR == 1996) %>%
  select(NUMBER_GUNS)

armas_vi_1996 <- analise_armas%>%
  filter(STATE=='Virginia', YEAR == 1996) %>%
  select(NUMBER_GUNS)

# primeira diferença
d_1994 <- armas_nc_1994 - armas_vi_1994

# segunda diferença
d_1996 <- armas_nc_1996 - armas_vi_1996

# DiD
did_armas <- d_1996 - d_1994







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


year <- c(sort(c(rep(seq(1980, 2020, 4), 51))), NaN)

state <- c(rep(unique(president$state), 11), NaN)



# df com ganhadores e numero de votos por ano por estado 
df_elections <- data.frame(YEAR = year,
                           STATE = state,
                           MOST_VOTED_PARTY = most_voted_party,
                           WINNER_VOTES = votes_winner,
                           LOSER_VOTES = votes_loser)


# tem um dado a mais (mas parece estar certo (?))



######## DATASET PREFERÊNCIAS POLÍTICAS ######## PARA FAZER!!!
data_pol_pref_raw <- read_xlsx('Partisan_Balance_For_Use2011_06_09b.xlsx')
names(data_pol_pref_raw)[names(data_pol_pref_raw) == "year"] <- "YEAR"
names(data_pol_pref_raw)[names(data_pol_pref_raw) == "state"] <- "STATE"
data_pol_pref <- data_pol_pref_raw %>%
  filter(YEAR >= 1980)













############################################################################
############################### RASCUNHO ######################################
############################################################################
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



# Para visualisar algum estado específico:

func <- function(df, col, x) {
  return(df %>%
           filter(col == as.str(x)) %>%
           dygraph(main = "Número de crimes de ódio por ano da categoria {}",
                   ylab = "Crimes",
                   xlab = "Anos"))
}

func(cab_per_state_per_year, STATE, "Utah")




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


# Estados que adotaram permit:

"""
California - 1993
Connecticut - 1993
Hawaii - 1991
Illinois - 1991
Iowa - 1991
Maryland - 2013
Massachusetts - 1991
Michigan - 1991
Missouri - 1991 - 2006
Nebraska - 1991
New Jersey - 1991
New Yorkk - 1991
North Carolina - 1995
Rhode Island - 1991
"""