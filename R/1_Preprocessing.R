library(dplyr)
library(tidyverse)

echo <- read.csv("./data/cardioOnco_echo.csv")
patients <- read.csv("cardioToxi_patients.csv")

'%!in%' <- Negate('%in%')

echo <- echo[,-1]
patients <- patients[,-1]
echo$studyDate <- as.Date(echo$studyDate)

echo <- echo %>% 
  group_by(ptno) %>%
  arrange(ptno,studyDate) %>%
  mutate(studySeq = 1:n()) 

exclusion <- echo %>%
  filter(is.na(lvGls) | is.na(ef))

exc_firstecho_cardiotoxicity <- echo %>%
  mutate(cardiotoxicity = ifelse(first(ef) - nth(ef,2) > 10 & abs(first(lvGls) - nth(lvGls,2)) > abs(first(lvGls)*0.15),1,0)) %>%
  filter(cardiotoxicity == 1)

echo <- echo %>%
  filter(.data$ptno %!in% exclusion$ptno) %>%
  filter(.data$ptno %!in% exc_firstecho_cardiotoxicity$ptno) %>%
  mutate(cardiotoxicity = ifelse(first(ef) - last(ef) > 10 & abs(first(lvGls) - last(lvGls)) > abs(first(lvGls)*0.15),1,0)) %>%
  mutate(baseEF = first(ef)) %>%
  mutate(baseLvGls = first(lvGls)) %>%
  mutate(firstEF = nth(ef, 2)) %>%
  mutate(firstLvGls = nth(lvGls, 2)) %>%
  mutate(deltaEF = first(ef) - nth(ef,2)) %>%
  mutate(deltaLvGls = first(lvGls) - nth(lvGls, 2))

uniqueEcho <- echo %>%
  filter(row_number() == 1) %>%
  select(ptno,baseEF,baseLvGls,firstEF,firstLvGls,deltaEF,deltaLvGls,cardiotoxicity)


patients <- merge(patients, uniqueEcho, by="ptno")  

patients <- rename(patients, 
                   cancerType = Cancer..type,
                   chemoType = Chemo.type_N,
                   doseOfAdrimycin = Dose.of.Adriamycin.mg.m2,
                   cancerStage = Cancer_Stage,
                   aFib = A.fib.or.A.FL)

patients$CAOD[patients$CAOD == "0 / MS" | patients$CAOD == "as moderate"] <- 0
patients$CAOD <- as.numeric(patients$CAOD)

patients$cancerStage[patients$cancerStage == "1a"] <- 1
patients$cancerStage[patients$cancerStage == "1b"] <- 1
patients$cancerStage[patients$cancerStage == "2a"] <- 2
patients$cancerStage[patients$cancerStage == "2b"] <- 2
patients$cancerStage[patients$cancerStage == "3a"] <- 3
patients$cancerStage[patients$cancerStage == "3b"] <- 3
patients$cancerStage <- as.numeric(patients$cancerStage)

patients$Sex <- ifelse(patients$Sex == 1, 0, 1)  # female : sex = 0

# cardiotoxicity : 23 / no cardiotoxicity : 279