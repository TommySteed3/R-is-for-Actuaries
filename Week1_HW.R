## READ IN LIBRARIES

library(dplyr)
library(data.table)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)


## IMPORT DATA
data.mortality <- fread('mortality.csv')
data.agebands <- read.csv('age_bandings.csv')

## BANDED AGE
data.grouped1 <- data.mortality %>%
  inner_join(data.agebands, by = 'attained.age') %>%
  group_by(banded.age) %>%
  summarise_at(vars(Deaths,Death.Dollar,qx7580E.amount,qx7580E.policy,qx2001VBT.amount,
                    qx2001VBT.policy,qx2008VBT.amount,qx2008VBT.policy,qx2008VBTLU.amount,
                    qx2008VBTLU.policy,qx2015VBT.amount,qx2015VBT.policy),list(sum)) %>%
  mutate(atoe7580d = qx7580E.amount/Death.Dollar,
         atoe01VBTd = qx2001VBT.amount/Death.Dollar,
         atoe08VBTd = qx2008VBT.amount/Death.Dollar,
         atoe08VBTLUd = qx2008VBTLU.amount/Death.Dollar,
         atoe15VBTd = qx2015VBT.amount/Death.Dollar,
         atoe7580c = qx7580E.policy/Deaths,
         atoe01VBTc = qx2001VBT.policy/Deaths,
         atoe08VBTc = qx2008VBT.policy/Deaths,
         atoe08VBTLUc = qx2008VBTLU.policy/Deaths,
         atoe15VBTc = qx2015VBT.policy/Deaths) %>%
  select(banded.age,atoe7580d,atoe01VBTd,atoe08VBTd,atoe08VBTLUd,atoe15VBTd,
         atoe7580c,atoe01VBTc,atoe08VBTc,atoe08VBTLUc,atoe15VBTc,)


# BANDED AGE AND GENDER
data.grouped2 <- data.mortality %>%
  inner_join(data.agebands, by = 'attained.age') %>%
  group_by(banded.age,Gender) %>%
  summarise_at(vars(Deaths,Death.Dollar,qx7580E.amount,qx7580E.policy,qx2001VBT.amount,
                    qx2001VBT.policy,qx2008VBT.amount,qx2008VBT.policy,qx2008VBTLU.amount,
                    qx2008VBTLU.policy,qx2015VBT.amount,qx2015VBT.policy),list(sum)) %>%
  mutate(atoe7580d = qx7580E.amount/Death.Dollar,
         atoe01VBTd = qx2001VBT.amount/Death.Dollar,
         atoe08VBTd = qx2008VBT.amount/Death.Dollar,
         atoe08VBTLUd = qx2008VBTLU.amount/Death.Dollar,
         atoe15VBTd = qx2015VBT.amount/Death.Dollar,
         atoe7580c = qx7580E.policy/Deaths,
         atoe01VBTc = qx2001VBT.policy/Deaths,
         atoe08VBTc = qx2008VBT.policy/Deaths,
         atoe08VBTLUc = qx2008VBTLU.policy/Deaths,
         atoe15VBTc = qx2015VBT.policy/Deaths) %>%
  select(banded.age,Gender,atoe7580d,atoe01VBTd,atoe08VBTd,atoe08VBTLUd,atoe15VBTd,
         atoe7580c,atoe01VBTc,atoe08VBTc,atoe08VBTLUc,atoe15VBTc,)



# BANDED AGE, GENDER, SMOKER STATUS
data.grouped3 <- data.mortality %>%
  inner_join(data.agebands, by = 'attained.age') %>%
  group_by(banded.age,Gender,smoker) %>%
  summarise_at(vars(Deaths,Death.Dollar,qx7580E.amount,qx7580E.policy,qx2001VBT.amount,
                    qx2001VBT.policy,qx2008VBT.amount,qx2008VBT.policy,qx2008VBTLU.amount,
                    qx2008VBTLU.policy,qx2015VBT.amount,qx2015VBT.policy),list(sum)) %>%
  mutate(atoe7580d = qx7580E.amount/Death.Dollar,
         atoe01VBTd = qx2001VBT.amount/Death.Dollar,
         atoe08VBTd = qx2008VBT.amount/Death.Dollar,
         atoe08VBTLUd = qx2008VBTLU.amount/Death.Dollar,
         atoe15VBTd = qx2015VBT.amount/Death.Dollar,
         atoe7580c = qx7580E.policy/Deaths,
         atoe01VBTc = qx2001VBT.policy/Deaths,
         atoe08VBTc = qx2008VBT.policy/Deaths,
         atoe08VBTLUc = qx2008VBTLU.policy/Deaths,
         atoe15VBTc = qx2015VBT.policy/Deaths) %>%
  select(banded.age,Gender,smoker,atoe7580d,atoe01VBTd,atoe08VBTd,atoe08VBTLUd,atoe15VBTd,
         atoe7580c,atoe01VBTc,atoe08VBTc,atoe08VBTLUc,atoe15VBTc,)
