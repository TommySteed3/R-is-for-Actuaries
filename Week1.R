
###  TASK 1
# install.packages('dplyr')
library(dplyr)
library(data.table)


## TASK 2
data.valyear <- read.csv('valyear_mapping.csv')
data.mortality <- fread('mortality.csv')


## TASK 3
data.grouped <- data.mortality %>%
  group_by(Val.Year) %>%
  summarise(actual = sum(Deaths), expected = sum(qx2015VBT.policy))

## TASK 4
data.grouped <- data.grouped %>%
  mutate(a.to.e = actual/expected)

## TASK 5
data.grouped <- data.grouped %>%
  inner_join(data.valyear, by = 'Val.Year')

data.grouped <- data.grouped %>%
  mutate(underwriter2 = ifelse(Val.Year < 2012,"A",
                               ifelse(Val.Year == 2012 | Val.Year == 2013,"B","C")))

## TASK 6
data.grouped <- data.grouped %>%
  filter(actual > 2000)

## TASK 7
data.groupeda <- data.grouped %>%
  select(c('Val.Year','actual','expected','underwriter','a.to.e'))

data.grouped <- data.grouped %>%
  select(-c('underwriter2'))


## SUMMARY
data.chained <- data.mortality %>%
  group_by(Val.Year) %>%
  summarise(actual = sum(Deaths), expected = sum(qx2015VBT.policy)) %>%
  mutate(a.to.e = actual/expected) %>%
  inner_join(data.valyear, by = 'Val.Year') %>%
  mutate(underwriter2 = ifelse(Val.Year < 2012,"A",
                               ifelse(Val.Year == 2012 | Val.Year == 2013,"B","C"))) %>%
  filter(actual > 2000) %>%
  select(-c('underwriter2'))



