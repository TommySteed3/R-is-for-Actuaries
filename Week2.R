  ##  WEEK 1 HW

library(dplyr)
library(data.table)
library(ggplot2)
library(plotly)
library(gridExtra)
library(grid)



data.mortality <- fread('mortality.csv')
data.agebands <- read.csv('age_bandings.csv')


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


data.grouped4 <- data.mortality %>%
  inner_join(data.agebands, by = 'attained.age') %>%
  group_by(banded.age,Gender,smoker,Preferred) %>%
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
  select(banded.age,Gender,Preferred,smoker,atoe7580d,atoe01VBTd,atoe08VBTd,atoe08VBTLUd,
         atoe15VBTd,atoe7580c,atoe01VBTc,atoe08VBTc,atoe08VBTLUc,atoe15VBTc,)


##  TASK 1

graph.task1 <- ggplot(data.grouped1,aes(x = banded.age, y = atoe7580d, group = 1))

scatter1 <- graph.task1 + geom_point()
line1 <- graph.task1 + geom_line()
bar1 <- graph.task1 + geom_bar(stat = "identity")
scatterline1 <- scatter1 + geom_line()

scatter1
bar1
line1
scatterline1

## TASK 2

graph.task2 <- ggplot(data.grouped2,aes(x = banded.age, y = atoe7580d, color = Gender, fill = Gender))

scatter2 <- graph.task2 + geom_point()
line2 <- graph.task2 + geom_line()
stackbar2 <- graph.task2 + geom_bar(stat = "identity",position = "stack")
fillbar2 <- graph.task2 + geom_bar(stat = "identity",position = "fill")
dodgebar2 <- graph.task2 + geom_bar(stat = "identity",position = "dodge")
scatterline2 <- scatter2 + geom_line()

scatter2
line2
stackbar2
fillbar2
dodgebar2
scatterline2

graph.task3 <- ggplot(data.grouped3,aes(x = banded.age, y = atoe7580d, color = Gender, fill = Gender))

linepoint3 <- graph.task3 + geom_line() + geom_point() + facet_wrap(~smoker)
linepoint3

linepoint4 <- graph.task4 + geom_line() + geom_point() + facet_grid(Preferred~smoker) + xlab("Banded Ages") + ylab("Actual to Expected") + ggtitle("Dollar Weighted Actual to Expected Analysis Using 7580E")
linepoint4


graph.task4 <- ggplot(data.grouped4,aes(x = banded.age, y = atoe7580d, color = Gender, fill = Gender))


ggplotly(scatterline2)
