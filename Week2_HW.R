library(data.table)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr) # summarise_at()
library(forcats) # helps reorder variables
library(gridExtra) # to use grid arrange - which we have seen


# SEVERAL LESSONS (THINK - PROJECTS) USING SAME DATASET - DON'T WANT A BUNCH OF COPIES - NEXT LEVEL (OF MANY)

data.mortality <- fread('../Data/mortality.csv')

## Theme will be a to e and number of deaths
## First create these 2 charts for all 4 factors - no interaction

###############################  FACE BAND #################################################

data.mortality.faceband <- data.mortality %>%
  group_by(face.band) %>%
  summarise_at(vars(Deaths,Death.Dollar,qx7580E.amount,qx7580E.policy,qx2001VBT.amount,
                    qx2001VBT.policy,qx2008VBT.amount,qx2008VBT.policy,qx2008VBTLU.amount,
                    qx2008VBTLU.policy,qx2015VBT.amount,qx2015VBT.policy),list(sum)) %>%
  mutate(atoe7580d = Death.Dollar/qx7580E.amount,
         atoe01VBTd = Death.Dollar/qx2001VBT.amount,
         atoe08VBTd = Death.Dollar/qx2008VBT.amount,
         atoe08VBTLUd = Death.Dollar/qx2008VBTLU.amount,
         atoe15VBTd = Death.Dollar/qx2015VBT.amount,
         atoe7580c = Deaths/qx7580E.policy,
         atoe01VBTc = Deaths/qx2001VBT.policy,
         atoe08VBTc = Deaths/qx2008VBT.policy,
         atoe08VBTLUc = Deaths/qx2008VBTLU.policy,
         atoe15VBTc = Deaths/qx2015VBT.policy) %>%
  select(Deaths,face.band,atoe7580d,atoe01VBTd,atoe08VBTd,atoe08VBTLUd,atoe15VBTd,
         atoe7580c,atoe01VBTc,atoe08VBTc,atoe08VBTLUc,atoe15VBTc) %>%
  as.data.frame(data.mortality.faceband) %>%
  mutate(face.band = fct_relevel(face.band, 
                                 '1-9999','10000-24999','25000-49999','50000-99999','100000-249999',
                                 '250000-499999','500000-999999','1000000-2499999','2500000-4999999',
                                 '5000000-9999999'))

##  CREATING VERTICAL AXIS LABELS
## ADDED LABEL TO DATA POINTS
## POINT/LINE GRAPH FOR A/E VALUES ; BAR GRAPH FOR DEATHS (CREDIBILITY)
## HORIZONTAL LINE AT 3007 FOR CLASSICAL CREDIBILITY STANDARD OF FULL CREDIBILITY

Mortality.By.FaceBand <- ggplot(data.mortality.faceband,aes(x = face.band, y = atoe15VBTc, group = 1, label = round(atoe15VBTc,2))) + 
  geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text() + geom_label()

Deaths.By.FaceBand <- ggplot(data.mortality.faceband,aes(x = face.band, y = Deaths, group = 1, label = Deaths)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text() + geom_label() + geom_line(y = 3007)

###############################  VALUTAION YEAR #################################################


data.mortality.valyear <- data.mortality %>%
  group_by(Val.Year) %>%
  summarise_at(vars(Deaths,Death.Dollar,qx7580E.amount,qx7580E.policy,qx2001VBT.amount,
                    qx2001VBT.policy,qx2008VBT.amount,qx2008VBT.policy,qx2008VBTLU.amount,
                    qx2008VBTLU.policy,qx2015VBT.amount,qx2015VBT.policy),list(sum)) %>%
  mutate(atoe7580d = Death.Dollar/qx7580E.amount,
         atoe01VBTd = Death.Dollar/qx2001VBT.amount,
         atoe08VBTd = Death.Dollar/qx2008VBT.amount,
         atoe08VBTLUd = Death.Dollar/qx2008VBTLU.amount,
         atoe15VBTd = Death.Dollar/qx2015VBT.amount,
         atoe7580c = Deaths/qx7580E.policy,
         atoe01VBTc = Deaths/qx2001VBT.policy,
         atoe08VBTc = Deaths/qx2008VBT.policy,
         atoe08VBTLUc = Deaths/qx2008VBTLU.policy,
         atoe15VBTc = Deaths/qx2015VBT.policy) %>%
  select(Val.Year,Deaths,atoe7580d,atoe01VBTd,atoe08VBTd,atoe08VBTLUd,atoe15VBTd,
         atoe7580c,atoe01VBTc,atoe08VBTc,atoe08VBTLUc,atoe15VBTc) %>%
  as.data.frame(data.mortality.faceband)

data.mortality.valyear$Val.Year <- as.factor(data.mortality.valyear$Val.Year)

Mortality.Val.Year <- ggplot(data.mortality.valyear,aes(x = Val.Year, y = atoe15VBTc, group = 1, label = round(atoe15VBTc,2))) + 
  geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text() + geom_label()

Deaths.Val.Year <- ggplot(data.mortality.valyear,aes(x = Val.Year, y = Deaths, group = 1, label = Deaths)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text() + geom_label() + geom_line(y = 3007)


#################################################################################################
###############################  GENDER  ########################################################

data.Gender <- data.mortality %>%
  group_by(Gender) %>%
  summarise_at(vars(Deaths,Death.Dollar,qx7580E.amount,qx7580E.policy,qx2001VBT.amount,
                    qx2001VBT.policy,qx2008VBT.amount,qx2008VBT.policy,qx2008VBTLU.amount,
                    qx2008VBTLU.policy,qx2015VBT.amount,qx2015VBT.policy),list(sum)) %>%
  mutate(atoe7580d = Death.Dollar/qx7580E.amount,
         atoe01VBTd = Death.Dollar/qx2001VBT.amount,
         atoe08VBTd = Death.Dollar/qx2008VBT.amount,
         atoe08VBTLUd = Death.Dollar/qx2008VBTLU.amount,
         atoe15VBTd = Death.Dollar/qx2015VBT.amount,
         atoe7580c = Deaths/qx7580E.policy,
         atoe01VBTc = Deaths/qx2001VBT.policy,
         atoe08VBTc = Deaths/qx2008VBT.policy,
         atoe08VBTLUc = Deaths/qx2008VBTLU.policy,
         atoe15VBTc = Deaths/qx2015VBT.policy) %>%
  select(Gender,Deaths,atoe7580d,atoe01VBTd,atoe08VBTd,atoe08VBTLUd,atoe15VBTd,
         atoe7580c,atoe01VBTc,atoe08VBTc,atoe08VBTLUc,atoe15VBTc) 

Mortality.Gender <- ggplot(data.Gender,aes(x = Gender, y = atoe15VBTc, group = 1, label = round(atoe15VBTc,2))) + 
  geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text() + geom_label()

Deaths.Gender <- ggplot(data.Gender,aes(x = Gender, y = Deaths, group = 1, label = Deaths)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text() + geom_label() + geom_line(y = 3007)




#################################################################################################
#####################################  AGE BASIS  ###############################################


data.mortality.age.basis <- data.mortality %>%
  group_by(age.basis) %>%
  summarise_at(vars(Deaths,Death.Dollar,qx7580E.amount,qx7580E.policy,qx2001VBT.amount,
                    qx2001VBT.policy,qx2008VBT.amount,qx2008VBT.policy,qx2008VBTLU.amount,
                    qx2008VBTLU.policy,qx2015VBT.amount,qx2015VBT.policy),list(sum)) %>%
  mutate(atoe7580d = Death.Dollar/qx7580E.amount,
         atoe01VBTd = Death.Dollar/qx2001VBT.amount,
         atoe08VBTd = Death.Dollar/qx2008VBT.amount,
         atoe08VBTLUd = Death.Dollar/qx2008VBTLU.amount,
         atoe15VBTd = Death.Dollar/qx2015VBT.amount,
         atoe7580c = Deaths/qx7580E.policy,
         atoe01VBTc = Deaths/qx2001VBT.policy,
         atoe08VBTc = Deaths/qx2008VBT.policy,
         atoe08VBTLUc = Deaths/qx2008VBTLU.policy,
         atoe15VBTc = Deaths/qx2015VBT.policy) %>%
  select(age.basis,Deaths,atoe7580d,atoe01VBTd,atoe08VBTd,atoe08VBTLUd,atoe15VBTd,
         atoe7580c,atoe01VBTc,atoe08VBTc,atoe08VBTLUc,atoe15VBTc) %>%
  as.data.frame(data.mortality.faceband)

data.mortality.age.basis$age.basis <- as.factor(data.mortality.age.basis$age.basis)

Mortality.Age.Basis <- ggplot(data.mortality.age.basis,aes(group = 1, x = age.basis, y = atoe15VBTc, label = round(atoe15VBTc,2))) + 
  geom_point() + geom_line() + geom_label()

Deaths.Age.Basis <- ggplot(data.mortality.age.basis,aes(x = age.basis, y = Deaths, label = Deaths)) + 
  geom_bar(stat = "identity") + geom_label() + geom_line(y = 3007)

################################################################################################
################################################################################################
##########################      INTERACTION      ###############################################

data.everything <- data.mortality %>%
  group_by(face.band,Val.Year,age.basis,Gender) %>%
  summarise_at(vars(Deaths,Death.Dollar,qx7580E.amount,qx7580E.policy,qx2001VBT.amount,
                    qx2001VBT.policy,qx2008VBT.amount,qx2008VBT.policy,qx2008VBTLU.amount,
                    qx2008VBTLU.policy,qx2015VBT.amount,qx2015VBT.policy),list(sum)) %>%
  mutate(atoe7580d = Death.Dollar/qx7580E.amount,
         atoe01VBTd = Death.Dollar/qx2001VBT.amount,
         atoe08VBTd = Death.Dollar/qx2008VBT.amount,
         atoe08VBTLUd = Death.Dollar/qx2008VBTLU.amount,
         atoe15VBTd = Death.Dollar/qx2015VBT.amount,
         atoe7580c = Deaths/qx7580E.policy,
         atoe01VBTc = Deaths/qx2001VBT.policy,
         atoe08VBTc = Deaths/qx2008VBT.policy,
         atoe08VBTLUc = Deaths/qx2008VBTLU.policy,
         atoe15VBTc = Deaths/qx2015VBT.policy) %>%
  select(age.basis,Gender,Val.Year,Deaths,face.band,atoe7580d,atoe01VBTd,atoe08VBTd,atoe08VBTLUd,atoe15VBTd,atoe7580c,atoe01VBTc,atoe08VBTc,atoe08VBTLUc,atoe15VBTc) %>%
  as.data.frame(data.mortality.faceband) %>%
  mutate(face.band = fct_relevel(face.band, 
                                 '1-9999','10000-24999','25000-49999','50000-99999','100000-249999',
                                 '250000-499999','500000-999999','1000000-2499999','2500000-4999999',
                                 '5000000-9999999'))

## CREATE atoe15VBTc2 for your heatmap color range - don't want extreme values to distort relative shading

data.everything <- data.everything %>%
  mutate(atoe15VBTc2 = ifelse(atoe15VBTc > 2,2,atoe15VBTc)) %>%
  mutate(atoe15VBTc2 = ifelse(atoe15VBTc2 < 0.5,0.5,atoe15VBTc2))

# new geometry for your heatmap
# added a layer to personalize the color scheme (not necessary)

all.ratio <- ggplot(data.everything,aes(x = face.band,y = Val.Year)) + 
  geom_tile(aes(fill = atoe15VBTc2,colour = "white")) + scale_fill_gradient(low = "white",high = "red") + 
  facet_grid(Gender ~ age.basis)+ geom_text(aes(label = round(atoe15VBTc,2))) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

all.deaths <- ggplot(data.everything,aes(x = face.band,y = Val.Year)) + 
  geom_tile(aes(fill = Deaths,colour = "white")) + scale_fill_gradient(low = "white",high = "red") + 
  facet_grid(Gender ~ age.basis)+ geom_text(aes(label = Deaths)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#################################################################################################
#######################   TABLE SELECTION - GATHER FUNCTION  ##################################

# Gather function

data.gather <- data.mortality %>%
  group_by(Val.Year) %>%
  summarise_at(vars(Deaths,Death.Dollar,qx7580E.amount,qx7580E.policy,qx2001VBT.amount,
                    qx2001VBT.policy,qx2008VBT.amount,qx2008VBT.policy,qx2008VBTLU.amount,
                    qx2008VBTLU.policy,qx2015VBT.amount,qx2015VBT.policy),list(sum)) %>%
  mutate(atoe7580d = Death.Dollar/qx7580E.amount,
         atoe01VBTd = Death.Dollar/qx2001VBT.amount,
         atoe08VBTd = Death.Dollar/qx2008VBT.amount,
         atoe08VBTLUd = Death.Dollar/qx2008VBTLU.amount,
         atoe15VBTd = Death.Dollar/qx2015VBT.amount,
         atoe7580c = Deaths/qx7580E.policy,
         atoe01VBTc = Deaths/qx2001VBT.policy,
         atoe08VBTc = Deaths/qx2008VBT.policy,
         atoe08VBTLUc = Deaths/qx2008VBTLU.policy,
         atoe15VBTc = Deaths/qx2015VBT.policy) %>%
  select(Val.Year,Deaths,atoe7580d,atoe01VBTd,atoe08VBTd,atoe08VBTLUd,atoe15VBTd,atoe7580c,atoe01VBTc,atoe08VBTc,atoe08VBTLUc,atoe15VBTc) %>%
  gather(table,a.to.e,atoe7580d:atoe15VBTc)



data.gather$a.to.e2 <- ifelse(data.gather$a.to.e > 2, 2, data.gather$a.to.e)

a.to.e_by.table <- ggplot(data.gather,aes(x = table,y = Val.Year)) + 
  geom_tile(aes(fill = a.to.e2,colour = "white")) + scale_fill_gradient(low = "white",high = "red")  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text(aes(label = round(a.to.e,2)))


###############################################  GRAPHS   ##################################################################################

Mortality.By.FaceBand
Deaths.By.FaceBand


Mortality.Val.Year
Deaths.Val.Year

Mortality.Gender
Deaths.Gender

Mortality.Age.Basis
Deaths.Age.Basis

all.deaths
all.ratio

a.to.e_by.table


