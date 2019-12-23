#PP1997B1. 11/2019
library(foreign); library(ggplot2); library(tidyverse); library(readstata13)
library(ggcorrplot); library(car)

dat <- read.dta13('./sav/PP1997B1.dta')
attach(dat)
influence<-rep(NA,1222)
influence[q38=="中國有非常大的影響力"]<-4
influence[q38=="中國比較有影響力"]<-3
influence[q38=="美國比較有影響力"]<-2
influence[q38=="美國有非常大的影響力"]<-1
#option are numeric
option2<-rep(NA,1222) #Is US or China relevant to Taiwan's economy
option2[q37=="與中國的關係非常重要"]<-5
option2[q37=="與中國的關係重要"]<-4
option2[q37=="都一樣"]<-3
option2[q37=="與美國的關係重要"]<-2
option2[q37=="與美國的關係非常重要"]<-1
option1<-rep(NA,1222) #Is US or China relevant to Taiwan's security
option1[q36=="與中國的關係非常重要"]<-5
option1[q36=="與中國的關係重要"]<-4
option1[q37=="都一樣"]<-3
option1[q36=="與美國的關係重要"]<-2
option1[q36=="與美國的關係非常重要"]<-1
#choice are factor
choice1<-rep(NA,1222) #Is US or China relevant to Taiwan's security
choice1[q36=="與中國的關係非常重要"]<-"China very important"
choice1[q36=="與中國的關係重要"]<-"China important"
choice1[q36=="與美國的關係重要"]<-"U.S. important"
choice1[q36=="與美國的關係非常重要"]<-"U.S. very important"
choice1[q36=="都一樣"]<-"Equally important"
choice2<-rep(NA,1222) #Is US or China relevant to Taiwan's economy
choice2[q37=="與中國的關係非常重要"]<-"China very important"
choice2[q37=="與中國的關係重要"]<-"China important"
choice2[q37=="與美國的關係重要"]<-"U.S. important"
choice2[q37=="與美國的關係非常重要"]<-"U.S. very important"
choice2[q37=="都一樣"]<-"Equally important"
choice1<-factor(choice1,levels=c("U.S. very important",
          "U.S. important","Equally important", "China important",
                "China very important"))
choice2<-factor(choice2,levels=c("U.S. very important","U.S. important",
               "Equally important", "China important",
                "China very important"))
#Economy or sovereignty
es<-rep(NA, 1222)
es[q34=='維持兩岸經貿發展']<-1
es[q34=='維護我們國家主權']<-0

#Feeling about China
feeling<-q35
feeling[q35=='非常喜歡']<-10
feeling[q35=='非常不喜歡']<-0
feeling[q35=='拒答']<-NA
feeling[q35=='不知道']<-NA
feeling<-as.numeric(feeling)

#Partisanship
kmt<-rep(0, 1222)
kmt[partyid2=='非常支持國民黨'] <- 3
kmt[partyid2=='普通支持國民黨'] <- 2
kmt[partyid2=='偏國民黨'] <- 1
dpp<-rep(0, 1222)
dpp[partyid2=='非常支持民進黨'] <- 3
dpp[partyid2=='普通支持民進黨'] <- 2
dpp[partyid2=='偏民進黨'] <- 1

panblue <- rep(0, 1222)
panblue[PID=='國民黨']<-1
panblue[PID=='親民黨']<-1
panblue[PID=='新黨']<-1
panblue[PID=='無黨團結聯盟']<-1
panblue[PID=='民國黨']<-1
panblue[PID=='無反應']<-NA
pangreen <- rep(0, 1222)
pangreen[PID=='民進黨']<-1
pangreen[PID=='綠黨']<-1
pangreen[PID=='基進黨']<-1
pangreen[PID=='社民黨']<-1
pangreen[PID=='一邊一國行動黨']<-1
pangreen[PID=='無反應']<-NA



#Independence/unification
uni<-rep(NA, 1222)
uni[q12=='儘快統一']<-6
uni[q12=='維持現狀，以後走向統一']<-5
uni[q12=='維持現狀，看情形再決定獨立或統一']<-4
uni[q12=='永遠維持現狀']<-3
uni[q12=='已經是獨立的國家']<-3
uni[q12=='維持現狀，以後走向獨立']<-2
uni[q12=='儘快獨立']<-1
#Tsai performance
Tsai <- rep(NA, 1222)
Tsai[q31=="非常滿意"]<-4
Tsai[q31=="有點滿意"]<-3
Tsai[q31=="不太滿意"]<-2
Tsai[q31=="非常不滿意"]<-1
#Gender
male.n <- as.factor(sex)

#Age
nq45 <- q45
nq45[q45=='拒答']<-NA

#Education
education<-rep(NA, 1222)
education[q46=='研究所及以上']<-7
education[q46=='大學']<-6
education[q46=='專科']<-5
education[q46=='高中、職']<-4
education[q46=='國、初中']<-3
education[q46=='小學']<-2
education[q46=='不識字及未入學']<-1
#US response
USintervene<-rep(NA, 1222)
USintervene[q39=='派兵協防台灣']<-1
USintervene[q39=='保持中立']<-0
USintervene[q39=='遲疑不決']<-0
#cross-Strait trade
trade <- rep(NA, 1222)
trade[q32=="更加開放"]<-1
trade[q32=="加強管制"]<-0

#create a dummy for priming of US intervention
US<-rep(0,1222)
US[nq1=='第三組：3a_10a3']<-1
US[nq1=='第九組：3b_10a3']<-1
US[nq1=='第六組：3a_10a6']<-1
US[nq1=='第十二組：3b_10a6']<-1

#create a dummy for priming of US conditional intervention
US.con<-rep(0,1222)
US.con[nq1=='第六組：3a_10a6']<-1
US.con[nq1=='第十二組：3b_10a6']<-1

#Binary security choice
china.security.5<-rep(NA,1222)
china.security.5[q36=='與美國的關係非常重要']<-0
china.security.5[q36=='與美國的關係重要']<-0
china.security.5[q36=='與中國的關係非常重要']<-1
china.security.5[q36=='與中國的關係重要']<-1
china.security.5[q36=='都一樣']<-0

us.security.5<-rep(NA,1222)
us.security.5[q36=='與美國的關係非常重要']<-1
us.security.5[q36=='與美國的關係重要']<-1
us.security.5[q36=='與中國的關係非常重要']<-0
us.security.5[q36=='與中國的關係重要']<-0
us.security.5[q36=='都一樣']<-0
age1<-rep(0, 1222)
age1[age=='20至29歲']<-1
age1[age=='30至39歲']<-1
age1[age=='無反應']<-NA
age2<-rep(0, 1222)
age2[age=='40至49歲']<-1
age2[age=='50至59歲']<-1
age2[age=='無反應']<-NA


save.image('PP1997B1.Rdata')
########################################################
newdat<-data.frame(option1, option2, influence, es, trade, feeling, 
        USintervene, Tsai, US, uni, kmt, dpp, male.n, nq45, education)
newdt<-na.omit(newdat)
newdt$agenew<-108 - as.numeric(newdt$nq45)

m1<-lm(option1 ~ influence + es + trade + feeling + US 
       +  uni + kmt + dpp 
       + male.n + agenew + education, data=newdt)
summary(m1)
m2<-lm(option2 ~ influence + es + trade + feeling + US 
       +uni + kmt + dpp
       + male.n + agenew + education, data=newdt)
summary(m2)
######################
#binary 
China.security<-recode(option1, "NA=0;3=1;4=1;2=0;1=0")


DAT<-data.frame(China.security, US, USintervene, kmt, dpp, 
                 male.n, nq45, education)
newDAT<-na.omit(DAT)
newDAT$age<-108 - as.numeric(newDAT$nq45)

ols1<-lm(USintervene ~  US + kmt + dpp + male.n + 
           age  + education , data=newDAT)
summary(ols1)

# 2nd binary #
DT<-data.frame(china.security.5, US, USintervene, kmt, dpp, 
                male.n, nq45, education)
newDT<-na.omit(DT)
newDT$age<-108 - as.numeric(newDT$nq45)

ols2<-lm(USintervene ~  US + kmt + dpp + male.n + 
           age  + education , data=newDT)
summary(ols2)

####### Alliance with US #######


  
DT1<-data.frame(US, USintervene, kmt, dpp, 
                male.n, age1, age2, education)
newDT1<-na.omit(DT1)

ols3<-lm(USintervene ~  US +  kmt + dpp  + male.n + 
           age1 + age2  + education , data=newDT1)
summary(ols3)

DT2<-data.frame(us.security.5, US, es, kmt, dpp, 
               feeling, male.n, age1, age2, education)
newDT2<-na.omit(DT2)

ols4.0<-glm(us.security.5 ~  US , 
          family=binomial('logit'), data=newDT2)
summary(ols4.0)

ols4<-glm(us.security.5 ~  US + es +  feeling + 
            kmt + dpp + male.n + age1 + age2  + education, 
          family=binomial('logit'), data=newDT2)
summary(ols4)



#Correlation

DT <-data.frame(Economy=choice1, Security=choice2)

DT %>%
  na.omit(DT) %>%
  ggplot(aes(x = Economy, fill = Security)) +
  geom_bar(position = "fill") +
  #facet_wrap(~cyl, labeller = label_both) +
  scale_y_continuous(name = "Within group Percentage"
                     , labels = scales::percent) 

