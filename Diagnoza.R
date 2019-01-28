library(Diagnoza)

library(ggplot2)
library(weights)

library(devtools)
library(dplyr)
library(scales)

data("gospodarstwa")
data("gospodarstwaDict")
data("osoby")
data("osobyDict")

load("C:\\Users\\anial\\Downloads\\Diagnoza-master\\Diagnoza-master\\data\\osobyDict.rda")
load("C:\\Users\\anial\\Downloads\\Diagnoza-master\\Diagnoza-master\\data\\osoby.rda")
View(osobyDict)

#-----------------------------------DANE---------------------------------------------

#mezczyzna 1, kobieta 2.
#tak 1, nie 2
o_20<-osoby[,c('waga_2000_osoby','ap77_10','plec_all')] #zadowolenie ze swoich osiÄ…gniÄ™Ä‡ Å¼yciowych
colnames(o_20)<-c("waga","osiagniecia","plec")
o_20$rok<-"2000"
j_20<-osoby[,c('waga_2000_osoby','ap71','wiek2000')] #Jak zyje, zalezy ode mnie 2000
colnames(j_20)<-c("waga","ja","wiek")
j_20$rok<-"2000"
j_29<-osoby[,c('waga_2009_osoby','ep71','wiek2009')] #Jak zyje, zalezy ode mnie 2009
colnames(j_29)<-c("waga","ja","wiek")
j_29$rok<-"2009"

df_wykres<-rbind(j_20,j_29)
df_wykres<-na.omit(df_wykres)

dane<-cbind(j_20, j_29)
dane<-na.omit(dane)
colnames(dane)<-c("waga_00","ja_00","wiek_00","rok","waga_09","ja_09","wiek_09","rok")

dane2<-cbind(o_20$waga,j_20$ja, o_20$osiagniecia, o_20$plec)
dane2<-na.omit(dane2)
colnames(dane2)<-c("waga","ja", "osiagniecia","plec")
dane2<-as.data.frame(dane2)

kobiety<-dane2[dane2$plec==2,]
mezczyzni<-dane2[dane2$plec==1,]


#------------------------------JEDEN FAJNY WYKRES---------------------------------------

ggplot(dane2, aes(x= osiagniecia, y = waga, fill=as.factor(plec)))+
  labs(title="zadowolenie ze swoich osiagniec zyciowych w kobiet i u mezczyzn, \njesli odpowiedzieli ze odpowiadaja za swoje zycie")+
  theme(legend.position="top") + 
  geom_bar( stat="identity", position="dodge") +  facet_wrap(~ja) +
  scale_fill_brewer(name="Legenda",
                    breaks=c("1","2"),
                    labels=c("mezczyzna","kobieta"),
                    palette = "Reds")

#-----------------------------------RZECZY-------------------------------------------

###################################################################################################
#H0: srednie poczucie posiadania wp³ywu na swoje ¿yce jest takie samo u na przeciagu lat.
#Ha: srednie poczucie posiadania wp³ywu na swoje ¿yce jest ró¿ne na przeciagu lat
weights::wtd.chi.sq(as.numeric(dane$ja_00),as.numeric(dane$ja_09), 
                    weight = as.numeric(dane$ja_09))
#p.value 0.78553910  > .05 --> nie mo¿emy odrzucic H0

wtd.hist(dane$ja_00, weight = dane$waga_00)
wtd.hist(dane$ja_09, weight = dane$waga_09)

ggplot(df_wykres, aes(x=ja, y = waga))+
  scale_fill_brewer(palette = "Reds") +
  ggtitle("poczucie posiadania wplywu na to co dzieje sie w moim zyciu w zaleznosci od roku")+
  geom_bar(stat = "identity", aes(fill = rok), position = "dodge")


###################################################################################################
#H0: w obu latach w badaniu wziê³y udzia³ osoby w tym samym wieku
#HA: osoby bior¹ce udzia³ w badaniu ró¿ni³y siê wiekiem
wynik1<-wtd.t.test(x=dane$wiek_00, y=dane$wiek_09, weight = dane$waga_00, weighty = dane$waga_09)
wynik1
#p.value 0  < .1 --> odrzucamy H0

wtd.hist(dane$wiek_00, weight = dane$waga_00)
wtd.hist(dane$wiek_09, weight = dane$waga_09)

###################################################################################################
#H0: srednie zadowolenie ze swoich osiagniec zyciowych jest takie samo u kobiet i u mezczyzn.
#Ha: srednie zadowolenie ze swoich osiagniec zyciowych jest wiêksze u kobiet ni¿ u mê¿czyzn
wtd.t.test(x=as.numeric(kobiety$osiagniecia), y=as.numeric(mezczyzni$osiagniecia) ,
           weight = as.numeric(kobiety$waga) ,weighty = as.numeric(mezczyzni$waga), 
           alternative = "greater")
#p.value 0.2615000  > .1 --> nie mo¿emy odrzucic H0

wtd.hist(kobiety$osiagniecia, weight = kobiety$waga)
wtd.hist(mezczyzni$osiagniecia, weight = mezczyzni$waga)

a1<-wtd.table(kobiety$osiagniecia, weights = kobiety$waga)
a2<-wtd.table(mezczyzni$osiagniecia, weights = mezczyzni$waga)
cbind(a1$sum.of.weights,a2$sum.of.weights)->temp
tmp<-as.data.frame(temp)
hist(x=tmp)

ggplot(dane2, aes(osiagniecia, y = ..count.. ))+
  labs(title="zadowolenie ze swoich osiagniec zyciowych w kobiet i u mezczyzn")+
  geom_histogram( aes(fill = as.factor(plec)),
            binwidth = .5,
            position = "dodge")+
  scale_fill_brewer(name="Legenda",
                      breaks=c("1","2"),
                      labels=c("mezczyzna","kobieta"),
                      palette = "Reds")


###################################################################################################
#H0: srednie poczucie posiadania wp³ywu na swoje ¿yce jest takie samo u kobiet i u mezczyzn.
#Ha: srednie poczucie posiadania wp³ywu na swoje ¿yce jest ró¿ne u kobiet i u mezczyzn.
weights::wtd.chi.sq(as.numeric(kobiety$ja),as.numeric(mezczyzni$ja), 
                    weight = as.numeric(mezczyzni$waga))
#p.value 0.3991502   > .05 --> nie mo¿emy odrzucic H0
wtd.hist(kobiety$ja, weight = kobiety$waga)
wtd.hist(mezczyzni$ja, weight = mezczyzni$waga)

b1<-wtd.table(kobiety$ja, weights = kobiety$waga)
b2<-wtd.table(mezczyzni$ja, weights = mezczyzni$waga)
rbind(b1$sum.of.weights,b2$sum.of.weights)->temp1
hist(temp1)

ggplot(dane2, aes(ja, y = ..count.. ))+
  labs(title="poczucie posiadania wplywu na to co dzieje sie w moim zyciu w zaleznosci od plci",
       x = "tak                                                                                                     nie")+
  geom_bar(stat = "identity",
           aes(fill = as.factor(plec)),
                  binwidth = .5,
                  position = "dodge")+
  scale_fill_brewer(name="Legenda",
                    breaks=c("1","2"),
                    labels=c("mezczyzna","kobieta"),
                    palette = "Reds")



