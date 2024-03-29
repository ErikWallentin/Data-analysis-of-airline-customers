#####################################################################################################
# I detta projekt �r m�let att analysera slumpdata fr�n flygprisj�mf�relsesajten X. Datamaterialet 
# presenteras i form av tv� Excel-filer d�r den f�rsta med namn "Feedback" bland annat inneh�ller 
# information om klagom�l fr�n kunder som rest med ett givet flygbolag, vilken tidpunkt klagom�let 
# mottagits samt om personen som klagat anv�nt prisj�mf�relsesajtens egna applikation eller inte n�r 
# klagom�let l�mnades.

# Den andra Excel-filen med namn "CoversionRate" inneh�ller information om hur m�nga personer som 
# bes�kt prisj�mf�relsesajten en given dag, hur m�nga bokningar som gjordes den dagen samt vilken 
# enhetstyp bes�karen anv�nt. 

# Med hj�lp av denna information �r m�let med projektet att besvara de tre nedanst�ende fr�gorna:

# I. Vilka klagom�l �r vanligast bland kunderna, och finns det n�gon skillnad p� vilken typ av klagom�l
# en kund l�mnar med avseende p� om kunden anv�nder prisj�mf�relsesajtens egna applikation eller inte?

# II. G�r det att urskilja ett samband mellan antal klagom�l och conversion rate en given dag f�r de 
# olika enhetstyperna? (conversion rate = antal bokningar dag x / antal bes�kare dag x).

# III. G�r det p� ett effektivt s�tt visualisera vilket flygbolag som �r associerad med en given
# typ av klagom�l?

# Projektet �r uppdelat i tre delar d�r datamaterialet genomg�r de n�dv�ndiga transformeringarna som 
# beh�vs f�r att besvara de givna fr�gorna. 
#####################################################################################################


################################
########### Fr�ga I ############
################################


# Importera excelfilen "Feedback" med hj�lp av paketet "XLConnect". 
install.packages("XLConnect")
library(XLConnect)

user_feedback <- loadWorkbook(file.choose(), create = T)
user_feedback <- readWorksheet(user_feedback,1, header = T)

# Innan analysfasen b�rjar �r det viktigt att leta efter outliers eller NULL-v�rden i datamaterialet
# som negativt kan p�verka analysen. Efter att user_feedback genoms�kts av bland annat summary()- samt
# plot()-funktionen hittas en kraftig outlier i kolumen "Final.Price".
# Nedan plottas denna outlier och sedan tas raden inneh�llande detta v�rde bort fr�n dataframen, 
# d� priset f�r denna tur-och-retur-biljett uppg�r till ca 1 123 393 svenska kronor. 
# (374 264 077 Colombian peso)
plot(user_feedback$Final.Price)
user_feedback <- user_feedback[-which(user_feedback$Final.Price==max(user_feedback$Final.Price)),]

# G�r s� att det endast st�r en typ av klagom�l under kolumnen "Feedback.Type" per rad med hj�lp av 
# paketet "tidyr". Inneh�ller en rad t.ex. tv� klagom�l skapas ist�llet tv� rader inneh�llandes 
# endast ett klagom�l per rad. Funktionen vet att den ska g�ra en uppdelning om tecknet "," f�ljt av
# mellanslag hittas i kolumnen "Feedback.Type".
install.packages("tidyr")
library(tidyr)

user_feedback <- separate_rows(user_feedback, Feedback.Type, sep = ",\\s")

# Plotta antalet klagom�l per kategori f�r de totalt 10 111 klagom�len.
barplot(sort(table(user_feedback$Feedback.Type), decreasing = T), 
        xlab = "Category", ylab = "Frequency", font.lab=2,
        main = paste("Total number of feedback per category.", 
                     "\n","A total of", nrow(user_feedback), "feedbacks were given."),
        ylim = c(0,3500), col = "grey")

# Redovisa antal klagom�l per kategori i R-konsolen.
sort(table(user_feedback$Feedback.Type), decreasing = T) 

# Plotta andelen klagom�l per kategori f�r de totalt 10 111 klagom�len.
barplot(round(prop.table(sort(table(user_feedback$Feedback.Type), decreasing = T)), 3),
        xlab = "Category", ylab = "Proportion", font.lab=2, 
        main = paste("Proportion of which feedback the customers gives.", 
                     "\n","A total of", nrow(user_feedback), "feedbacks were given."), 
        ylim = c(0,0.35), col = "grey")

# Redovisa antal klagom�l per kategori i R-konsolen.
round(prop.table(sort(table(user_feedback$Feedback.Type), decreasing = T)),3)

#####################################################################################################
# Vi har nu besvarat den f�rsta delen av fr�ga I, d� vi i grafen samt i R-konsolen kan utl�sa att
# det som folk klagar mest p� �r att priset inte �verensst�mmer, f�ljt av ov�ntade extrakostnader samt
# att hemsidan var sv�r att anv�nda. Nedan besvaras den andra delen av fr�ga I.
#####################################################################################################

# G�r s� att de tv� kategorierna "app_mobile" & "app_tablet" sl�s ihop under nament "App user", samt
# att de tre kategorierna "desktop", "mobile" & "tablet" sl�s ihop under namnet "Non app user".
# G�r sedan kolumnen som inneh�ller dessa tv� kategorier till en factor-kolumn.
user_feedback$Device.Type <- ifelse(user_feedback$Device.Type %in% c("app_mobile", "app_tablet"),
                                    "App user", "Non app user")
user_feedback$Device.Type <- as.factor(user_feedback$Device.Type)

# Ta reda p� vilken kategori av klagom�l som �r vanligast bland app anv�ndare.
FeedbackApp <- table(user_feedback[which(user_feedback$Device == "App user"),]$Feedback.Type)

barplot(FeedbackApp, xlab = "Category", ylab = "Frequency", font.lab=2,
        main = paste("Total number of feedback per category from customers using the app.", 
                     "\n","A total of", sum(FeedbackApp), "feedbacks were given."),
        ylim = c(0,2000), col = "grey")

# Ta reda p� vilken kategori av klagom�l som �r vanligast bland icke-app anv�ndare.
FeedbackNonApp <- table(user_feedback[which(user_feedback$Device == "Non app user"),]$Feedback.Type)

barplot(FeedbackNonApp, xlab = "Category", ylab = "Frequency", font.lab=2,
        main = paste("Total number of feedback per category from customers not using the app.", 
                     "\n","A total of", sum(FeedbackNonApp), "feedbacks were given."),
        ylim = c(0,2000), col = "blue")

# Redovisa andelen klagom�l f�r b�de app-anv�ndare & icke app-anv�ndare
FBboth <- rbind(prop.table(FeedbackApp), prop.table(FeedbackNonApp))
barplot(FBboth, beside = T, xlab = "Category", ylab = "Proportion", font.lab=2, ylim = c(0,0.40),
        main = "Proportion of which feedback the customers gives.", col = c("grey", "blue"))
legend("topright", legend = c("App user", "Non app user"), fill = c("grey", "blue"))

# Vi ser i plotten som redovisar andelen klagom�l f�r b�de app-anv�ndare & icke app-anv�ndare att
# klagom�l tenderar att skilja sig �t beroende p� om man anv�nder prisj�mf�relsesajtens applikation
# eller inte. F�r att bekr�fta alternativt dementera denna tes utf�rs nedan ett statistiskt test i 
# form av ett chi-tv� test.

# Nollhypotes (Ho): Anv�ndning av app eller inte app har ingen koppling till vilken typ av klagom�l
# man ger.
# Alternativhypotes (Ha): Anv�ndning av app eller inte app  har en koppling till vilken typ av
# klagom�l man ger.

FBboth2 <- rbind(FeedbackApp, FeedbackNonApp) 
chisq.test(FBboth2)

# X-squared = 149.28 & p-v�rde < 2.2e-16. Ho f�rkastas p� v�ldigt l�g signifikansniv� d� X-squared 
# samt p-v�rdet �r v�ldigt stort respektive litet. 
# (p-v�rde = sannolikheten att teststatistikan X-squared antar uppn�t eller ett extremare v�rde 
# givet att nollhypotesen verkligen �r sann).

# Vi har statistisk bevis att klagom�len skiljer sig �t mellan app-anv�ndare och icke app-anv�ndare.
# De tv� grupperna tenderar s�ledes att ge olika typ av klagom�l.


################################
########### Fr�ga II ###########
################################


# M�let med denna uppgift �r att r�kna ut korrelationen mellan klagom�l och conversion rate f�r de tv�
# grupperna data-anv�ndare samt telefon-anv�ndare. F�r att r�kna ut denna korrelation m�ste
# datamaterialet f�rst f�rberedas.

# Importera excelfilen "CoversionRate" med hj�lp av paketet "XLConnect". 
coversion_rate <- loadWorkbook(file.choose(), create = T)
coversion_rate <- readWorksheet(coversion_rate,1, header = T)

# (Efter genomg�ng av coversion_rate hittades inga outliers eller NULL-v�rden p� kritiska platser).

# F�r att besvara fr�ga tv� m�ste vi ha tillg�ng till ursprungsv�rderna i kolumnen "Device.Type" i 
# dataframen "user_feedback". Vi importerar d�rf�r ursprungsversionen, tar bort outliern och utf�r 
# sedan samma operation s� att det inte finns mer �n ett klagom�l per rad.
user_feedback2 <- loadWorkbook(file.choose(), create = T)
user_feedback2 <- readWorksheet(user_feedback2, 1, header = T)

user_feedback2 <- user_feedback2[-which(user_feedback2$Final.Price==max(user_feedback2$Final.Price)),]

user_feedback2 <- separate_rows(user_feedback2, Feedback.Type, sep = ",\\s")

# G�r s� att datumen sorteras p� samma s�tt i de tv� nyskapde dataframen.
user_feedback2 <- user_feedback2[order(user_feedback2$Feedback.Date),]

# Unders�k om de tv� dataframen inneh�ller lika m�nga unika datum.
length(unique(user_feedback2$Feedback.Date)) # 92 unika datum.
length(unique(coversion_rate$Order.Date)) # 93 unika datum.
# I "user_feedback2" har vi data med startdatum "2018-09-10" och slutdatum "2018-12-10".
# I "coversion_rate" har vi data med startdatum "2018-09-09" och slutdatum "2018-12-10".

# Ta bort data fr�n datumet "2018-09-09" i dataframen "coversion_rate".
Dates <- unique(user_feedback2$Feedback.Date)
coversion_rate <- coversion_rate[coversion_rate$Order.Date %in% Dates,]

# R�kna ut conversion rate (antal bokningar / antal sidbes�k) och l�gg till resultatet i en kolumn i
# dataframen "coversion_rate".
coversion_rate$'Conversion rate' <- round(coversion_rate$Bookings / coversion_rate$Entries, 5)

# Vid inspektion av resultatet efter ovanst�ende utr�kning, ser vi nedan att tre v�rden f�r 
# conversion rate �verstiger v�rdet 1. Raderna inneh�llande dessa v�rden stannar dock kvar i 
# dataframen d� en person kan boka mer �n en resa per sid-bes�k.
sort(coversion_rate$'Conversion rate', decreasing = T)

# Nedan skapas fyra mindre dataframes, inneh�llandes informationen som beh�vs f�r att r�kna ut
# korrelationen mellan klagom�l och conversion rate f�r de tv� grupperna data-anv�ndare samt 
# telefon-anv�ndare.
user_feedbackComputer <- subset(user_feedback2[,c(1,4,14)], user_feedback2$Device.Type == "desktop")
user_feedbackPhone <- subset(user_feedback2[,c(1,4,14)], user_feedback2$Device.Type == "app_mobile" | user_feedback2$Device.Type == "mobile")

coversion_rateComputer <- subset(coversion_rate[,c(1:3,6)], coversion_rate$DeviceType == "Desktop Computer")
coversion_ratePhone <- subset(coversion_rate[,c(1:3,6)], coversion_rate$DeviceType == "Phone")

# Unders�k hur m�nga unika datum som finns i v�ra dataframes. Detta �r viktigt eftersom vi beh�ver 
# kunna koppla ihop hur m�nga klagom�l samt conversion rate en specifik dag har.
length(unique(user_feedbackComputer$Feedback.Date)) # 39
length(unique(coversion_rateComputer$Order.Date))   # 92

length(unique(user_feedbackPhone$Feedback.Date)) # 92
length(unique(coversion_ratePhone$Order.Date))   # 92

# Vi har endast data �ver folk som l�mnade klagom�l fr�n en dator f�r 39 dagar. Efter inspektion visar
# det sig att data �ver klagom�l f�r dataanv�ndande b�rjade samlas in f�rst "2018-11-02". 
# Detta f�rklarra varf�r vi endast har 39 unika datum i "user_feedbackComputer".

# Trimma dataframen "coversion_rateComputer" s� den endast inneh�ller samma datum som i dataframen
# "user_feedbackComputer".
Dates2 <- unique(user_feedbackComputer$Feedback.Date)
coversion_rateComputer <- coversion_rateComputer[coversion_rateComputer$Order.Date %in% Dates2,]

# Ber�kna medelv�rdet av conversion rate per datum f�r de tv� olika kategorierna data- samt telefon-anv�ndare.
meanCoversion_rateComputer <- coversion_rateComputer$'Conversion rate'
meanCoversion_ratePhone <- as.vector(round(tapply(coversion_ratePhone$'Conversion rate', coversion_ratePhone$Order.Date, mean),5))

# Ber�kna antal klagom�l per datum f�r de tv� olika kategorierna data- samt telefon-anv�ndare.
totUser_feedbackComputer <- as.vector(tapply(user_feedbackComputer$Feedback.Type, user_feedbackComputer$Feedback.Date,  function(x) length(x)))
totUser_feedbackPhone <- as.vector(tapply(user_feedbackPhone$Feedback.Type, user_feedbackPhone$Feedback.Date, function(x) length(x)))

# Skapa en scatterplot samt ber�kna korrelationen f�r att se relationen mellan de tv� variablerna 
# klagom�l och conversion rate f�r dataanv�ndare.
plot(meanCoversion_rateComputer, totUser_feedbackComputer, 
     xlab = "Mean convention rate for computer-users",
     ylab = "Total feedback from people using computers", font.lab = 2, 
     main = paste("The correlation beetwen the two variables is",round(cor(meanCoversion_rateComputer, totUser_feedbackComputer),5)))

cor(meanCoversion_rateComputer, totUser_feedbackComputer) # -0.2436917

# Skapa en scatterplot samt ber�kna korrelatioen f�r att se relationen mellan de tv� variablerna 
# klagom�l och conversion rate f�r telefonanv�ndare.
plot(meanCoversion_ratePhone, totUser_feedbackPhone,
     xlab = "Mean convention rate for phone-users",
     ylab = "Total feedback from people using phones", font.lab = 2,
     main = paste("The correlation beetwen the two variables is",round(cor(meanCoversion_ratePhone, totUser_feedbackPhone),5)))

cor(meanCoversion_ratePhone, totUser_feedbackPhone) # 0.0572671

# F�r dataanv�ndare finner vi en liten negativ korrelation, vilket inneb�r att om conversion rate g�r
# upp, tenderar klagom�len att minska lite och vice versa. F�r telefonanv�ndare kan vi n�stan inte 
# utfinna n�gon korrelation alls mellan de tv� variablerna, vilket inneb�r att relationen mellan
# dessa tv� variabler �r svag. Notera att de tre dagarna d�r conversion rate �versteg 1 tydligt kan 
# urtydas i plotten. Hade vi tagit bort dessa tre dagar hade korrelationen blivit �nnu mindre.

# Avslutnignsvis unders�ks om den l�ga korrelationen mellan klagom�l och conversion rate f�r 
# telefonanv�ndare kan f�rklaras av att en l�g andel anv�ndare av t.ex. BlackBerry och/eller 
# Windows Phone bokar biljetter n�r de bes�ker prisj�mf�relsesajten.
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "BlackBerry"),]$'Conversion rate') # mean = 0
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "Windows Phone"),]$'Conversion rate') # mean = 0.02933
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "iPod"),]$'Conversion rate') # mean = 0.1235
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "iPhone"),]$'Conversion rate') # mean = 0.1708
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "Android Phone"),]$'Conversion rate') # mean = 0.12645

# Skulle korrelationen mellan klagom�l och convention rate f�r iPhone- samt Android-anv�ndare r�knas ut
# skulle korrelationen s�kerligen bli st�rre mellan de tv� variablerna, vilket bekr�ftas nedan.
coversion_rateIphoneAndroid <- subset(coversion_ratePhone, coversion_ratePhone$Devices == "Android Phone" | coversion_ratePhone$Devices == "iPhone")

length(unique(coversion_rateIphoneAndroid$Order.Date)) # 92

meanCoversion_rateIphoneAndroid <- as.vector(round(tapply(coversion_rateIphoneAndroid$'Conversion rate', coversion_rateIphoneAndroid$Order.Date, mean),5))

plot(meanCoversion_rateIphoneAndroid, totUser_feedbackPhone,
     xlab = "Mean convention rate for iPhone or Android users",
     ylab = "Total feedback from people using iPhone or Android", font.lab = 2,
     main = paste("The correlation beetwen the two variables is",round(cor(meanCoversion_rateIphoneAndroid, totUser_feedbackPhone),5)))

cor(meanCoversion_rateIphoneAndroid, totUser_feedbackPhone) # -0.3464306

# Vi uppn�r nu den st�rsta sambandet/korrelationen hittills mellan de tv� variablerna, vilket inneb�r 
# att om conversion rate g�r upp, tenderar klagom�len att i �nnu st�rre grad minska och vice versa.


################################
########## Fr�ga III ###########
################################


# I fr�ga I redovisades vilken typ av klagom�l som var vanligast bland kunderna. Vad som dock inte 
# framg�r i svaret �r vilka flygbolag som �r associerade med vilken typ av klagom�l samt vilka som f�tt 
# mest. Den informationen �r viktig att ha tillg�ng till s� att t.ex. ett flygbolag som f�r mycket 
# klagom�l om att deras pris inte �verensst�mde med det f�rv�ntade, kan ta �t sig av denna kritik och
# �tg�rda detta problem f�r att f�rb�ttra kundupplevelsen.

# Nedan skapas d�rf�r en funktion som returnerar en dataframe inneh�llandes information om hur mycket 
# av en viss typ av klagom�l ett givet flygbolag f�tt. Funktionen �r flexibel i den m�n att den kan 
# hantera att ny data om klagom�l kommer in. Det enda som d� beh�ver g�ras �r att importera 
# datamaterialet till variabeln "user_feedback3" med hj�lp av paketet "XLConnect".

# Importera excelfilen "user_feedback" med hj�lp av paketet "XLConnect" och ta bort outliern 
user_feedback3 <- loadWorkbook(file.choose(), create = T)
user_feedback3 <- readWorksheet(user_feedback3,1, header = T)
user_feedback3 <- user_feedback3[-which(user_feedback3$Final.Price==max(user_feedback3$Final.Price)),]

# Visa v�rden s�som "1e-04" som "0.0001".
options(scipen=999)

# Skapa funktionen som returnerar en dataframe inneh�llande information om vilka flygbolag som �r 
# associerade med vilken typ av klagom�l samt vilka som f�tt mest. 
# Anropet till funktioner hittas l�ngst ner i koden.
GetFeedback <- function(){
  
  # G�r s� att varje rad endast inneh�ller ett flygbolagsnamn under kolumnen "Carrier.Name" samt ett 
  # klagom�l under kolumnen "Feedack.Type". Inneh�ller en rad t.ex. tv� olika flygbolagnamn skapas tv� 
  # rader inneh�llandes endast ett flygbolagsnamn per rad.
  user_feedback3 <- separate_rows(user_feedback3, Carrier.Name, sep = ",\\s")
  user_feedback3 <- separate_rows(user_feedback3, Feedback.Type, sep = ",\\s")
  
  # H�mta information om hur mycket klagom�l av en given typ flygbolag x f�tt.
  'Flight Not Available' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Flight Not Available"),]$Carrier.Name))
  'Other issues' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Other issues"),]$Carrier.Name))
  'Prices did not match' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Prices didn't match"),]$Carrier.Name))
  'Site hard to use' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Site hard to use"),]$Carrier.Name))
  'Unexpected extras' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Unexpected extras"),]$Carrier.Name))
  
  # H�mta namnen p� flygbolagen som mottagit minst ett klagom�l. Spara dessa namn i en kolum av typen 
  # character i en dataframe vid namn "Feedback".
  Name <- sort(unique(user_feedback3$Carrier.Name))
  Feedback <- as.data.frame(Name)
  Feedback$Name <- as.character(Feedback$Name)
  
  # Skapa en vector inneh�llandes lika m�nga nollor som antalet rader i dataframen "Feedback".
  zero <- rep(0, length(Feedback))
  
  # Nedan byggs dataframen "Feedback" ut genom att addera totala antalet klagom�l ett givet flygbolag 
  # f�tt i de fem olika klagom�ls-kategorierna. Den f�rsta for-each loopen nedan anv�nder sig av
  # dataframen "Flight Not Available" f�r att ta reda p� namnet p� det f�rsta flygbolaget som mottagit 
  # minst ett klagom�l av kategorin "Flight Not Available". F�r att placera v�rdet, dvs antal klagom�l
  # av typen "Flight Not Available" f�r flygbolag x, p� r�tt plats i dataframen "Feedback" letas raden 
  # d�r kolumn ett i "Feedback" �r lika med just det flygbolagsnamnet. Denna procedur repeteras sedan 
  # f�r de fyra �vriga klagom�ls-kategorierna.
  # Slutligen skapas tv� kolumner inneh�llandes total antal klagom�l, respektive hur stor andel av de 
  # totala antalet klagom�l det givna flygbolaget mottagit.
  
  # Fixa andra kolumnen "Flight Not Available".
  Feedback$'Flight Not Available' <- zero
  
  `Flight Not Available`$Var1 <- as.character(`Flight Not Available`$Var1)
  
  for(d in 1:nrow(`Flight Not Available`)){
    
    Feedback[which(Feedback$Name ==`Flight Not Available`[d,1]),2] <- `Flight Not Available`[d,2]
  }
  
  # Fixa tredje kolumnen "Other issues".
  Feedback$'Other issues' <- zero
  
  `Other issues`$Var1 <- as.character(`Other issues`$Var1)
  
  for(d in 1:nrow(`Other issues`)){
    
    Feedback[which(Feedback$Name ==`Other issues`[d,1]),3] <- `Other issues`[d,2]
    
  }
  
  # Fixa fj�rde kolumnen "Prices did not match".
  Feedback$'Prices did not match' <- zero
  
  `Prices did not match`$Var1 <- as.character(`Prices did not match`$Var1)
  
  for(d in 1:nrow(`Prices did not match`)){
    
    Feedback[which(Feedback$Name ==`Prices did not match`[d,1]),4] <- `Prices did not match`[d,2]
    
  }
  
  # Fixa femte kolumnen "Site hard to use".
  Feedback$'Site hard to use' <- zero
  
  `Site hard to use`$Var1 <- as.character(`Site hard to use`$Var1)
  
  for(d in 1:nrow(`Site hard to use`)){
    
    Feedback[which(Feedback$Name ==`Site hard to use`[d,1]),5] <- `Site hard to use`[d,2]
    
  }
  
  # Fixa sj�tte kolumnen "Unexpected extras".
  Feedback$'Unexpected extras' <- zero
  
  `Unexpected extras`$Var1 <- as.character(`Unexpected extras`$Var1)
  
  for(d in 1:nrow(`Unexpected extras`)){
    
    Feedback[which(Feedback$Name ==`Unexpected extras`[d,1]),6] <- `Unexpected extras`[d,2]
    
  }
  
  # Fixa sjunde kolumnen "Total feedback".
  Feedback$'Total feedback' <- rowSums(Feedback[,2:6])
  
  # Fixa �ttonde kolumnen "Proportion of the total feedback".
  Feedback$'Proportion of the total feedback' <- round(prop.table(Feedback$`Total feedback`),5)
  
  # Returnera den resulterande dataframen.
  return(View(Feedback))
}

# Funktionsanrop.
GetFeedback()

# Vi ser att det flybolag som mottagit mest klagom�l �ver att deras pris inte �verensst�mde med det 
# f�rv�ntade �r "Emirates" f�ljt av "Virgin Atlantic".