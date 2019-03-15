#####################################################################################################
# I detta projekt är målet att analysera slumpdata från flygprisjämförelsesajten X. Datamaterialet 
# presenteras i form av två Excel-filer där den första med namn "Feedback" bland annat innehåller 
# information om klagomål från kunder som rest med ett givet flygbolag, vilken tidpunkt klagomålet 
# mottagits samt om personen som klagat använt prisjämförelsesajtens egna applikation eller inte när 
# klagomålet lämnades.

# Den andra Excel-filen med namn "CoversionRate" innehåller information om hur många personer som 
# besökt prisjämförelsesajten en given dag, hur många bokningar som gjordes den dagen samt vilken 
# enhetstyp besökaren använt. 

# Med hjälp av denna information är målet med projektet att besvara de tre nedanstående frågorna:

# I. Vilka klagomål är vanligast bland kunderna, och finns det någon skillnad på vilken typ av klagomål
# en kund lämnar med avseende på om kunden använder prisjämförelsesajtens egna applikation eller inte?

# II. Går det att urskilja ett samband mellan antal klagomål och conversion rate en given dag för de 
# olika enhetstyperna? (conversion rate = antal bokningar dag x / antal besökare dag x).

# III. Går det på ett effektivt sätt visualisera vilket flygbolag som är associerad med en given
# typ av klagomål?

# Projektet är uppdelat i tre delar där datamaterialet genomgår de nödvändiga transformeringarna som 
# behövs för att besvara de givna frågorna. 
#####################################################################################################


################################
########### Fråga I ############
################################


# Importera excelfilen "Feedback" med hjälp av paketet "XLConnect". 
install.packages("XLConnect")
library(XLConnect)

user_feedback <- loadWorkbook(file.choose(), create = T)
user_feedback <- readWorksheet(user_feedback,1, header = T)

# Innan analysfasen börjar är det viktigt att leta efter outliers eller NULL-värden i datamaterialet
# som negativt kan påverka analysen. Efter att user_feedback genomsökts av bland annat summary()- samt
# plot()-funktionen hittas en kraftig outlier i kolumen "Final.Price".
# Nedan plottas denna outlier och sedan tas raden innehållande detta värde bort från dataframen, 
# då priset för denna tur-och-retur-biljett uppgår till ca 1 123 393 svenska kronor. 
# (374 264 077 Colombian peso)
plot(user_feedback$Final.Price)
user_feedback <- user_feedback[-which(user_feedback$Final.Price==max(user_feedback$Final.Price)),]

# Gör så att det endast står en typ av klagomål under kolumnen "Feedback.Type" per rad med hjälp av 
# paketet "tidyr". Innehåller en rad t.ex. två klagomål skapas istället två rader innehållandes 
# endast ett klagomål per rad. Funktionen vet att den ska göra en uppdelning om tecknet "," följt av
# mellanslag hittas i kolumnen "Feedback.Type".
install.packages("tidyr")
library(tidyr)

user_feedback <- separate_rows(user_feedback, Feedback.Type, sep = ",\\s")

# Plotta antalet klagomål per kategori för de totalt 10 111 klagomålen.
barplot(sort(table(user_feedback$Feedback.Type), decreasing = T), 
        xlab = "Category", ylab = "Frequency", font.lab=2,
        main = paste("Total number of feedback per category.", 
                     "\n","A total of", nrow(user_feedback), "feedbacks were given."),
        ylim = c(0,3500), col = "grey")

# Redovisa antal klagomål per kategori i R-konsolen.
sort(table(user_feedback$Feedback.Type), decreasing = T) 

# Plotta andelen klagomål per kategori för de totalt 10 111 klagomålen.
barplot(round(prop.table(sort(table(user_feedback$Feedback.Type), decreasing = T)), 3),
        xlab = "Category", ylab = "Proportion", font.lab=2, 
        main = paste("Proportion of which feedback the customers gives.", 
                     "\n","A total of", nrow(user_feedback), "feedbacks were given."), 
        ylim = c(0,0.35), col = "grey")

# Redovisa antal klagomål per kategori i R-konsolen.
round(prop.table(sort(table(user_feedback$Feedback.Type), decreasing = T)),3)

#####################################################################################################
# Vi har nu besvarat den första delen av fråga I, då vi i grafen samt i R-konsolen kan utläsa att
# det som folk klagar mest på är att priset inte överensstämmer, följt av oväntade extrakostnader samt
# att hemsidan var svår att använda. Nedan besvaras den andra delen av fråga I.
#####################################################################################################

# Gör så att de två kategorierna "app_mobile" & "app_tablet" slås ihop under nament "App user", samt
# att de tre kategorierna "desktop", "mobile" & "tablet" slås ihop under namnet "Non app user".
# Gör sedan kolumnen som innehåller dessa två kategorier till en factor-kolumn.
user_feedback$Device.Type <- ifelse(user_feedback$Device.Type %in% c("app_mobile", "app_tablet"),
                                    "App user", "Non app user")
user_feedback$Device.Type <- as.factor(user_feedback$Device.Type)

# Ta reda på vilken kategori av klagomål som är vanligast bland app användare.
FeedbackApp <- table(user_feedback[which(user_feedback$Device == "App user"),]$Feedback.Type)

barplot(FeedbackApp, xlab = "Category", ylab = "Frequency", font.lab=2,
        main = paste("Total number of feedback per category from customers using the app.", 
                     "\n","A total of", sum(FeedbackApp), "feedbacks were given."),
        ylim = c(0,2000), col = "grey")

# Ta reda på vilken kategori av klagomål som är vanligast bland icke-app användare.
FeedbackNonApp <- table(user_feedback[which(user_feedback$Device == "Non app user"),]$Feedback.Type)

barplot(FeedbackNonApp, xlab = "Category", ylab = "Frequency", font.lab=2,
        main = paste("Total number of feedback per category from customers not using the app.", 
                     "\n","A total of", sum(FeedbackNonApp), "feedbacks were given."),
        ylim = c(0,2000), col = "blue")

# Redovisa andelen klagomål för både app-användare & icke app-användare
FBboth <- rbind(prop.table(FeedbackApp), prop.table(FeedbackNonApp))
barplot(FBboth, beside = T, xlab = "Category", ylab = "Proportion", font.lab=2, ylim = c(0,0.40),
        main = "Proportion of which feedback the customers gives.", col = c("grey", "blue"))
legend("topright", legend = c("App user", "Non app user"), fill = c("grey", "blue"))

# Vi ser i plotten som redovisar andelen klagomål för både app-användare & icke app-användare att
# klagomål tenderar att skilja sig åt beroende på om man använder prisjämförelsesajtens applikation
# eller inte. För att bekräfta alternativt dementera denna tes utförs nedan ett statistiskt test i 
# form av ett chi-två test.

# Nollhypotes (Ho): Användning av app eller inte app har ingen koppling till vilken typ av klagomål
# man ger.
# Alternativhypotes (Ha): Användning av app eller inte app  har en koppling till vilken typ av
# klagomål man ger.

FBboth2 <- rbind(FeedbackApp, FeedbackNonApp) 
chisq.test(FBboth2)

# X-squared = 149.28 & p-värde < 2.2e-16. Ho förkastas på väldigt låg signifikansnivå då X-squared 
# samt p-värdet är väldigt stort respektive litet. 
# (p-värde = sannolikheten att teststatistikan X-squared antar uppnåt eller ett extremare värde 
# givet att nollhypotesen verkligen är sann).

# Vi har statistisk bevis att klagomålen skiljer sig åt mellan app-användare och icke app-användare.
# De två grupperna tenderar således att ge olika typ av klagomål.


################################
########### Fråga II ###########
################################


# Målet med denna uppgift är att räkna ut korrelationen mellan klagomål och conversion rate för de två
# grupperna data-användare samt telefon-användare. För att räkna ut denna korrelation måste
# datamaterialet först förberedas.

# Importera excelfilen "CoversionRate" med hjälp av paketet "XLConnect". 
coversion_rate <- loadWorkbook(file.choose(), create = T)
coversion_rate <- readWorksheet(coversion_rate,1, header = T)

# (Efter genomgång av coversion_rate hittades inga outliers eller NULL-värden på kritiska platser).

# För att besvara fråga två måste vi ha tillgång till ursprungsvärderna i kolumnen "Device.Type" i 
# dataframen "user_feedback". Vi importerar därför ursprungsversionen, tar bort outliern och utför 
# sedan samma operation så att det inte finns mer än ett klagomål per rad.
user_feedback2 <- loadWorkbook(file.choose(), create = T)
user_feedback2 <- readWorksheet(user_feedback2, 1, header = T)

user_feedback2 <- user_feedback2[-which(user_feedback2$Final.Price==max(user_feedback2$Final.Price)),]

user_feedback2 <- separate_rows(user_feedback2, Feedback.Type, sep = ",\\s")

# Gör så att datumen sorteras på samma sätt i de två nyskapde dataframen.
user_feedback2 <- user_feedback2[order(user_feedback2$Feedback.Date),]

# Undersök om de två dataframen innehåller lika många unika datum.
length(unique(user_feedback2$Feedback.Date)) # 92 unika datum.
length(unique(coversion_rate$Order.Date)) # 93 unika datum.
# I "user_feedback2" har vi data med startdatum "2018-09-10" och slutdatum "2018-12-10".
# I "coversion_rate" har vi data med startdatum "2018-09-09" och slutdatum "2018-12-10".

# Ta bort data från datumet "2018-09-09" i dataframen "coversion_rate".
Dates <- unique(user_feedback2$Feedback.Date)
coversion_rate <- coversion_rate[coversion_rate$Order.Date %in% Dates,]

# Räkna ut conversion rate (antal bokningar / antal sidbesök) och lägg till resultatet i en kolumn i
# dataframen "coversion_rate".
coversion_rate$'Conversion rate' <- round(coversion_rate$Bookings / coversion_rate$Entries, 5)

# Vid inspektion av resultatet efter ovanstående uträkning, ser vi nedan att tre värden för 
# conversion rate överstiger värdet 1. Raderna innehållande dessa värden stannar dock kvar i 
# dataframen då en person kan boka mer än en resa per sid-besök.
sort(coversion_rate$'Conversion rate', decreasing = T)

# Nedan skapas fyra mindre dataframes, innehållandes informationen som behövs för att räkna ut
# korrelationen mellan klagomål och conversion rate för de två grupperna data-användare samt 
# telefon-användare.
user_feedbackComputer <- subset(user_feedback2[,c(1,4,14)], user_feedback2$Device.Type == "desktop")
user_feedbackPhone <- subset(user_feedback2[,c(1,4,14)], user_feedback2$Device.Type == "app_mobile" | user_feedback2$Device.Type == "mobile")

coversion_rateComputer <- subset(coversion_rate[,c(1:3,6)], coversion_rate$DeviceType == "Desktop Computer")
coversion_ratePhone <- subset(coversion_rate[,c(1:3,6)], coversion_rate$DeviceType == "Phone")

# Undersök hur många unika datum som finns i våra dataframes. Detta är viktigt eftersom vi behöver 
# kunna koppla ihop hur många klagomål samt conversion rate en specifik dag har.
length(unique(user_feedbackComputer$Feedback.Date)) # 39
length(unique(coversion_rateComputer$Order.Date))   # 92

length(unique(user_feedbackPhone$Feedback.Date)) # 92
length(unique(coversion_ratePhone$Order.Date))   # 92

# Vi har endast data över folk som lämnade klagomål från en dator för 39 dagar. Efter inspektion visar
# det sig att data över klagomål för dataanvändande började samlas in först "2018-11-02". 
# Detta förklarra varför vi endast har 39 unika datum i "user_feedbackComputer".

# Trimma dataframen "coversion_rateComputer" så den endast innehåller samma datum som i dataframen
# "user_feedbackComputer".
Dates2 <- unique(user_feedbackComputer$Feedback.Date)
coversion_rateComputer <- coversion_rateComputer[coversion_rateComputer$Order.Date %in% Dates2,]

# Beräkna medelvärdet av conversion rate per datum för de två olika kategorierna data- samt telefon-användare.
meanCoversion_rateComputer <- coversion_rateComputer$'Conversion rate'
meanCoversion_ratePhone <- as.vector(round(tapply(coversion_ratePhone$'Conversion rate', coversion_ratePhone$Order.Date, mean),5))

# Beräkna antal klagomål per datum för de två olika kategorierna data- samt telefon-användare.
totUser_feedbackComputer <- as.vector(tapply(user_feedbackComputer$Feedback.Type, user_feedbackComputer$Feedback.Date,  function(x) length(x)))
totUser_feedbackPhone <- as.vector(tapply(user_feedbackPhone$Feedback.Type, user_feedbackPhone$Feedback.Date, function(x) length(x)))

# Skapa en scatterplot samt beräkna korrelationen för att se relationen mellan de två variablerna 
# klagomål och conversion rate för dataanvändare.
plot(meanCoversion_rateComputer, totUser_feedbackComputer, 
     xlab = "Mean convention rate for computer-users",
     ylab = "Total feedback from people using computers", font.lab = 2, 
     main = paste("The correlation beetwen the two variables is",round(cor(meanCoversion_rateComputer, totUser_feedbackComputer),5)))

cor(meanCoversion_rateComputer, totUser_feedbackComputer) # -0.2436917

# Skapa en scatterplot samt beräkna korrelatioen för att se relationen mellan de två variablerna 
# klagomål och conversion rate för telefonanvändare.
plot(meanCoversion_ratePhone, totUser_feedbackPhone,
     xlab = "Mean convention rate for phone-users",
     ylab = "Total feedback from people using phones", font.lab = 2,
     main = paste("The correlation beetwen the two variables is",round(cor(meanCoversion_ratePhone, totUser_feedbackPhone),5)))

cor(meanCoversion_ratePhone, totUser_feedbackPhone) # 0.0572671

# För dataanvändare finner vi en liten negativ korrelation, vilket innebär att om conversion rate går
# upp, tenderar klagomålen att minska lite och vice versa. För telefonanvändare kan vi nästan inte 
# utfinna någon korrelation alls mellan de två variablerna, vilket innebär att relationen mellan
# dessa två variabler är svag. Notera att de tre dagarna där conversion rate översteg 1 tydligt kan 
# urtydas i plotten. Hade vi tagit bort dessa tre dagar hade korrelationen blivit ännu mindre.

# Avslutnignsvis undersöks om den låga korrelationen mellan klagomål och conversion rate för 
# telefonanvändare kan förklaras av att en låg andel användare av t.ex. BlackBerry och/eller 
# Windows Phone bokar biljetter när de besöker prisjämförelsesajten.
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "BlackBerry"),]$'Conversion rate') # mean = 0
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "Windows Phone"),]$'Conversion rate') # mean = 0.02933
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "iPod"),]$'Conversion rate') # mean = 0.1235
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "iPhone"),]$'Conversion rate') # mean = 0.1708
summary(coversion_ratePhone[which(coversion_ratePhone$Devices == "Android Phone"),]$'Conversion rate') # mean = 0.12645

# Skulle korrelationen mellan klagomål och convention rate för iPhone- samt Android-användare räknas ut
# skulle korrelationen säkerligen bli större mellan de två variablerna, vilket bekräftas nedan.
coversion_rateIphoneAndroid <- subset(coversion_ratePhone, coversion_ratePhone$Devices == "Android Phone" | coversion_ratePhone$Devices == "iPhone")

length(unique(coversion_rateIphoneAndroid$Order.Date)) # 92

meanCoversion_rateIphoneAndroid <- as.vector(round(tapply(coversion_rateIphoneAndroid$'Conversion rate', coversion_rateIphoneAndroid$Order.Date, mean),5))

plot(meanCoversion_rateIphoneAndroid, totUser_feedbackPhone,
     xlab = "Mean convention rate for iPhone or Android users",
     ylab = "Total feedback from people using iPhone or Android", font.lab = 2,
     main = paste("The correlation beetwen the two variables is",round(cor(meanCoversion_rateIphoneAndroid, totUser_feedbackPhone),5)))

cor(meanCoversion_rateIphoneAndroid, totUser_feedbackPhone) # -0.3464306

# Vi uppnår nu den största sambandet/korrelationen hittills mellan de två variablerna, vilket innebär 
# att om conversion rate går upp, tenderar klagomålen att i ännu större grad minska och vice versa.


################################
########## Fråga III ###########
################################


# I fråga I redovisades vilken typ av klagomål som var vanligast bland kunderna. Vad som dock inte 
# framgår i svaret är vilka flygbolag som är associerade med vilken typ av klagomål samt vilka som fått 
# mest. Den informationen är viktig att ha tillgång till så att t.ex. ett flygbolag som får mycket 
# klagomål om att deras pris inte överensstämde med det förväntade, kan ta åt sig av denna kritik och
# åtgärda detta problem för att förbättra kundupplevelsen.

# Nedan skapas därför en funktion som returnerar en dataframe innehållandes information om hur mycket 
# av en viss typ av klagomål ett givet flygbolag fått. Funktionen är flexibel i den mån att den kan 
# hantera att ny data om klagomål kommer in. Det enda som då behöver göras är att importera 
# datamaterialet till variabeln "user_feedback3" med hjälp av paketet "XLConnect".

# Importera excelfilen "user_feedback" med hjälp av paketet "XLConnect" och ta bort outliern 
user_feedback3 <- loadWorkbook(file.choose(), create = T)
user_feedback3 <- readWorksheet(user_feedback3,1, header = T)
user_feedback3 <- user_feedback3[-which(user_feedback3$Final.Price==max(user_feedback3$Final.Price)),]

# Visa värden såsom "1e-04" som "0.0001".
options(scipen=999)

# Skapa funktionen som returnerar en dataframe innehållande information om vilka flygbolag som är 
# associerade med vilken typ av klagomål samt vilka som fått mest. 
# Anropet till funktioner hittas längst ner i koden.
GetFeedback <- function(){
  
  # Gör så att varje rad endast innehåller ett flygbolagsnamn under kolumnen "Carrier.Name" samt ett 
  # klagomål under kolumnen "Feedack.Type". Innehåller en rad t.ex. två olika flygbolagnamn skapas två 
  # rader innehållandes endast ett flygbolagsnamn per rad.
  user_feedback3 <- separate_rows(user_feedback3, Carrier.Name, sep = ",\\s")
  user_feedback3 <- separate_rows(user_feedback3, Feedback.Type, sep = ",\\s")
  
  # Hämta information om hur mycket klagomål av en given typ flygbolag x fått.
  'Flight Not Available' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Flight Not Available"),]$Carrier.Name))
  'Other issues' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Other issues"),]$Carrier.Name))
  'Prices did not match' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Prices didn't match"),]$Carrier.Name))
  'Site hard to use' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Site hard to use"),]$Carrier.Name))
  'Unexpected extras' <- as.data.frame(table(user_feedback3[which(user_feedback3$Feedback.Type == "Unexpected extras"),]$Carrier.Name))
  
  # Hämta namnen på flygbolagen som mottagit minst ett klagomål. Spara dessa namn i en kolum av typen 
  # character i en dataframe vid namn "Feedback".
  Name <- sort(unique(user_feedback3$Carrier.Name))
  Feedback <- as.data.frame(Name)
  Feedback$Name <- as.character(Feedback$Name)
  
  # Skapa en vector innehållandes lika många nollor som antalet rader i dataframen "Feedback".
  zero <- rep(0, length(Feedback))
  
  # Nedan byggs dataframen "Feedback" ut genom att addera totala antalet klagomål ett givet flygbolag 
  # fått i de fem olika klagomåls-kategorierna. Den första for-each loopen nedan använder sig av
  # dataframen "Flight Not Available" för att ta reda på namnet på det första flygbolaget som mottagit 
  # minst ett klagomål av kategorin "Flight Not Available". För att placera värdet, dvs antal klagomål
  # av typen "Flight Not Available" för flygbolag x, på rätt plats i dataframen "Feedback" letas raden 
  # där kolumn ett i "Feedback" är lika med just det flygbolagsnamnet. Denna procedur repeteras sedan 
  # för de fyra övriga klagomåls-kategorierna.
  # Slutligen skapas två kolumner innehållandes total antal klagomål, respektive hur stor andel av de 
  # totala antalet klagomål det givna flygbolaget mottagit.
  
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
  
  # Fixa fjärde kolumnen "Prices did not match".
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
  
  # Fixa sjätte kolumnen "Unexpected extras".
  Feedback$'Unexpected extras' <- zero
  
  `Unexpected extras`$Var1 <- as.character(`Unexpected extras`$Var1)
  
  for(d in 1:nrow(`Unexpected extras`)){
    
    Feedback[which(Feedback$Name ==`Unexpected extras`[d,1]),6] <- `Unexpected extras`[d,2]
    
  }
  
  # Fixa sjunde kolumnen "Total feedback".
  Feedback$'Total feedback' <- rowSums(Feedback[,2:6])
  
  # Fixa åttonde kolumnen "Proportion of the total feedback".
  Feedback$'Proportion of the total feedback' <- round(prop.table(Feedback$`Total feedback`),5)
  
  # Returnera den resulterande dataframen.
  return(View(Feedback))
}

# Funktionsanrop.
GetFeedback()

# Vi ser att det flybolag som mottagit mest klagomål över att deras pris inte överensstämde med det 
# förväntade är "Emirates" följt av "Virgin Atlantic".