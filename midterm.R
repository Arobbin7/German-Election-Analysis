rm(list = ls())
setwd("C:/psci200/midterm")
library(tidyverse)

##Election data obtained from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/S1M6SA 
##Economic data from https://www.bmwk.de/Redaktion/EN/Publikationen/jahresbericht-zum-stand-der-deutschen-einheit-2018.pdf?__blob=publicationFile&v=3
##https://www-genesis.destatis.de/genesis/online?operation=abruftabelleBearbeiten&levelindex=2&levelid=1677787660428&auswahloperation=abruftabelleAuspraegungAuswaehlen&auswahlverzeichnis=ordnungsstruktur&auswahlziel=werteabruf&code=82111-0001&auswahltext=&werteabruf=Werteabruf#abreadcrumb

bund <- read_csv("bundestag_v2.csv")

bund <- bund%>% 
  select(-7, -8, -9, -10, -12, -15, -16)

bund.east <- bund%>% 
  filter(state_abbrev == "mv" |state_abbrev == "bb" |state_abbrev == "st" |state_abbrev == "th" |state_abbrev == "sn")

bund.west <- bund %>% 
  filter(state_abbrev != "mv" & state_abbrev != "bb" & state_abbrev != "st" & state_abbrev != "th" & state_abbrev != "sn" & 
           state_abbrev != "be") %>%
  filter(election == 1990 | election == 1994 | election == 1998 | election == 2002 |
           election == 2005 | election == 2009 | election == 2013 | election == 2017 | election == 2021)

avg.left.east <- bund.east %>% 
  group_by(election, state_abbrev) %>% 
  mutate(vote_total = sum(first_vote, na.rm = TRUE))%>% 
  ungroup() %>% 
  filter(party_abbrev == "die linke" | party_abbrev == "pds") %>% 
  group_by(election, state_abbrev) %>%
  mutate(avg_per = (sum(first_vote, na.rm = TRUE) / vote_total) * 100) %>%
  ungroup() %>%
  select(-1, -3, -6, -7, -8, -9, -10) %>%
  group_by(election, state_abbrev) %>%
  filter(row_number() == 1) %>% 
  na.omit() 

line1 <- avg.left.east$state_abbrev
line2 <- avg.left.east$election 

pdf(file = "Avg_Left_East.pdf", height = 6, width = 30)
barplot(avg.left.east$avg_per, 
        names.arg = paste(line1, line2, sep = "\n"), 
        main = "Vote Percent for Far Left Parties by Lander and Election in the East", 
        xlab = "Lander and Election",
        ylab = "Average Voter Percent per Lander",
        ylim = c(0, 35)) 
dev.off()

avg.left.west <- bund.west %>% 
  group_by(election, state_abbrev) %>% 
  mutate(vote_total = sum(first_vote, na.rm = TRUE))%>% 
  ungroup() %>% 
  filter(party_abbrev == "die linke" | party_abbrev == "pds") %>% 
  group_by(election, state_abbrev) %>%
  mutate(avg_per = (sum(first_vote, na.rm = TRUE) / vote_total) * 100) %>%
  ungroup() %>%
  select(-1, -3, -6, -7, -8, -9, -10) %>%
  group_by(election, state_abbrev) %>%
  filter(row_number() == 1) %>% 
  na.omit()  

linke.west.line1 <- avg.left.west$state_abbrev
linke.west.line2 <- avg.left.west$election

pdf( file = "Avg_Left_West.pdf", height = 6, width = 55)
barplot(avg.left.west$avg_per, 
        names.arg = paste(linke.west.line1, linke.west.line2, sep = "\n"), 
        main = "Vote Percent for Far Left by Lander and Election in the West", 
        xlab = "Lander and Election",
        ylab = "Average Voter Percent per Lander",
        ylim = c(0, 35)) 
dev.off()

avg.right.east <- bund.east %>% 
  group_by(election, state_abbrev) %>% 
  mutate(vote_total = sum(first_vote, na.rm = TRUE))%>% 
  ungroup() %>% 
  filter(party_abbrev == "afd" | party_abbrev == "npd") %>% 
  group_by(election, state_abbrev) %>%
  mutate(avg_per = (sum(first_vote, na.rm = TRUE) / vote_total) * 100) %>%
  ungroup() %>%
  select(-1, -3, -6, -7, -8, -9, -10) %>%
  group_by(election, state_abbrev) %>%
  filter(row_number() == 1) %>% 
  na.omit() 

avg.right.west <- bund.west %>% 
  group_by(election, state_abbrev) %>% 
  mutate(vote_total = sum(first_vote, na.rm = TRUE))%>% 
  ungroup() %>% 
  filter(party_abbrev == "afd" | party_abbrev == "npd") %>% 
  group_by(election, state_abbrev) %>%
  mutate(avg_per = (sum(first_vote, na.rm = TRUE) / vote_total) * 100) %>%
  ungroup() %>%
  select(-1, -3, -6, -7, -8, -9, -10) %>%
  group_by(election, state_abbrev) %>%
  filter(row_number() == 1) %>% 
  na.omit() 

afd.east.line1 <- avg.right.east$state_abbrev
afd.east.line2 <- avg.right.east$election

afd.west.line1 <- avg.right.west$state_abbrev
afd.west.line2 <- avg.right.west$election

pdf(file = "Avg_Right_East.pdf", height = 6, width = 30)
barplot(avg.right.east$avg_per, 
        names.arg = paste(afd.east.line1, afd.east.line2, sep = "\n"), 
        main = "Vote Percent for Far Right Parties by Lander and Election in the East", 
        xlab = "Lander and Election",
        ylab = "Average Voter Percent per Lander",
        ylim = c(0, 30)) 
dev.off()

pdf(file = "Avg_Right_West.pdf", height = 6, width = 55)
barplot(avg.right.west$avg_per, 
        names.arg = paste(afd.west.line1, afd.west.line2, sep = "\n"), 
        main = "Vote Percent for Far Right Parties by Lander and Election in the West", 
        xlab = "Lander and Election",
        ylab = "Average Voter Percent per Lander",
        ylim = c(0, 30)) 
dev.off()

ex.avg.east <- bund.east %>%
  group_by(election) %>%
  mutate(votes_total = sum(first_vote, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(party_abbrev == "die linke" | party_abbrev =="afd" | party_abbrev == "pds" | party_abbrev == "npd") %>%
  group_by(election) %>%
  mutate(ex_percent = (sum(first_vote, na.rm = TRUE) / votes_total) * 100) %>%
  ungroup() %>%
  select(election, ex_percent)%>%
  group_by(election) %>%
  filter(row_number() ==1)

ex.avg.west <- bund.west %>%
  group_by(election) %>%
  mutate(votes_total = sum(first_vote, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(party_abbrev == "die linke" | party_abbrev =="afd" | party_abbrev == "pds" | party_abbrev == "npd") %>%
  group_by(election) %>%
  mutate(ex_percent = (sum(first_vote, na.rm = TRUE) / votes_total) * 100) %>%
  ungroup() %>%
  select(election, ex_percent)%>%
  group_by(election) %>%
  filter(row_number() ==1)

pdf( file = "Rise_E_vs_W.pdf", height = 6, width = 6)
plot( ex.avg.east, col = "red", type = "l",
      ylim = c(0, 45), xlab= "Election", ylab = "Average First Vote Percent of All Lander in East or West", 
      main = "First Vote Percent of Extreme Parties over Time")
lines(ex.avg.west, col = "blue", type = "l")
text(2020, 40, "East", col="red")
text(2020, 17, "West", col="blue")
dev.off() 

east.left.change <- bund.east %>%
  group_by(election) %>%
  mutate(votes_total = sum(first_vote, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(party_abbrev == "die linke" | party_abbrev == "pds") %>%
  group_by(election) %>%
  mutate(ex_percent = (sum(first_vote, na.rm = TRUE) / votes_total) * 100) %>%
  ungroup() %>%
  select(election, ex_percent)%>%
  group_by(election) %>%
  filter(row_number() ==1)

east.right.change <- bund.east %>%
  group_by(election) %>%
  mutate(votes_total = sum(first_vote, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(party_abbrev == "afd" | party_abbrev == "npd") %>%
  group_by(election) %>%
  mutate(ex_percent = (sum(first_vote, na.rm = TRUE) / votes_total) * 100) %>%
  ungroup() %>%
  select(election, ex_percent)%>%
  group_by(election) %>%
  filter(row_number() ==1)

pdf(file = "Ex_Change.pdf", height = 6, width = 6)
plot( east.left.change, col = "red", type = "l",
      ylim = c(0, 45), xlab= "Election", ylab = "Average First Vote Percent of All Lander in East", 
      main = "First Vote Percent over Time in the East")
lines(east.right.change, col = "blue", type = "l")
text(2020, 10, "Far Left", col="red")
text(2020, 27, "Far Right", col="blue")
dev.off()

gdp <- read_csv("gdp_per_capita.csv") 

gdp.gap <- gdp %>%
  group_by(year) %>%
  mutate(gap = west - east) %>%
  select(year, gap)

pdf(file = "Ex_combined.pdf", height = 6, width = 6)
plot( ex.avg.east, type = "l",
      ylim = c(0, 45), xlab= "Election", ylab = "Average First Vote Percent of All Lander in East or West", 
      main = "First Vote Percent of Extreme Parties over Time")
dev.off()

pdf( file = "Capita_Dif.pdf", height = 6, width = 6)
plot(gdp.gap, type = "l", xlab = "Year", ylab = "Gap between Western and Eastern GDP per Capita in Euros", 
     main = "Western/Eastern GDP per Capita Disparity over Time")
dev.off()

disposable <- read_csv("disposable_income.csv")

pdf(file = "Disposable.pdf", height =6, width = 6)
plot(disposable$year, disposable$west, type = "l", col= "blue", xlab = "Year", ylab = "Disposable Income in Euros",
     main = "Disposable Income in Eastern and Western Germany", ylim = c(7000, 25000))
lines(disposable$year, disposable$east, type = "l", col = "red")
text(2015, 23000, "West", col = "blue")
text(2015, 17000, "East", col = "red")
dev.off() 

disposable.dif <- disposable %>% 
  group_by(year) %>%
  mutate(disp.inc.dif = west - east)%>%
  select(year, disp.inc.dif)

pdf(file = "Disposable_dif.pdf", height = 6, width = 6)
plot(disposable.dif, type = "l", xlab = "Year", ylab = "Difference in Disposable Income Between West and East", 
     main = "Disparity of Disposable Income Between West and East")
dev.off() 

gdp.region <- read_csv("gdp_region.csv")

gdp.east <- gdp.region %>%
  select(1, 5, 9, 14, 15, 17)

gdp.west <- gdp.region %>%
  select(-4, -5, -9, -14, -15, -17, -18)

gdp.east <- gdp.east %>%
  pivot_longer(Brandenburg:Thüringen, 
               names_to = "lander") %>%
  rename(gdp = value)

gdp.west <- gdp.west %>%
  pivot_longer(`Baden-Württemberg`:`Schleswig-Holstein`,
               names_to = "lander")%>%
  rename(gdp = value)

gdp.avg.east <- gdp.east %>%
  group_by(year) %>%
  mutate(avg_regional_gdp = sum(gdp, na.rm = TRUE) / length(year)) %>%
  ungroup() %>%
  select(-2, -3) %>%
  group_by(year) %>%
  filter(row_number() ==1)

gdp.avg.west <- gdp.west %>%
  group_by(year) %>%
  mutate(avg_regional_gdp = sum(gdp, na.rm = TRUE) / length(year)) %>%
  ungroup() %>%
  select(-2, -3) %>%
  group_by(year) %>%
  filter(row_number() ==1)

pdf(file = "State_GDP.pdf", height = 6, width = 6)
plot(gdp.avg.west, type = "l", col = "blue", xlab = "Year", ylab = "Average Regional GDP",
     main = "Average State GDP in the East and West by Year", ylim = c(20000, 320000))
lines(gdp.avg.east, type = "l", col = "red")
text(2020, 310000, "West", col = "blue")
text(2020,100000, "East", col = "red")
dev.off() 

gdp.reg.all <- gdp.region %>%
  select(-4, -18) %>%
  pivot_longer(`Baden-Württemberg`:Thüringen, 
               names_to = "lander") %>%
  rename(gdp = value) 

gdps.east <- gdp.avg.east$avg_regional_gdp 
gdps.west <- gdp.avg.west$avg_regional_gdp 

gdps.dif <- gdps.west - gdps.east 
names(gdps.dif) <- gdp.avg.east$year

pdf(file = "State_GDP_Dif.pdf", height = 6, width = 6)
plot(gdp.avg.west$year, gdps.dif, type = "l", xlab= "Year", ylab= "Difference in Average State GDP between West and East in Euro", 
     main = "Difference in State GDP by Year")
dev.off()

gdps.dif.relat <- gdps.dif[c(1, 4, 8, 12, 15, 19, 23, 27, 31)]

pdf(file = "GDP_Ex_Corr.pdf", height = 8, width = 8)
plot(gdps.dif.relat, ex.avg.east$ex_percent, xlab = "Difference of Average State GDP between West and East in Euro", 
     ylab = "First Vote Share for Extreme Parties in the East", 
     main = "Vote Share for Extreme Parties vs Avg State GDP Difference")
abline(lm(ex.avg.east$ex_percent ~ gdps.dif.relat))

correl <- cor(gdps.dif.relat, ex.avg.east$ex_percent)
correl.txt <- print(paste0("Correlation = ", correl))
text(140000, 37, correl.txt)
dev.off()

