rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.

library(tidyverse)
#library(NCmisc)
#list.functions.in.file("Code/coach.R", alphabetic = TRUE)

setwd("~/NFL")

#Coaching History
coach_history <- read.csv("PFR/coachhistory.csv")

ch <- coach_history |>
  filter(Year>=1966) |>
  separate_rows(Role, sep="/") |>
  filter(!grepl("^Head Coach \\(Suspended for 2012 season\\)$", Role)) |>
  filter(Employer!="")
  

#College
ch$Level <- gsub("^College (FBS)$", "College", ch$Level)
ch$Level <- gsub("^College (FCS)$", "College", ch$Level)
ch$Level <- gsub("^College (NAIA)$", "College", ch$Level)
ch$Level <- gsub("^College (NCAA D-I)$", "College", ch$Level)
ch$Level <- gsub("^College (NCAA D-II)$", "College", ch$Level)
ch$Level <- gsub("^College (NCAA D-III)$", "College", ch$Level)
ch$Level <- gsub("^College (NJCAA)$", "College", ch$Level)

ch <- ch |>
  filter(Level=="AFL" | Level=="College" | Level=="NFL")

#GA
ch$Role <- gsub("^Grad Asst$", "GA", ch$Role)
ch$Role <- gsub("^Graduate Assistant$", "GA", ch$Role)
#Intern
ch$Role <- gsub("^Offensive Coaching Intern$", "Intern", ch$Role)
ch$Role <- gsub("^Player Personnel Intern$", "Intern", ch$Role)
ch$Role <- gsub("^Defensive Intern$", "Intern", ch$Role)
ch$Role <- gsub("^Coaching Intern$", "Intern", ch$Role)
#Scout
ch$Role <- gsub("^Personnel Scout$", "Scout", ch$Role)
ch$Role <- gsub("^Pro Scout$", "Scout", ch$Role)
ch$Role <- gsub("^Scout$", "Scout", ch$Role)
ch$Role <- gsub("^Scouting Assistant$", "Scout", ch$Role)
ch$Role <- gsub("^Scouting Coordinator$", "Scout", ch$Role)
ch$Role <- gsub("^Asst AD$", "Scout", ch$Role)

#aHC
ch$Role <- gsub("^Assistant HC$", "aHC", ch$Role)
ch$Role <- gsub("^Assistant Head Coach$", "aHC", ch$Role)
ch$Role <- gsub("^Asst Head Coach$", "aHC", ch$Role)
ch$Role <- gsub("^Asst. HC$", "aHC", ch$Role)
ch$Role <- gsub("^Asst. Head Coach$", "aHC", ch$Role)
ch$Role <- gsub("^AHC$", "aHC", ch$Role)
ch$Role <- gsub("^Play-Caller$", "aHC", ch$Role)
ch$Role <- gsub("^Associate HC$", "aHC", ch$Role)
#aC
ch$Role <- gsub("^Freshman$", "aC", ch$Role)
ch$Role <- gsub("^Freshman Coach$", "aC", ch$Role)
ch$Role <- gsub("^Freshmen Team Head Coach$", "aC", ch$Role)
ch$Role <- gsub("^Assistant$", "aC", ch$Role)
ch$Role <- gsub("^Assistant Coach$", "aC", ch$Role)
ch$Role <- gsub("^Asst. Coach$", "aC", ch$Role)
ch$Role <- gsub("^Coaches' Assistant$", "aC", ch$Role)
ch$Role <- gsub("^Coaches Assistant$", "aC", ch$Role)
ch$Role <- gsub("^Coaching Assistant$", "aC", ch$Role)
ch$Role <- gsub("^Asst. to the Head Coach$", "aC", ch$Role)
ch$Role <- gsub("^Senior Assistant$", "aC", ch$Role)
ch$Role <- gsub("^Sr. Asst.$", "aC", ch$Role)
ch$Role <- gsub("^Personnel Assistant$", "aC", ch$Role)
ch$Role <- gsub("^Player Personnel Staff$", "aC", ch$Role)
ch$Role <- gsub("^Quality Control$", "aC", ch$Role)
ch$Role <- gsub("^Sports Science$", "aC", ch$Role)
ch$Role <- gsub("^Spread Game Analyst$", "aC", ch$Role)
ch$Role <- gsub("^Sr. Coaching Assistant$", "aC", ch$Role)
ch$Role <- gsub("^Strength$", "aC", ch$Role)
ch$Role <- gsub("^Strength & Conditioning$", "aC", ch$Role)
ch$Role <- gsub("^Strength and Conditioning$", "aC", ch$Role)
ch$Role <- gsub("^Video Assistant$", "aC", ch$Role)
ch$Role <- gsub("^Volunteer Assistant$", "aC", ch$Role)
ch$Role <- gsub("^Consultant$", "aC", ch$Role)
#aOC
ch$Role <- gsub("^Assistant Offensive Line$", "aOC", ch$Role)
ch$Role <- gsub("^Assistant Wide Receivers$", "aOC", ch$Role)
ch$Role <- gsub("^Asst OL$", "aOC", ch$Role)
ch$Role <- gsub("^Asst. Offense$", "aOC", ch$Role)
ch$Role <- gsub("^Asst. OL$", "aOC", ch$Role)
ch$Role <- gsub("^Asst. OL Coach$", "aOC", ch$Role)
ch$Role <- gsub("^Asst. QB Coach$", "aOC", ch$Role)
ch$Role <- gsub("^Asst. Quarterbacks$", "aOC", ch$Role)
ch$Role <- gsub("^Asst. RB Coach$", "aOC", ch$Role)
ch$Role <- gsub("^Asst. Tight Ends$", "aOC", ch$Role)
ch$Role <- gsub("^Asst. Wide Receivers$", "aOC", ch$Role)
ch$Role <- gsub("^Backfield$", "aOC", ch$Role)
ch$Role <- gsub("^Backfield Coach$", "aOC", ch$Role)
ch$Role <- gsub("^Offense$", "aOC", ch$Role)
ch$Role <- gsub("^Offensive Assistant$", "aOC", ch$Role)
ch$Role <- gsub("^Offensive Asst.$", "aOC", ch$Role)
ch$Role <- gsub("^Offensive Backfield$", "aOC", ch$Role)
ch$Role <- gsub("^Offensive Backs$", "aOC", ch$Role)
ch$Role <- gsub("^Offensive Line$", "aOC", ch$Role)
ch$Role <- gsub("^Offensive Line Assistant$", "aOC", ch$Role)
ch$Role <- gsub("^OL$", "aOC", ch$Role)
ch$Role <- gsub("^Passing Game$", "aOC", ch$Role)
ch$Role <- gsub("^Passing Game Coord.$", "aOC", ch$Role)
ch$Role <- gsub("^Passing Game Coordinator$", "aOC", ch$Role)
ch$Role <- gsub("^QB$", "aOC", ch$Role)
ch$Role <- gsub("^QB Coach$", "aOC", ch$Role)
ch$Role <- gsub("^QBs$", "aOC", ch$Role)
ch$Role <- gsub("^Quarterbacks$", "aOC", ch$Role)
ch$Role <- gsub("^RB$", "aOC", ch$Role)
ch$Role <- gsub("^RB Coach$", "aOC", ch$Role)
ch$Role <- gsub("^H-Backs$", "aOC", ch$Role)
ch$Role <- gsub("^Receivers$", "aOC", ch$Role)
ch$Role <- gsub("^Receivers Coach$", "aOC", ch$Role)
ch$Role <- gsub("^Run Game Coordinator$", "aOC", ch$Role)
ch$Role <- gsub("^Running Backs$", "aOC", ch$Role)
ch$Role <- gsub("^Running Game Coordinator$", "aOC", ch$Role)
ch$Role <- gsub("^TE$", "aOC", ch$Role)
ch$Role <- gsub("^TE Coach$", "aOC", ch$Role)
ch$Role <- gsub("^Tight Ends$", "aOC", ch$Role)
ch$Role <- gsub("^Wide Receivers$", "aOC", ch$Role)
ch$Role <- gsub("^WR$", "aOC", ch$Role)
ch$Role <- gsub("^WR Coach$", "aOC", ch$Role)
ch$Role <- gsub("^WRs$", "aOC", ch$Role)
ch$Role <- gsub("^Asst. Offensive Line$", "aOC", ch$Role)
ch$Role <- gsub("^Offensive Consultant$", "aOC", ch$Role)
ch$Role <- gsub("^Sr. Offensive Assistant$", "aOC", ch$Role)
ch$Role <- gsub("^Senior Offensive Assistant$", "aOC", ch$Role)
ch$Role <- gsub("^Senior Offensive Asst.$", "aOC", ch$Role)
ch$Role <- gsub("^Offensive Quality Control$", "aOC", ch$Role)
ch$Role <- gsub("^Sr. Offensive Consultant$", "aOC", ch$Role)
ch$Role <- gsub("^RGC$", "aOC", ch$Role)
#aDC
ch$Role <- gsub("^Assistant Defensive Backs$", "aDC", ch$Role)
ch$Role <- gsub("^Asst. DB Coach$", "aDC", ch$Role)
ch$Role <- gsub("^Asst. Defensive Backs$", "aDC", ch$Role)
ch$Role <- gsub("^Asst. Defensive Line$", "aDC", ch$Role)
ch$Role <- gsub("^Asst. LB$", "aDC", ch$Role)
ch$Role <- gsub("^Asst. Linebackers$", "aDC", ch$Role)
ch$Role <- gsub("^DB$", "aDC", ch$Role)
ch$Role <- gsub("^DB Coach$", "aDC", ch$Role)
ch$Role <- gsub("^DE$", "aDC", ch$Role)
ch$Role <- gsub("^Def. Pass Gm. Coord.$", "aDC", ch$Role)
ch$Role <- gsub("^Defense$", "aDC", ch$Role)
ch$Role <- gsub("^Defensive Assistant$", "aDC", ch$Role)
ch$Role <- gsub("^Defensive Backfield$", "aDC", ch$Role)
ch$Role <- gsub("^Defensive Backs$", "aDC", ch$Role)
ch$Role <- gsub("^Defensive Backs Coach$", "aDC", ch$Role)
ch$Role <- gsub("^Defensive Coach$", "aDC", ch$Role)
ch$Role <- gsub("^Defensive Consultant$", "aDC", ch$Role)
ch$Role <- gsub("^Defensive Ends$", "aDC", ch$Role)
ch$Role <- gsub("^Defensive Line$", "aDC", ch$Role)
ch$Role <- gsub("^DL$", "aDC", ch$Role)
ch$Role <- gsub("^LB$", "aDC", ch$Role)
ch$Role <- gsub("^LBs$", "aDC", ch$Role)
ch$Role <- gsub("^Line Coach$", "aDC", ch$Role)
ch$Role <- gsub("^Inside Linebackers$", "aDC", ch$Role)
ch$Role <- gsub("^Linebackers$", "aDC", ch$Role)
ch$Role <- gsub("^Linebackers Coach$", "aDC", ch$Role)
ch$Role <- gsub("^OLB$", "aDC", ch$Role)
ch$Role <- gsub("^Outside Linebackers$", "aDC", ch$Role)
ch$Role <- gsub("^Secondary$", "aDC", ch$Role)
ch$Role <- gsub("^Secondary \\(nickel package\\)$", "aDC", ch$Role)
ch$Role <- gsub("^Senior Def. Asst.$", "aDC", ch$Role)
ch$Role <- gsub("^Senior Defensive Assistant$", "aDC", ch$Role)
ch$Role <- gsub("^Senior Defensive Asst.$", "aDC", ch$Role)
ch$Role <- gsub("^Sr. Def. Asst.$", "aDC", ch$Role)
ch$Role <- gsub("^Sr. Defensive Asst.$", "aDC", ch$Role)
ch$Role <- gsub("^Safeties$", "aDC", ch$Role)
ch$Role <- gsub("^Strong Safeties$", "aDC", ch$Role)
ch$Role <- gsub("^Defensive Quality Assistant$", "aDC", ch$Role)
ch$Role <- gsub("^Defensive Quality Control$", "aDC", ch$Role)
ch$Role <- gsub("^Defensive Quality Control Assistant$", "aDC", ch$Role)

#aSTC
ch$Role <- gsub("^Asst. Special Teams$", "aSTC", ch$Role)
ch$Role <- gsub("^Asst. Special Teams Coach$", "aSTC", ch$Role)
ch$Role <- gsub("^Asst. ST Coach$", "aSTC", ch$Role)
ch$Role <- gsub("^Special Team Assistant$", "aSTC", ch$Role)
ch$Role <- gsub("^Special Teams$", "aSTC", ch$Role)
ch$Role <- gsub("^Special Teams Assistant$", "aSTC", ch$Role)
ch$Role <- gsub("^Special Teams Coach$", "aSTC", ch$Role)
ch$Role <- gsub("^ST$", "aSTC", ch$Role)
ch$Role <- gsub("^Kicking Teams$", "aSTC", ch$Role)
ch$Role <- gsub("^Special Assistant$", "aSTC", ch$Role)
ch$Role <- gsub("^Special Asst. to Head Coach$", "aSTC", ch$Role)
ch$Role <- gsub("^Special Advisor$", "aSTC", ch$Role)

#Interim Coach
ch$Role <- gsub("^Interim OC$", "iOC", ch$Role)
ch$Role <- gsub("^Interim Offensive Coordinator$", "iOC", ch$Role)
ch$Role <- gsub("^Interim DC$", "iDC", ch$Role)
ch$Role <- gsub("^Interim Defensive Coordinator$", "iDC", ch$Role)
ch$Role <- gsub("^Interim HC$", "iHC", ch$Role)
ch$Role <- gsub("^Interim Head Coach$", "iHC", ch$Role)
#OC
ch$Role <- gsub("^Offensive Coordinator$", "OC", ch$Role)
ch$Role <- gsub("^Co-Offensive Coordinator$", "OC", ch$Role)
#DC
ch$Role <- gsub("^Defensive Coordinator$", "DC", ch$Role)
ch$Role <- gsub("^Co-Defensive Coordinator$", "DC", ch$Role)
#STC
ch$Role <- gsub("^Special Teams Coordinator$", "STC", ch$Role)
#HC
ch$Role <- gsub("^Head Coach$", "HC", ch$Role)
ch$Role <- gsub("^Coach Emeritus$", "HC", ch$Role)

NFL <- ch |>
  filter(Level=="NFL") #|>
  #distinct()

ac <- NFL |>
  mutate(bHC = ifelse(Role=="HC", 1, 0)) |>
  arrange(Year, Coach) |>
  group_by(Coach) |>
  filter(row_number() <= first( which(bHC  == 1) ) -1 ) |>
  ungroup() |> group_by(Year, Role, Coach) |>
  mutate(sn = row_number()) |> slice_max(sn, n=1) |>
  mutate(sn = ifelse(Role=='OC' | Role=='DC', 1, sn)) |>
  ungroup() 

hc <- NFL |>
  mutate(bHC = ifelse(Role=="HC", 1, 0)) |>
  filter(bHC==1) |>
  group_by(Year, Role, Coach) |>
  mutate(sn = row_number()) |> slice_max(sn, n=1) |>
  ungroup()

final <- ac |>
  bind_rows(hc) |>
  group_by(Year, Coach) |>
  ungroup() 

#TEAM
final$Employer <- gsub("^Arizona Cardinals$", "ARI", final$Employer)
final$Employer <- gsub("^Phoenix Cardinals$", "ARI", final$Employer)
final$Employer <- gsub("^St. Louis Cardinals$", "ARI", final$Employer)
final$Employer <- gsub("^Atlanta Falcons$", "ATL", final$Employer)
final$Employer <- gsub("^Baltimore Ravens$", "BAL", final$Employer)
final$Employer <- ifelse(final$Year<1996, gsub("^Cleveland Browns$", "BAL", final$Employer), 
                         gsub("^Cleveland Browns$", "CLE", final$Employer))
final$Employer <- gsub("^Boston Patriots$", "NE", final$Employer)
final$Employer <- gsub("^New England Patriots$", "NE", final$Employer)
final$Employer <- gsub("^Buffalo Bills$", "BUF", final$Employer)
final$Employer <- gsub("^Carolina Panthers$", "CAR", final$Employer)
final$Employer <- gsub("^Chicago Bears$", "CHI", final$Employer)
final$Employer <- gsub("^Cincinnati Bengals$", "CIN", final$Employer)
final$Employer <- gsub("^Dallas Cowboys$", "DAL", final$Employer)
final$Employer <- gsub("^Denver Broncos$", "DEN", final$Employer)
final$Employer <- gsub("^Detroit Lions$", "DET", final$Employer)
final$Employer <- gsub("^Green Bay Packers$", "GB", final$Employer)
final$Employer <- gsub("^Houston Oilers$", "TEN", final$Employer)
final$Employer <- gsub("^Tennessee Oilers$", "TEN", final$Employer)
final$Employer <- gsub("^Tennessee Titans$", "TEN", final$Employer)
final$Employer <- gsub("^Houston Texans$", "HOU", final$Employer)
final$Employer <- gsub("^Baltimore Colts$", "IND", final$Employer)
final$Employer <- gsub("^Indianapolis Colts$", "IND", final$Employer)
final$Employer <- gsub("^Jacksonville Jaguars$", "JAX", final$Employer)
final$Employer <- gsub("^Kansas City Chiefs$", "KC", final$Employer)
final$Employer <- gsub("^San Diego Chargers$", "LAC", final$Employer)
final$Employer <- gsub("^Los Angeles Chargers$", "LAC", final$Employer)
final$Employer <- gsub("^Los Angeles Raiders$", "LV", final$Employer)
final$Employer <- gsub("^Oakland Raiders$", "LV", final$Employer)
final$Employer <- gsub("^Las Vegas Raiders$", "LV", final$Employer)
final$Employer <- gsub("^Los Angeles Rams$", "LA", final$Employer)
final$Employer <- gsub("^St. Louis Rams$", "LA", final$Employer)
final$Employer <- gsub("^Miami Dolphins$", "MIA", final$Employer)
final$Employer <- gsub("^Minnesota Vikings$", "MIN", final$Employer)
final$Employer <- gsub("^New Orleans Saints$", "NO", final$Employer)
final$Employer <- gsub("^New York Giants$", "NYG", final$Employer)
final$Employer <- gsub("^New York Jets$", "NYJ", final$Employer)
final$Employer <- gsub("^Philadelphia Eagles$", "PHI", final$Employer)
final$Employer <- gsub("^Pittsburgh Steelers$", "PIT", final$Employer)
final$Employer <- gsub("^San Francisco 49ers$", "SF", final$Employer)
final$Employer <- gsub("^Seattle Seahawks$", "SEA", final$Employer)
final$Employer <- gsub("^Tampa Bay Buccaneers$", "TB", final$Employer)
final$Employer <- gsub("^Washington Redskins$", "WAS", final$Employer)
final$Employer <- gsub("^Washington Football Team$", "WAS", final$Employer)
final$Employer <- gsub("^Washington Commanders$", "WAS", final$Employer)

wins <- read.csv("Coach Record/wins.csv")

#Head Coach, Main Role for Asst, and Win %
ch_NFL1 <- final |>
  filter(Role=="HC") |>
  select(c(Year, Employer, HC_Age = Age, HC = Role, Head_Coach = Coach)) |>
  right_join(final, by = c("Year", "Employer")) |>
  filter(Role!="HC" & Role!="iDC" & Role!="iOC" & Role!="iHC") |>
  group_by(Year, Head_Coach, Coach) |>
  mutate(n0 = row_number()) |>
  right_join(wins, by = c('Year' = 'Season', 'Head_Coach' = 'Coach')) |>
  filter(!is.na(Coach)) |>
  arrange(Year, Head_Coach, Coach) |> group_by(Head_Coach, Coach) |>
  mutate(#HC_Age = mean(HC_Age[n0==1]),
         #Age = mean(Age[n0==1]),
         #Age_D = HC_Age - Age,
         #TG??? see final_ch
         fy_a = first(Year),
         ly_a = last(Year),
         TYr = length(unique(Year[n0==1])),
         PFa = mean(PF[n0==1])/mean(Total[n0==1]),
         PAa = mean(PA[n0==1])/mean(Total[n0==1]),
         #LAa = mean(LA[n0==1])/mean(MG[n0==1]),
         #PFd = PFa - LAa,
         #PAd = LAa - PAa
         ) |>
  ungroup() |>
  group_by(Head_Coach, Coach, Role) |>
  mutate(n = sum(sn)) |> 
  select(Head_Coach, tW, tPyW, tg_hc, ftm, ltm, fy_hc, ly_hc, Coach, Role, fy_a, ly_a, TYr, n) |>
  distinct() |>
  pivot_wider(names_from = Role, values_from = n, values_fill = list(n = 0)) |>
  ungroup() |>
  mutate(Coach = ifelse(Head_Coach==Coach, NA, Coach)) |>
  group_by(Head_Coach, Coach) |>
  mutate(weight = rowSums(across(aDC:STC))) |>
  ungroup() |>
  distinct() |>
  gather(group, group_count, aDC:STC) |> 
  group_by(Head_Coach, Coach) |> 
  slice(which.max(group_count)) |>
  ungroup()

#last Role
ch_NFL2 <- final |>
  filter(Role=="HC") |>
  select(c(Year, Employer, Role, Coach)) |>
  rename(HC = Role, Head_Coach = Coach) |>
  right_join(final, by = c("Year", "Employer")) |>
  filter(Role!="HC" & Role!="iDC" & Role!="iOC" & Role!="iHC") |>
  select(Year, Head_Coach, Coach, Role) |>
  arrange(Year, Head_Coach, Coach) |>
  group_by(Head_Coach, Coach) |>
  slice_tail(n=1) |>
  ungroup() |>
  select(Head_Coach, Coach, Role) |>
  rename(lrole = Role)

#Final for Network Analysis File
final_ch <- ch_NFL1 |>
  right_join(ch_NFL2, by = c("Head_Coach", "Coach")) |>
  select(-tW, -tPyW, -tg_hc, -ftm, -ltm, -fy_hc, -ly_hc, -TYr, -fy_a, -ly_a, everything()) |>
  # group_by(Coach) |>
  # mutate(TCyr = sum(TYr, na.rm=T)) |> ungroup() |>
  rowwise() |> mutate(nw = weight / TYr,
                       pyw500 = ifelse((tPyW/tg_hc)>=.5, 1, 0),
                       above500 = ifelse((tW/tg_hc)>=.5, 1, 0),
                       odo = ifelse(group=='aOC' | group=='OC', 1,
                                    ifelse(group=='aDC' | group=='DC', 2, 3)),
                       cord = ifelse(group=='OC' | group=='DC' | group=='STC', 1, 0),
                       lodo = ifelse(lrole=='aOC' | lrole=='OC', 1,
                                    ifelse(lrole=='aDC' | lrole=='DC', 2, 3)),
                       lcord = ifelse(lrole=='OC' | lrole=='DC' | lrole=='STC', 1, 0),
                       #PFda = TYr/TCyr * PFd,
                       #PAda = TYr/TCyr * PAd
                       ) |>
  filter(!is.na(group)) |>
  # group_by(Coach) |>
  # mutate(#TGA = sum(TG),
  #        PFt = sum(PFda),
  #        PAt = sum(PAda)) |>
  ungroup()

#csv for Final
write.csv(final_ch,"PFR/NFL_coaches.csv", row.names = FALSE)

NFL_coaches <- read.csv("PFR/NFL_coaches.csv")

coaches_22_23 <- read.csv("PFR/coaches_22_23.csv")

tree <- NFL_coaches |>
  rbind(coaches_22_23) |>
  distinct()

write.csv(tree, "Coach Record/tree.csv", row.names = FALSE)
