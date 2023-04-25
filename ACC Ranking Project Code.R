getwd()
setwd("/Users/graydon/Desktop/ACC Ranking Project")

con <- dbConnect(RSQLite::SQLite(), "acc1819.db")
dbListTables(con)
dbListFields(con, "box_scores")
dbListFields(con, "games")
box.scores <- dbReadTable(con, "box_scores")
games <- dbReadTable(con, "games")

df <- games %>% left_join( box.scores,
                           by = c("GameId" = "GameId"))
df

library(tidyverse)

Teamnames <- unique(df$Team)

dftibble <- as_tibble(df) %>% select(GameId,NeutralSite,Team,Home,Score)
dftibble

bygame <- dftibble %>%
  pivot_wider(id_cols=c(GameId,NeutralSite), names_from=Home, values_from=c(Team,Score)) %>%
  mutate(mov=abs(Score_1-Score_0), winner=ifelse(Score_1>Score_0,Team_1,Team_0), loser=ifelse(Score_1>Score_0,Team_0,Team_1), winAtHome=(winner==Team_1), winOnRoad=(winner==Team_0))
bygame

bygame2 <- bind_rows(bygame %>% mutate(Team=Team_1,Opp=Team_0,win=winAtHome,mov=ifelse(winAtHome,mov,-mov)),
                     bygame %>% mutate(Team=Team_0,Opp=Team_1,win=winOnRoad,mov=ifelse(winOnRoad,mov,-mov))) %>%
  select(-Team_1,-Team_0,-Score_1,-Score_0)

removeneutral <- TRUE

if(removeneutral) bygame2 <- bygame2 %>% filter(NeutralSite==0)
bygame2

byteam2 <- bygame2 %>% group_by(Team) %>% summarize(tgames=n(), wp=mean(win))

bypair2 <- bygame2 %>% group_by(Team,Opp) %>% summarize(ngames=n(), mov=mean(mov), win=sum(win))

bypair2withowp <- bypair2 %>% left_join(byteam2 %>% rename(Opp=Team),by=join_by(Opp))
byteam2 <- byteam2 %>% left_join(
  bypair2withowp %>%
    group_by(Team,Opp) %>%
    summarize(wtwp = sum(wp*ngames), ngames = sum(ngames)) %>%
    summarize(owp = sum(wtwp)/sum(ngames))
)
bypair2withoowp <- bypair2withowp %>% left_join(byteam2 %>% rename(Opp=Team),by=join_by(Opp))
byteam2 <- byteam2 %>% left_join(
  bypair2withoowp %>%
    group_by(Team,Opp) %>%
    summarize(wtowp = sum(owp*ngames), ngames = sum(ngames)) %>%
    summarize(oowp = sum(wtowp)/sum(ngames))
)

byteam2 <- byteam2 %>% group_by(Team) %>% mutate(rpi=0.25*wp+0.5*owp+0.25*oowp) %>% arrange(desc(rpi))
byteam2$rank <- 1:nrow(byteam2)
byteam2

write.csv(byteam2, "ACCRankings1819.csv", row.names = FALSE)

