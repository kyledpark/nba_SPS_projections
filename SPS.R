require('tidyverse')
setwd("~/Desktop/NBA")

#methodology: https://www.basketball-reference.com/about/projections.html
#download csv of season stats from bbref
# -> season totals, partial seasons hidden

# season1 <- read_csv("14-15.csv") %>% mutate(year = as.double(1))
season1 <- read_csv("15-16.csv") %>% mutate(year = as.double(1))
season3 <- read_csv("16-17.csv") %>% mutate(year = as.double(3))
season6 <- read_csv("17-18.csv") %>% mutate(year = as.double(6))
seasonall <- union(season1, season3) %>% union(season6)

#stats to project: FG, FGA, 3P, 3PA, FT, FTA, ORB, TRB, AST, STL, BLK, TOV, PF, PTS, (%)


## Give the 2007-08 season a weight of 6, the 2006-07 season a weight of 3, 
## and the 2005-06 season a weight of 1 and calculate the weighted sum of minutes 
## played. The previous three seasons Howard played 3088, 3023, 3021 minutes, 
## so his weighted sum is 6 * 3088 + 3 * 3023 + 1 * 3021 = 30618 minutes played.

## Use the same weights as above and calculate the weighted sum of rebounds. 
## The previous three seasons Howard grabbed 1161, 1008, and 1022 rebounds, 
## so his weighted sum is 6 * 1161 + 3 * 1008 + 1 * 1022 = 11012 rebounds.

seasonall_weighted <- seasonall %>%  
  select(-Rk, -Age, -`FG%`, -`3P%`, -`2P%`, -`eFG%`, -`FT%`) %>%
  mutate_if(is.numeric, .funs=funs(.*year))

player_totals <- seasonall_weighted %>%
  group_by(Player) %>%
  summarize_if(is.numeric, sum)

## Calculate the weighted sum of rebounds for a league average player playing 
## Howard's minutes, then scale this figure to 1000 minutes. The weighted sum 
## for a league average player playing these minutes is 
## 6 * 3088 * (103271 / 594100) + 3 * 3023 * (100994 / 595750) + 
## 1 * 3021 * (100754 / 595550) = 5269.180 rebounds. 
## Scaled to 1000 minutes this is 1000 * (5269.180 / 30618) = 172.094 rebounds.

leagueavg <- seasonall %>% 
  select_if(is.numeric) %>%
  select(-Rk, -Age, -`FG%`, -`3P%`, -`2P%`, -`eFG%`, -`FT%`) %>%
  group_by(year) %>%
  summarise_all(sum) %>%
  mutate(year=year*year)

league_weighted <- leagueavg %>%
  # Had to move MP to end otherwise mutate_all would stop
  select(-MP,MP) %>%
  mutate_all(.funs=funs(./MP)) %>%
  mutate(year=c(1,9,36))

weighted_join <- left_join(seasonall_weighted, league_weighted, by="year")

season_weighted <- weighted_join %>%
  transmute(Player,
           year,
           FG = FG.x*FG.y,
           FGA = FGA.x*FGA.y,
           `3P` = `3P.x`*`3P.y`,
           `3PA` = `3PA.x`*`3PA.y`,
           `2P` = `2P.x`*`2P.y`,
           `2PA` = `2PA.x`*`2PA.y`,
           FT = FT.x*FT.y,
           FTA = FTA.x*FTA.y,
           ORB = ORB.x*ORB.y,
           DRB = DRB.x*DRB.y,
           TRB = TRB.x*TRB.y,
           AST = AST.x*AST.y,
           STL = STL.x*STL.y,
           BLK = BLK.x*BLK.y,
           TOV = TOV.x*TOV.y,
           PF = PF.x*PF.y,
           PTS = PTS.x*PTS.y
           ) %>%
  group_by(Player) %>%
  summarize_if(is.numeric, sum)

season_weighted_1000 <- left_join(season_weighted,
                                  select(player_totals, Player, MP),
                                  by="Player") %>%
  transmute(Player,
            year,
            FG = 1000 * (FG/MP),
            FGA = 1000 * (FGA/MP),
            `3P` = 1000 * (`3P`/MP),
            `3PA` = 1000 * (`3PA`/MP),
            `2P` = 1000 * (`2P`/MP),
            `2PA` = 1000 * (`2PA`/MP),
            FT = 1000 * (FT/MP),
            FTA = 1000 * (FTA/MP),
            ORB = 1000 * (ORB/MP),
            DRB = 1000 * (DRB/MP),
            TRB = 1000 * (TRB/MP),
            AST = 1000 * (AST/MP),
            STL = 1000 * (STL/MP),
            BLK = 1000 * (BLK/MP),
            TOV = 1000 * (TOV/MP),
            PF = 1000 * (PF/MP),
            PTS = 1000 * (PTS/MP)
            )
  

## Calculate the player's projected per-36 minute value. To do this we will 
## find the sum of (2) and (3); divide by the sum of (1) and 1000; and 
## multiply by 36: (11012 + 172.094) / (30618 + 1000) * 36 = 12.734 rebounds 
## per 36 minutes.

per36 <- left_join(player_totals, season_weighted_1000, by="Player") %>%
  transmute(Player,
            FG = (FG.x+FG.y) / (MP+1000) * 36,
            FGA = (FGA.x+FGA.y) / (MP+1000) * 36,
            `3P` = (`3P.x`+`3P.y`) / (MP+1000) * 36,
            `3PA` = (`3PA.x`+`3PA.y`) / (MP+1000) * 36,
            `2P` = (`2P.x`+`2P.y`) / (MP+1000) * 36,
            `2PA` = (`2PA.x`+`2PA.y`) / (MP+1000) * 36,
            FT = (FT.x+FT.y) / (MP+1000) * 36,
            FTA = (FTA.x+FTA.y) / (MP+1000) * 36,
            ORB = (ORB.x+ORB.y) / (MP+1000) * 36,
            DRB = (DRB.x+DRB.y) / (MP+1000) * 36,
            TRB = (TRB.x+TRB.y) / (MP+1000) * 36,
            AST = (AST.x+AST.y) / (MP+1000) * 36,
            STL = (STL.x+STL.y) / (MP+1000) * 36,
            BLK = (BLK.x+BLK.y) / (MP+1000) * 36,
            TOV = (TOV.x+TOV.y) / (MP+1000) * 36,
            PF = (PF.x+PF.y) / (MP+1000) * 36,
            PTS = (PTS.x+PTS.y) / (MP+1000) * 36
  )

## Calculate and apply the age adjustment. If the player is younger than 28, 
## then the age adjustment is equal to (28 - age) * 0.004. If the player is older 
## than 28, then the adjustment is equal to (28 - age) * 0.002. Howard's 2008-09 
## season was his age 23 season, so his age adjustment is (28 - 23) * 0.004 = 0.02. 
## Applying this to the figure in (4) we get (1 + 0.02) * 12.734 = 13.0 rebounds 
## per 36 minutes.


ages <- seasonall %>%
  group_by(Player) %>%
  summarise(Age = max(Age), year = max(year))

ages1 <- ages %>%
  filter(year=="1") %>%
  mutate(Age = Age+3)

ages2 <- ages %>%
  filter(year=="3") %>%
  mutate(Age = Age+2)

ages3 <- ages %>%
  filter(year=="6") %>%
  mutate(Age = Age+1)

ages <- union(ages1, ages2) %>% union(ages3)

ages29 <- ages %>%
  filter(Age>28) %>%
  mutate(Adj = (28-Age) * 0.002)
ages28 <- ages %>%
  filter(Age==28) %>%
  mutate(Adj = 0)
ages27 <- ages %>%
  filter(Age<28) %>%
  mutate(Adj = (28-Age) * 0.004)

ages <- union(ages29, ages28) %>% union(ages27)

# per36_age_adj <- left_join(per36, select(ages, Player, Adj, Age), by="Player") %>%
#   transmute(Player,
#             Age,
#             FG = FG + (FG*Adj),
#             FGA = FGA - (FGA*Adj),
#             `3P` = `3P` + (`3P`*Adj),
#             `3PA` = `3PA` - (`3PA`*Adj),
#             `2P` = `2P` + (`2P`*Adj),
#             `2PA` = `2PA` - (`2PA`*Adj),
#             FT = FT + (FT*Adj),
#             FTA = FTA - (FTA*Adj),
#             ORB = ORB + (ORB*Adj),
#             DRB = DRB + (DRB*Adj),
#             TRB = TRB + (TRB*Adj),
#             AST = AST + (AST*Adj),
#             STL = STL + (STL*Adj),
#             BLK = BLK + (BLK*Adj),
#             TOV = TOV + (TOV*Adj),
#             PF = PF + (PF*Adj),
#             PTS = PTS + (PTS*Adj),
#             PTS_CALC = (`2P`*2)+(`3P`*3)+(FT),
#             FGP = FG/FGA,
#             FTP = FT/FTA
#   )


per36_age_adj <- left_join(per36, select(ages, Player, Adj, Age), by="Player") %>%
  transmute(Player,
            Age,
            FG = FG * (1 + Adj),
            FGA = FGA * (1 + Adj),
            `3P` = `3P` * (1 + Adj),
            `3PA` = `3PA` * (1 + Adj),
            `2P` = `2P` * (1 + Adj),
            `2PA` = `2PA` * (1 + Adj),
            FT = FT * (1 + Adj),
            FTA = FTA * (1 + Adj),
            ORB = ORB * (1 + Adj),
            DRB = DRB * (1 + Adj),
            TRB = TRB * (1 + Adj),
            AST = AST * (1 + Adj),
            STL = STL * (1 + Adj),
            BLK = BLK * (1 + Adj),
            TOV = TOV * (1 + Adj),
            PF = PF * (1 + Adj),
            PTS = PTS * (1 + Adj),
            # PTS_CALC = (`2P`*2)+(`3P`*3)+(FT),
            FGP = FG/FGA,
            FTP = FT/FTA
  ) %>%
  separate(Player, c("Player", "ID"), "\\\\")

write_csv(per36_age_adj, "simple18-19.csv")

