library(rvest)
library(stringr)
library(stringi)
library(statnet)
library(igraph)

tmlstURL <- "https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations"
tmlst <- html_text(html_nodes(read_html(tmlstURL), "td:nth-child(1)"))
tmlst <- gsub("\n", "", tmlst)
tmlst <- tmlst[2:31]
tmlst[2] <- "BRK"
tmlst[4] <- "CHO"
tmlst[24] <- "PHO"
tmlst2 <- c(tmlst, "KCK", "WSB", "SEA", "CHH", "CHA", "NJN", "VAN", "NOH", "NOK" )
tmlst_stable <- tmlst2[c(1,3,5,6,7,8,9,10,11,12,13,14,17,20,23,24,25,27,29)]

years <- seq(from=1985, to=2020, by=1)
years <- as.character(years)


#players <- read.csv("/Users/ian/Downloads/datasets_1358_30676_Players.csv", stringsAsFactors = F)
players2 <- read.csv("/Users/ian/Downloads/Seasons_Stats.csv")
players2 <- players2[players2$Year >= 1984,]
players2 <- players2[,c("Player", "Tm", "Year")]
playerslist <- unique(players2$Player)


############################ Adds players from post-dataset era to playerlist

for(i in 1:length(tmlst)){
  team <- tmlst[i]
  for(j in (length(years)-2):length(years)){
    year <- years[j]
    
    rosterURL <- paste("https://www.basketball-reference.com/teams/", team,"/", year, ".html", sep = "")
    rosterHTML <- read_html(rosterURL)
    
    roster <- html_nodes(rosterHTML, "tr :nth-child(2)")
    roster <- html_text(roster)
    roster <- gsub("\\(TW\\)", "", roster)
    roster <- str_trim(roster)
    roster <- roster[2:length(roster)]
    roster <- stri_remove_empty(roster)
    
    playerslist <- c(as.character(playerslist),roster)
  }
}

##################################################################
#creates list of current players

tmlst3 <- html_text(html_nodes(read_html(tmlstURL), "td:nth-child(1)"))
tmlst3 <- gsub("\n", "", tmlst3)
tmlst3 <- tmlst3[2:31]
tmlst3[2] <- "BRK"
tmlst3[4] <- "CHO"
tmlst3[24] <- "PHO"

current_players <- c()
for(a in 1:length(tmlst3)){
  team <- tmlst3[a]
  rosterURL <- paste("https://www.basketball-reference.com/teams/", team,"/", "2020", ".html", sep = "")
  rosterHTML <- read_html(rosterURL)
  roster <- html_nodes(rosterHTML, "tr :nth-child(2)")
  roster <- html_text(roster)
  roster <- gsub("\\(TW\\)", "", roster)
  roster <- str_trim(roster)
  roster <- roster[2:length(roster)]
  roster <- stri_remove_empty(roster)
  current_players <- c(current_players,roster)
}

playerslist <- playerslist[2:length(playerslist)]
playerslist <- gsub("\\*", "", playerslist)
playerslist <- unique(playerslist)
###################################################
#introduces function for active years lookup by team

GetYearsByTeam <- function(team){
  if(team %in% tmlst_stable){
    return(as.character(seq(from=1985, to=2020, by=1)))
  }
  else if(team == "KCK"){
    return(as.character(1985))
  }else if(team == "SAC"){
    return(as.character(seq(from=1986, to=2020,by=1)))
  }else if(team == "WSB"){
    return(as.character(seq(from=1985,to=1997, by=1)))
  }else if(team == "WAS"){
    return(as.character(seq(from=1998,to=2020, by=1)))
  }else if(team == "SEA"){
    return(as.character(seq(from=1985,to=2008,by=1)))
  }else if(team == "OKC"){
    return(as.character(seq(from=2009,to=2020,by=1)))
  }else if(team == "NJN"){
    return(as.character(seq(from=1985,to=2012,by=1)))
  }else if(team == "BRK"){
    return(as.character(seq(from=2013,to=2020,by=1)))
  }else if(team =="CHH"){
    return(as.character(seq(from=1989, to=2002,by=1)))
  }else if(team == "CHA"){
    return(as.character(seq(from=2005, to=2014, by=1)))
  }else if(team == "CHO"){
    return(as.character(seq(from=2015,to=2020,by=1)))
  }else if(team == "MIA"){
    return(as.character(seq(from=1989,to=2020,by=1)))
  }else if(team == "ORL"){
    return(as.character(seq(from=1990,to=2020,by=1)))
  }else if(team == "MIN"){
    return(as.character(seq(from=1990,to=2020,by=1)))
  }else if(team == "TOR"){
    return(as.character(seq(from=1996,to=2020,by=1)))
  }else if(team == "VAN"){
    return(as.character(seq(from=1996,to=2001,by=1)))
  }else if(team == "MEM"){
    return(as.character(seq(from=2002,to=2020,by=1)))
  }else if(team == "NOH"){
    return(as.character(c(2003,2004,2005,2008,2009,2010,2011,2012,2013)))
  }else if(team == "NOK"){
    return(as.character(c(2006,2007)))
  }else if(team == "NOP"){
    return(as.character(seq(from=2014,to=2020,by=1)))
  }else return(0)
}

###########################################
#filters out college names erroneously introduced to player list
colleges <- c("Florida Gulf Coast", "SMU", "Nevada", "DePaul", "Duke", "University of South Dakota", "Villanova",
              "Kentucky", "Missouri", "Arizona", "Arizona State", "Houston", "Iowa State", "UNLV", "La Salle", 
              "UNC", "St. John's", "Tulsa", "Colorado", "Baylor", "Washington", "Gonzaga", "Syracuse", "Kansas", "Miami (FL)", 
              "Georgia", "Oregon", "Marquette", "UCLA", "New Mexico State", "Arkansas", "Michigan", "Oakland", "Louisville",
              "University of Indianapolis")

playerslist <- playerslist[!(playerslist %in% colleges)]
current_players <- current_players[!(current_players %in% colleges)] 
#######################################################################


num_nodes <- length(playerslist)
socmatrix <- matrix(rep(0, num_nodes*num_nodes), # edge values
         nrow = num_nodes, #nrow must be same as ncol
         ncol = num_nodes)

row.names(socmatrix) <- playerslist
colnames(socmatrix) <- playerslist


for(i in 1:length(tmlst2)){
  team <- tmlst2[i]
  years <- GetYearsByTeam(team)
  for(j in 1:length(years)){
    year <- years[j]
    rosterURL <- paste("https://www.basketball-reference.com/teams/", team,"/", year, ".html", sep = "")
    rosterHTML <- read_html(rosterURL)
    roster <- html_nodes(rosterHTML, "tr :nth-child(2)")
    roster <- html_text(roster)
    roster <- gsub("\\(TW\\)", "", roster)
    roster <- str_trim(roster)
    roster <- roster[2:length(roster)]
    roster <- stri_remove_empty(roster)
    roster <- roster[roster %in% playerslist]
    
    for(k in 1:length(roster)){
      for(m in 1:length(roster)){
        socmatrix[match(roster[k], playerslist), match(roster[m],playerslist)] <- 1
        #socmatrix[roster[k], roster[m]] <- 1
      }
    }
  }
}

#net <- as.network(x = socmatrix, # the network object
#                  directed = F, # specify whether the network is directed
#                  loops = FALSE, # do we allow self ties (should not allow them)
#                  matrix.type = "adjacency" # the type of input
#)
#network.vertex.names(net) <- playerslist
#pdf("Network_Plot_1.pdf", width = 10, height = 10)
#plot.network(net)
#dev.off()

gph1 <- graph_from_adjacency_matrix(socmatrix, mode = "undirected", diag = F)
#jordan_number <- all_shortest_paths(gph1, from = V(gph1), to = "Michael Jordan")
#jordan_number$nrgeo

#########################################################################

jordan_number_a <- all_shortest_paths(gph1, from= current_players, to = "Michael Jordan")
jordan_number_a$nrgeo[which(playerslist %in% current_players == TRUE)]
playerslist[which(playerslist %in% current_players == TRUE)]

 player_jordan_numbers <- rep(-1, length(current_players))

 for(b in 1:length(current_players)){
  jordan_number_a <- all_shortest_paths(gph1, from= current_players[b], to = "Michael Jordan")
  player_jordan_numbers[b] <- length(as.character(jordan_number_a$res[[1]])) -1
}

#use format length(as.character(leRondo$res[[1]])) -1

JN_table <- cbind(current_players, player_jordan_numbers)
write.csv(JN_table, file = "/Users/ian/Documents/JNtable.csv")


 