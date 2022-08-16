#Load Libraries
library(readr)
library(dplyr)
library(sqldf)
library(stringi)
library(stringr)
library(ggplot2)
#get data
setwd("~/Desktop")
wrk_dir<-getwd()
all_pokemon <- read.csv(paste0(wrk_dir,"/all_pokemon.txt"))
##Call on Kanto or Jhoto Region
region <- readline(prompt="Analyze Kanto or Jhoto Pokemon? ")
region_pokemon <- if (region == 'Kanto') {all_pokemon %>% filter(X.<=151)
                  } else {all_pokemon %>% filter(X.>151 & X. <= 251)}
                  
##Create OG pokemon dataset
pokemon <- sqldf("SELECT * FROM region_pokemon
                      WHERE Name NOT LIKE '%Mega%'   AND
                            Name NOT LIKE '%Alolan%' AND
                            Name NOT LIKE '%Partner%'   ")

# pokemon <- pokemon %>% rename("Special Attack" = Sp..Atk,
#                       "Special Defense" = Sp..Def)
###R way of doing above
##identify new school ishes
# new_school<- c("Mega","Alolan","Partner")
# pokemon <- kanto_pokemon[!grepl(new_school[1], kanto_pokemon$Name),]
# pokemon <- pokemon[!grepl(new_school[2], pokemon$Name),]
# pokemon <- pokemon[!grepl(new_school[3], pokemon$Name),]

##Create type count pivot table
type_count <- pokemon %>% group_by(Type) %>% summarize(count = n()) %>% arrange(desc(count))

###check on total
#bulbasaur <- head(pokemon,1)
#sum(select(bulbasaur,c(5:10)))

##Table for pokemon stats by Type
pivot <- pokemon %>% group_by(Type) %>% summarize(count = n(), 
                                                     median(Total),
                                                     median(HP),
                                                     median(Attack),
                                                     median(Defense),
                                                     median(Sp..Atk),
                                                     median(Sp..Def),
                                                     median(Speed))
##round up on numbers
ceil <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  ifelse(!is.na(x_num),
         as.character(ceiling(x_num)),
         as.character(x))
}

pivot <- transmute_all(pivot, ceil)
#Create color index
colors=c("#94BC4A",
         "#6A7BAF",
         "#E5C531",
         "#E397D1",
         "#CB5F48",
         "#EA7A3C",
         "#7DA6DE",
         "#846AB6",
         "#71C558",
         "#CC9F4F",
         "#70CBD4",
         "#AAB09F",
         "#B468B7",
         "#E5709B",
         "#B2A061",
         "#89A1B0",
         "#539AE2"
)
##Dark Type append for Jhoto since there are no Dark Type Pokemon in Kanto
if (region == 'Jhoto') {colors <-append(colors,"#736C75",1)}


######Create Graphs

bar_type_total <- ggplot(type_count,aes(x = Type,y=count, fill = Type))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=colors)+
  ggtitle("Number of Pokemon by Type")
bar_type_total
#
# ##Median Total
bar_total <- ggplot(pivot,aes(x = Type,y=`median(Total)`, fill = Type))+
                    geom_bar(stat='identity')+
                    scale_fill_manual(values=colors)+
                    ggtitle("Median of Total Power Points by Type") +
                    xlab("") + ylab("Total PP Median")
bar_total
#
##Median Health Points
bar_hp <- ggplot(pivot,aes(x = Type,y=`median(HP)`, fill = Type))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=colors)+
  ggtitle("Median of Health Power Points by Type") +
  xlab("") + ylab("HP Median")
bar_hp
#
##Median Attack
bar_attack <- ggplot(pivot,aes(x = Type,y=`median(Attack)`, fill = Type))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=colors)+
  ggtitle("Median of Attack Power Points by Type") +
  xlab("") + ylab("Attack PP Median")
bar_attack

##Median Defense
bar_defense <- ggplot(pivot,aes(x = Type,y=`median(Defense)`, fill = Type))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=colors)+
  ggtitle("Median of Defense Power Points by Type") +
  xlab("") + ylab("Defense PP Median")
bar_defense
#
##Median Special Attack
bar_spatk <- ggplot(pivot,aes(x = Type,y=`median(Sp..Atk)`, fill = Type))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=colors)+
  ggtitle("Median of Special Attack Power Points by Type") +
  xlab("") + ylab("Sp Attack PP Median")
bar_spatk
#
##Median Speed
bar_spdef <- ggplot(pivot,aes(x = Type,y=`median(Sp..Def)`, fill = Type))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=colors)+
  ggtitle("Median of Special Defense Power Points by Type") +
  xlab("") + ylab("Sp Defense PP Median")
bar_spdef
#
##Median Speed
bar_speed <- ggplot(pivot,aes(x = Type,y=`median(Speed)`, fill = Type))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=colors)+
  ggtitle("Median of Speed Power Points by Type") +
  xlab("") + ylab("Speed PP Median")
bar_speed

# ###################Pokedex Type Beat#############


#####get user's pokemon
user_pokemon <- readline(prompt="Choose your pokemon: ")
##solves case insensitivity
user_pokemon <- str_to_sentence(user_pokemon)
while (!(user_pokemon %in% pokemon$Name)) 
  {user_pokemon <- readline(prompt="Please choose a pokemon from your selected region: ")
if (str_to_sentence(user_pokemon) %in% pokemon$Name)
{break}
}

user_pokemon <- pokemon %>% filter(grepl(user_pokemon,Name,ignore.case = TRUE))


#Takes unique record - pokemon with more than one type would output two equivalent percentiles
user_pokemon <- user_pokemon[1,]
if (is.na(user_pokemon$Name)==TRUE)  {stop()}

##Get stat of pokemon
stats <- c('Total', 'HP', 'Attack', 'Defense', 'Sp..Atk', 'Sp..Def', 'Speed')
#############if statement on stats
user_pokemon_stat <- readline(prompt=paste0("Which of ",user_pokemon$Name,"'s stats would you like to view?\nPlease enter one of the following stats as displayed below.\n",paste(stats,collapse = " | ")))
while (!(user_pokemon_stat %in% stats))
  {user_pokemon_stat <- readline(prompt=paste0("Please pick a correct stat\n",paste(stats,collapse = " | ")))

if (user_pokemon_stat %in% stats)
{break}
}
user_pokemon_stat <- grep(user_pokemon_stat,names(user_pokemon),ignore.case=TRUE, value = TRUE)
##Grabs stat from pokemon df to create histogram
prep1 <- pokemon %>% select(grep(user_pokemon_stat,names(pokemon)))
prep1 <- sort(unlist(prep1))

##Grabs user's pokemon's desired stat
prep2 <- user_pokemon %>% select(all_of(user_pokemon_stat))
prep2 <- sort(unlist(prep2))

##Gets pokemon's stat's percentile relative to og dataframe
total_percentile <- ecdf(prep1)
input_percentile <- format(round(total_percentile(prep2),2), nsmall = 2)

# plot histogram
hist <- qplot(prep1,
              geom="histogram",
              main = paste0('Histogram of ',user_pokemon_stat,' pp\n',user_pokemon$Name,
                            ' is in the ',input_percentile,' percentile'),
              xlab = paste0(user_pokemon_stat,' pp'),
              ylab = 'Count of Pokemon',
              fill=I("blue"),
              col=I("red"),
              binwidth = 5,
              alpha=I(.2)) +
  geom_vline(aes(xintercept=prep2,
                 color=I("blue")),
             linetype="solid",
             size=1,
             show.legend=T) +
  scale_colour_manual(name = "Chosen Pokemon",
                      labels =c(user_pokemon$Name),
                      values=c("blue"))

hist

