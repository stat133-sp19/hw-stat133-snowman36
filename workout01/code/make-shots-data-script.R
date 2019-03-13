#title:shot-data
#description: creat a csv data file that will be used in visualization
#input:
#output:

library(dplyr)

curry <- read.csv("./data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("./data/andre-iguodala.csv",stringsAsFactors = FALSE)
green <- read.csv("./data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("./data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("./data/klay-thompson.csv", stringsAsFactors = FALSE)

typeof(curry)

curry$shot_made_flag[which(curry$shot_made_flag == "y")] <- "shot_yes"
curry$shot_made_flag[which(curry$shot_made_flag == "n")] <- "shot_no"
iguodala$shot_made_flag[which(iguodala$shot_made_flag == "y")] <- "shot_yes"
iguodala$shot_made_flag[which(iguodala$shot_made_flag == "n")] <- "shot_no"
durant$shot_made_flag[which(durant$shot_made_flag == "y")] <- "shot_yes"
durant$shot_made_flag[which(durant$shot_made_flag == "n")] <- "shot_no"
thompson$shot_made_flag[which(thompson$shot_made_flag == "y")] <- "shot_yes"
thompson$shot_made_flag[which(thompson$shot_made_flag == "n")] <- "shot_no"
green$shot_made_flag[which(green$shot_made_flag == "y")] <- "shot_yes"
green$shot_made_flag[which(green$shot_made_flag == "n")] <- "shot_no"

curry <- mutate(curry, mintue = period*12 - minutes_remaining,)
iguodala <- mutate(iguodala, mintue = period*12 - minutes_remaining)
green <- mutate(green, mintue = period*12 - minutes_remaining)
durant <- mutate(durant, mintue = period*12 - minutes_remaining)
thompson <- mutate(thompson, mintue = period*12 - minutes_remaining)

sink(file = "./output/andre-iguodala-summary.txt")
summary(curry)
sink()

sink(file = "./output/draymond-green-summary.txt")
summary(green)
sink()

sink(file = "./output/kevin-durant-summary.txt")
summary(durant)
sink()

sink(file = "./output/klay-thompson-summary.txt")
summary(thompson)
sink()

sink(file = "./output/stephen-curry-summary.txt")
summary(curry)
sink()

total <- rbind(curry,durant,thompson,green,iguodala)
write.csv(x=total, file = "./data/shots-data.csv")

sink(file = "./output/shots-data-summary.txt")
summary(total)
sink()


