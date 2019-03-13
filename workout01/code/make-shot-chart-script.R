#title:shot-data
#description: 
#input:
#output:


library(ggplot2)
library(jpeg)
library(grid)
library(dplyr)


court_file <- "./images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

thompson_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()

iguodala_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()

green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()

durant_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()

curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()



pdf(file = "./images/klay-thompson-shot-chart.pdf",width = 6.5, height = 5)
thompson_shot_chart
dev.off()


pdf(file = "./images/andre-iguodala-shot-chart.pdf",width = 6.5, height = 5)
iguodala_shot_chart
dev.off()

pdf(file = "./images/draymond-green-shot-chart.pdf",width = 6.5, height = 5)
green_shot_chart
dev.off()

pdf(file = "./images/kevin-durant-shot-chart.pdf",width = 6.5, height = 5)
durant_shot_chart
dev.off()

pdf(file = "./images/stephen-curry-shot-chart.pdf",width = 6.5, height = 5)
curry_shot_chart
dev.off()

curry <- mutate(curry,player="curry")
iguodala <- mutate(iguodala, player = 'iguodala')
green <- mutate(green,player="green")
durant <- mutate(durant,player="durant")
thompson <- mutate(thompson,player="thompson")


total <- rbind(curry,iguodala,green,durant,thompson)
write.csv(x=total, file = "./data/shots-data.csv")

pdf(file = "./images/gsw-shot-charts.pdf",width = 6.5, height = 5)
gsw_shot_chart <- ggplot(data=total, aes(x=x,y=y)) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(color=shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: GSW Shot Charts (2016 season)') +
  theme_minimal()+
  facet_wrap(.~player)
gsw_shot_chart
dev.off()

png(filename = "./images/gsw-shot-charts.png",width = 800,height = 700)
gsw_shot_chart
dev.off()











