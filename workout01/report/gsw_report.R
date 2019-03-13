

library(dplyr)


effective_player <- total %>%
  select(player,shot_made_flag,shot_type) %>%
  group_by(player) %>%
  summarise(total=length(shot_made_flag),
            made=length(shot_made_flag[which(shot_made_flag == "shot_yes")]),
            perc_made=length(shot_made_flag[which(shot_made_flag=="shot_yes")])/length(shot_made_flag)) %>%
  arrange(desc(perc_made))

sink(file = "./output/effective-player-summary.txt")
effective_player
sink()

two_pt_effective_player <- total %>% 
  filter(shot_type == "2PT Field Goal") %>%
  select(player,shot_made_flag) %>%
  group_by(player) %>%
  summarise(
    total = length(player),
    made = length(which(shot_made_flag == "shot_yes")),
    perc_made = made / total
  ) %>%
  arrange(desc(perc_made))


sink(file = "./output/two-pt-effective-player-summary.txt")
two_pt_effective_player
sink()


three_pt_effective_player <- total %>% 
  filter(shot_type == "3PT Field Goal") %>%
  select(player,shot_made_flag) %>%
  group_by(player) %>%
  summarise(
    total = length(player),
    made = length(which(shot_made_flag == "shot_yes")),
    perc_made = made / total
  ) %>%
  arrange(desc(perc_made))

sink(file = "./output/three-pt-effective-player-summary.txt")
three_pt_effective_player
sink()