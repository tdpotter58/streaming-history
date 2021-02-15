library(rjson)

test <- fromJSON(file = "input.json")
print(test)
test <- as.data.frame(test)


test <- fromJSON(file = "StreamingHistory0.json")
test <- as.data.frame(test)

library(jsonlite)
test <- fromJSON(txt = "StreamingHistory0.json")
test2 <- fromJSON(txt = "StreamingHistory1.json")
full_test <- rbind(test, test2)

library(dplyr)
library(tidyr)
full_test <- full_test %>%
  mutate(secPlayed = msPlayed / 1000)

trim <- full_test %>%
  filter(secPlayed > 60)

trim %>%
  filter(artistName == "Weezer")
library(stringr)
trim <- trim %>%
  mutate(month = str_sub(endTime,1,7))
library(ggplot2)

grouped <- trim %>%
  select(month, artistName, secPlayed) %>%
  group_by(month, artistName)  %>%
  summarise(timePlayed = sum(secPlayed))

grouped[order(-grouped$timePlayed),]

trim <- trim %>%
  mutate(artistName.f = as.factor(trim$artistName))

g <- trim %>%
  select(month, artistName, secPlayed, artistName.f) %>%
  group_by(artistName.f)  %>%
  summarise(timePlayed = sum(secPlayed))  %>%
  filter(artistName.f != "Stardust Vibes" & artistName.f != "Sol Good Sounds") %>%
  arrange(desc(timePlayed), .by_group = TRUE) %>%
  slice_max(order_by = timePlayed, n = 25)

g$artistName.f <- factor(g$artistName.f, levels = g$artistName.f[order(-g$timePlayed)])
g %>%
  ggplot(aes(x = artistName.f, y = timePlayed)) +
  geom_col(fill = 'black') +
  coord_flip()

g <- trim %>%
  select(month, artistName, secPlayed, artistName.f) %>%
  group_by(artistName.f, month)  %>%
  summarise(timePlayed = sum(secPlayed))  %>%
  filter(artistName.f != "Stardust Vibes" & artistName.f != "Sol Good Sounds") %>%
  arrange(desc(timePlayed), .by_group = TRUE) %>%
  slice_max(order_by = timePlayed, n = 20)

g %>%
  ggplot(aes(x = month, y = timePlayed, group = artistName.f)) +
  geom_bar(fill = 'black') +
  coord_flip()

g <- trim %>%
  select(month, artistName, secPlayed, artistName.f) %>%
  group_by(artistName.f)  %>%
  summarise(timePlayed = sum(secPlayed))  %>%
  filter(artistName.f != "Stardust Vibes" & artistName.f != "Sol Good Sounds") %>%
  arrange(desc(timePlayed), .by_group = TRUE) %>%
  slice_max(order_by = timePlayed, n = 8)

g$artistName.f
top_ten <- g$artistName.f

top_ten[2]
##This sort of works down here
g <- trim %>%
  filter(artistName.f == "Yellowcard" | artistName.f == "Rise Against" | artistName.f == "Neck Deep" | artistName.f =="YONAKA" | artistName.f == "With Confidence") %>%
  select(month, artistName, secPlayed, artistName.f) %>%
  group_by(artistName.f, month)  %>%
  summarise(timePlayed = sum(secPlayed))  %>%
  arrange(desc(timePlayed), .by_group = TRUE)

g %>%
  ggplot(aes(x = month, y = timePlayed, group = artistName.f, color = artistName.f)) +
  geom_line(size = 1) +
  geom_point(size = 3)

g <- trim %>%
  filter(artistName.f %in% top_ten) %>%
  select(month, artistName, secPlayed, artistName.f) %>%
  group_by(artistName.f, month)  %>%
  summarise(timePlayed = sum(secPlayed))  %>%
  arrange(desc(timePlayed), .by_group = TRUE)

g %>%
  ggplot(aes(x = month, y = timePlayed, group = artistName.f, color = artistName.f)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  scale_color_manual(values = c("brown","blue","blueviolet","coral4","cyan","wheat4","green","red","darkseagreen","grey4"))

library(viridis)

g %>%
  ggplot(aes(x = month, y = timePlayed, group = artistName.f, color = artistName.f)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  scale_color_viridis(discrete = TRUE)

library(RColorBrewer)
g %>%
  ggplot(aes(x = month, y = timePlayed, group = artistName.f, color = artistName.f)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  scale_color_brewer(palette = "Dark2")
