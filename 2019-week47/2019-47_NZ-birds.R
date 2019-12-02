
library(tidyverse)
library(tidytuesdayR)

#nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

tuesdata <- tidytuesdayR::tt_load(2019, week = 47)

votes <- tuesdata$'BOTY-votes-2019' %>%
  select(-country) %>%
  mutate(voter_id = row_number()) %>%
  pivot_longer(-c(date, hour, voter_id), names_to = "vote_rank", values_to = "bird") %>%
  mutate(vote_rank = as.integer(str_extract(vote_rank, '\\d'))) %>%
  drop_na

top_ranked_birds_v1 <-
  votes %>%
  arrange(voter_id, vote_rank) %>%
  group_by(voter_id) %>%
  top_n(-1, vote_rank) %>%
  ungroup() %>%
  count(bird, name = 'total', sort = TRUE) %>%
  top_n(10, total)


plot <-
ggplot(top_ranked_birds_v1, aes(reorder(bird, total), total)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    y = '\n Total Votes \n',
    x = '',
    title = ' \n Bird of the Year 2019  •  New Zealand',
    subtitle = 'Round 1',
    caption = '#TidyTuesday 47|2019  •  Lorena Garcia-Perez'
  ) +
  theme(
    text = element_text(family = 'Palatino'),
    panel.border = element_blank(),
    panel.background = element_rect(fill = '#f9f9fa', color = 'transparent'),
    panel.grid.major.x = element_line(size = 0.3, color = '#cccccc'),
    panel.grid.major.y = element_blank(),
    plot.background = element_rect(fill = '#f9f9fa', color = 'transparent'),
    plot.title = element_text(
      face = 'bold', hjust = 0.5, size = 20
    ),
    plot.subtitle = element_text(
      face = 'plain', hjust = 0.5, size = 15
    ),
    plot.caption = element_text(
      face = 'plain', color = '#cccccc', size = 12, hjust = 0.5
    ),
    plot.margin = margin(b = 10, r = 40, l = 10),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face = 'plain', size = 14, color = '#cccccc'),
    axis.text.y = element_text(face = 'plain', size = 14, hjust = 0),
    axis.title = element_text(
      face = 'bold', size = 14, color = '#333333'
    )
  )

ggsave("NZ-birds.png", plot,
       width = 10, height = 7)
