library(gifski)
library(dplyr) # subsetting data
library(ggplot2) # plotting data
library(gganimate) # animating data
library(tidyverse) # cleaning data

nba_file <- './csv/nba-top-10-career-scoring-leaders-yearly.csv'
nba_df <- read_csv(nba_file)

nba_df_1960s_random_10 <- nba_df %>% 
  filter(YearEnd >= 1960 & YearEnd <= 1969) %>% 
  sample_n(10)
nba_df_1960s_random_10

### between()----
# or: same as above but with between() method
nba_df_1960s_random_10 <- nba_df %>% 
  filter(between(YearEnd,1960,1969)) %>% 
  sample_n(10)
nba_df_1960s_random_10

# filter() for just LeBron
lebron <- nba_df %>% 
  filter(Player=="LeBron James")
lebron

nba_grouped <- nba_df %>% 
  group_by(Player) %>% 
  summarize(Top_10_Yrs=n()) %>% 
  arrange(desc(Top_10_Yrs)) 
# View() -- don't run view when you are also saving to a new object

# name the column header, so it's not just literally n()
nba_grouped %>% 
  View()

### sum()----
# get sum total of Top 10 points by year
nba_df_sum <- nba_df %>% 
  group_by(YearEnd) %>% 
  summarize(TotPts = sum(CareerPts)) %>% 
  arrange(desc(TotPts)) 

nba_df_sum %>% 
  View()

nba_2020_df <- nba_df[nba_df["YearEnd"] == 2020,]
nba_2020_df

nba_2020_2022_df <- nba_df[nba_df["YearEnd"] >= 2020,]
nba_2020_2022_df[15:25,]

# above with piping
nba_2020_df <- nba_df %>% 
  filter(YearEnd == 2020)
nba_2020_df
# it works, but the bars are vertical
# we also need some space between bars (width less than 1)
# animated bar chart race uses horizontal bars
anim <- nba_df %>% 
  ggplot(aes(x = -Rank, y = CareerPts, fill=Player)) + 
  geom_tile(aes(y=CareerPts/2, height=CareerPts), width=0.9) + 
  # coord_flip() + 
  # put the player name inside the bar in white text
  geom_text(aes(label=Player, col="white", hjust="right", nudge_y=-1000)) + 
  # put the player pts next to the bar
  # package::method() means if the package this method belongs
  # to is not loaded, load it on the fly
  geom_text(aes(label = scales::comma(CareerPts, accuracy=1)),
            hjust="left", nudge_y=1000) + 
  coord_flip(clip="off", expand=FALSE) + 
  ylab("Career Points") + 
  ggtitle("NBA All-Time Scoring Leaders") + 
  scale_x_discrete("") + 
  scale_y_continuous(limits = c(-1000, 49000),
                     labels = scales::comma) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust=0.5, size=20),
        legend.position="none",
        panel.grid.minor = element_line(linetype="dashed"),
        panel.grid.major = element_line(linetype="dashed")) + 
  transition_time(YearEnd) + 
  labs(subtitle = "Top 10 Scorers as of {round(frame_time,0)}") + 
  theme(plot.subtitle = element_text(hjust=0.5, size=12)
  )

# the big object nba_2020_2022_plot returned by ggplot is passed
# to the animate(nba_2020_2022_plot,) method as its first argument
animate(anim,
        renderer = gifski_renderer(),
        end_pause = 50,
        nframes = 12*(2022-1950), fps=12,
        width=1080, height=720, res=150)

anim_save("NBA-Top-10-Career-Scorers-1950-2022.gif")


# from FINAL File
anim <- nba_df |> 
  ggplot(aes(x = -Rank, y = CareerPts, fill = Player)) + 
  geom_tile(aes(y = CareerPts / 2, height = CareerPts), 
            width = 0.9) + 
  geom_text(aes(label = Player), col = "white", 
            hjust = "right", nudge_y = -1000) +
  # geom_text(aes(label = comma(CareerPts, accuracy = 1)),
  #           hjust = "left", nudge_y = 1000) +
  geom_text(aes(label = as.character(CareerPts)),
            hjust = "left", nudge_y = 1000) +
  coord_flip(clip = "off", expand = FALSE) + 
  ylab("Career Points") + 
  ggtitle("NBA All-Time Scoring Leaders") + 
  scale_x_discrete("") +
  scale_y_continuous(limits = c(-1000, 49000),
                     labels = scales::comma) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        legend.position = "none",
        panel.grid.minor = element_line(linetype = "dashed"),
        panel.grid.major = element_line(linetype = "dashed")) + 
  transition_time(YearEnd) +
  labs(subtitle = "Top 10 Scorers as of {round(frame_time, 0)}") + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12))

animate(anim, renderer = gifski_renderer(),
        end_pause = 50, 
        nframes = 12*(2020-1950), fps = 12,
        width = 1080, height = 720, res = 150)

anim_save("NBA-Top-10-Leading_Scorers-All-Time-1950-2022.gif")