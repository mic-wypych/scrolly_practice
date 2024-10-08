library(tidyverse)
library(showtext)

font_add_google("Ubuntu")
showtext_auto()
data <- showtextdata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-08/most_visited_nps_species_data.csv')

#make custom theme (fonts, colros etc for the plot)

#palette option one
my_pal <- c("#bc6c25", "#283618", "#dda15e", "#606c38")

#palette option two
my_pal_alt <- c("#6f1d1b", "#bb9457", "#99582a", "#432818")

#custom theme
my_theme <- function() {
  #requires Lato and Ubuntu fonts
  theme_minimal(base_family = "Ubuntu", base_size = 15) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linewidth = .1, linetype = "dashed", colour = "grey80"),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold", size = 25, family = "Ubuntu", hjust = .5),
          plot.subtitle = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(fill = NA, color = NA),
          strip.clip = "clip",
          plot.title.position = "plot",
          legend.position = "top",
          legend.key.size = unit(.4, "cm"),
          legend.title = element_blank(),
          legend.text = element_text(size = 10, family = "Ubuntu"),
          legend.margin = margin(0,0,0,0))
}

#main plot
main <- data %>%
  drop_na(ParkName, CategoryName, Occurrence) %>%
  group_by(ParkName) %>%
  count(CategoryName, Occurrence) %>%
  mutate(ParkName = str_remove(ParkName, " National Park")) %>%
  ggplot(aes(x = ParkName, y = n, fill =Occurrence)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = my_pal_alt) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~CategoryName) +
  coord_flip() +
  labs(title = "Category Occurrence in National Parks", x = NULL, y = NULL) +
  my_theme()
main
ggsave("img/main.png", main, width = 6, height = 4.5)


data %>%
  filter(ParkName == "Acadia National Park") %>%
  count(CategoryName, Occurrence) %>%
  ggplot(aes(x = CategoryName, y = n, fill = Occurrence)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = my_pal_alt) +
  coord_flip() +
  labs(title = "Category Occurrence in Acadia National Park", x = NULL) +
  my_theme()

#wooho we got the plot
get_plot <- function(parkname) {
  data %>%
    filter(ParkName == parkname) %>%
    select(ParkName, CategoryName, Occurrence) %>%
    drop_na() %>%
    count(CategoryName, Occurrence) %>%
    ggplot(aes(x = CategoryName, y = n, fill = Occurrence)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = my_pal_alt) +
    coord_flip() +
    labs(title = glue::glue("{parkname}"), x = NULL, y = NULL) +
    my_theme()
}

get_plot("Olympic National Park")

parknames <- unique(data$ParkName)

for(i in parknames) {
  temp <- get_plot(i)
  filename <- str_replace_all(i, " ", "_")
  ggsave(glue::glue("img/{filename}.png"), temp, width = 3, height = 3)
  
}


## making a map of National Parks

