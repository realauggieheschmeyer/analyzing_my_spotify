# Packages ----

library(broom)
library(ggdark)
library(ggrepel)
library(recipes)
library(spotifyr)
library(textrecipes)
library(tidyverse)
library(umap)

# Sys.setenv(SPOTIFY_CLIENT_ID = "a905fab479a848579b4a190f33e57aa6")
# Sys.setenv(SPOTIFY_CLIENT_SECRET = "02c30b0b7def484b871779e6c0359704")

access_token <- get_spotify_access_token()

# Exploring Top Artists ----

my_top_artists_tbl <- get_my_top_artists_or_tracks(
  type = "artists",
  limit = 50,
  time_range = "medium_term"
) %>% 
  as_tibble()

my_top_artists_tbl %>% 
  mutate(
    my_ranking = row_number(),
    name = name %>% as_factor() %>% fct_reorder(popularity)
  ) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = popularity, y = name)) +
  geom_col(fill = "#1DB954") +
  dark_theme_minimal() +
  labs(
    title = "My Top 10 Artists Over the Last Six Months",
    subtitle = "Ordered by Artist's Popularity",
    x = "Popularity",
    y = "Artist"
  )

# Exploring Top Songs ----

my_top_songs_tbl <- get_my_top_artists_or_tracks(
  type = "tracks",
  limit = 50,
  time_range = "medium_term"
) %>% 
  as_tibble()

combine_artists <- function(df) {
  
  df %>% 
    unnest() %>% 
    pull(name) %>% 
    str_c(collapse = " / ")
  
}

my_top_songs_cleaned_tbl <- my_top_songs_tbl %>%
  select(name, artists, popularity) %>%
  mutate(my_ranking = row_number()) %>% 
  rename(song = name) %>%
  unnest(artists) %>%
  group_by(song, popularity, my_ranking) %>%
  group_nest() %>%
  mutate(artist = map_chr(data, combine_artists)) %>%
  select(song, artist, popularity, my_ranking)

my_top_songs_cleaned_tbl %>% 
  mutate(
    plot_name = str_c(song, artist, sep = "\n"),
    plot_name = plot_name %>% as_factor() %>% fct_reorder(popularity)
  ) %>% 
  arrange(my_ranking) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = popularity, y = plot_name)) +
  geom_col(fill = "#1DB954") +
  dark_theme_minimal() +
  labs(
    title = "My Top 10 Songs Over the Last Six Months",
    subtitle = "Ordered by Song's Popularity",
    x = "Popularity",
    y = "Song"
  )

# Clustering Top Artists ----

artists_genre_long_tbl <- my_top_artists_tbl %>% 
  select(name, popularity, followers.total, genres) %>% 
  unnest(genres)

artists_genre_long_tbl %>% 
  tidytext::unnest_tokens(word, genres) %>% 
  count(word, sort = TRUE) %>% 
  View()

artists_genre_processed_tbl <- recipe(name ~ ., data = artists_genre_long_tbl) %>% 
  step_tokenize(genres) %>% 
  step_tokenfilter(genres, max_tokens = 10) %>% 
  step_tf(genres, weight_scheme = "binary") %>% 
  step_normalize(popularity, followers.total) %>%
  prep() %>% 
  juice() %>% 
  group_by(name, popularity, followers.total) %>% 
  summarize_all(max) %>% 
  ungroup() %>% 
  set_names(names(.) %>% str_replace_all("\\.", "_"))

kmeans_mapper <- function(data, rm_col, centers = 3) {
  
  rm_var <- enquo(rm_col)
  
  data %>% 
    select(- !! rm_var) %>% 
    kmeans(centers = centers, nstart = 100)
  
}

kmeans_mapped_tbl <- tibble(centers = 1:15) %>% 
  mutate(k_means = centers %>% map(kmeans_mapper, data = artists_genre_processed_tbl, rm_col = name),
         glance = k_means %>% map(glance))

kmeans_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  ggplot(aes(x = centers, y = tot.withinss)) +
  geom_point(color = "#1DB954", size = 4) +
  geom_line(color = "#1DB954") +
  theme_minimal() +
  labs(title = "Skree plot",
       subtitle = "Measures the distance that each of the customers are from the K-Means center",
       caption = "Conclusion: Based on the skree plot, we are going to select three clusters")

umap_obj <- artists_genre_processed_tbl %>% 
  select(-name) %>% 
  umap()

umap_results_tbl <- umap_obj$layout %>% 
  as_tibble() %>% 
  set_names(c("x", "y")) %>% 
  bind_cols(artists_genre_processed_tbl %>% 
              select(name))

umap_results_tbl %>% 
  ggplot(aes(x, y)) +
  geom_point() +
  geom_label_repel(aes(label = name), size = 3)

kmeans_obj <- kmeans_mapped_tbl %>% 
  pull(k_means) %>% 
  pluck(3)

kmeans_clusters_tbl <- kmeans_obj %>% 
  augment(artists_genre_processed_tbl) %>% 
  select(name, .cluster)

umap_kmeans_results_tbl <- umap_results_tbl %>% 
  left_join(kmeans_clusters_tbl,
            by = "name")

umap_kmeans_results_tbl %>%
  mutate(
    genre = case_when(
      .cluster == 1 ~ "Alternative",
      .cluster == 2 ~ "Metal/Emo",
      TRUE ~ "Pop/Rap"
    ),
    label_text = str_glue("Artist: {name}\nGenre: {genre}")
  ) %>% 
  ggplot(aes(x, y, color = .cluster)) +
  geom_point() +
  geom_label_repel(aes(label = label_text), size = 3) +
  dark_theme_minimal() +
  scale_color_manual(values = c("#1DB954", "white", "#b3b3b3")) +
  labs(
    title = "Grouping My Top Artists by Genres",
    subtitle = "Using UMAP 2D Projection with K-Means Cluster Assignment",
    caption = "Conclusion: Three groups identified using two different algorithms",
    x = "",
    y = ""
  ) +
  theme(legend.position = "none")

# Clustering Top Songs ----

my_top_songs_cleaned_tbl
