# Packages ----

library(broom)
library(ggdark)
library(ggrepel)
library(recipes)
library(spotifyr)
library(textrecipes)
library(tidyverse)
library(umap)

Sys.setenv(SPOTIFY_CLIENT_ID = "a905fab479a848579b4a190f33e57aa6")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "02c30b0b7def484b871779e6c0359704")

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
  mutate(my_ranking = row_number()) %>% 
  rename(
    song = name,
    song_id = id
  ) %>%
  mutate(artist = map_chr(artists, combine_artists)) %>%
  select(song_id, song, artist, popularity, my_ranking)

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
  count(word, sort = TRUE)

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

kmeans_mapper <- function(data, centers = 3) {
  
  data %>%
    kmeans(centers = centers, nstart = 100)
  
}

set.seed(2020)
kmeans_artists_mapped_tbl <- tibble(centers = 1:15) %>% 
  mutate(k_means = centers %>% map(kmeans_mapper, data = artists_genre_processed_tbl, rm_col = name),
         glance = k_means %>% map(glance))

kmeans_artists_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  ggplot(aes(x = centers, y = tot.withinss)) +
  geom_point(color = "#1DB954", size = 4) +
  geom_line(color = "#1DB954") +
  theme_minimal() +
  labs(title = "Elbow plot",
       subtitle = "Measures the distance that each of the customers are from the K-Means center",
       caption = "Conclusion: Based on the elbow plot, we are going to select four clusters")

set.seed(2020)
umap_artists_obj <- artists_genre_processed_tbl %>% 
  select(-name) %>% 
  umap()

umap_artists_results_tbl <- umap_artists_obj$layout %>% 
  as_tibble() %>% 
  set_names(c("x", "y")) %>% 
  bind_cols(artists_genre_processed_tbl %>% 
              select(name))

kmeans_artists_obj <- kmeans_artists_mapped_tbl %>% 
  pull(k_means) %>% 
  pluck(4)

kmeans_clusters_artists_tbl <- kmeans_artists_obj %>% 
  augment(artists_genre_processed_tbl) %>% 
  select(name, .cluster)

umap_kmeans_results_artists_tbl <- umap_artists_results_tbl %>% 
  left_join(kmeans_clusters_tbl,
            by = "name")

umap_kmeans_results_artists_tbl %>%
  mutate(
    label_text = str_glue("Artist: {name}\nGenre: {genre}")
  ) %>% 
  ggplot(aes(x, y, color = .cluster)) +
  geom_point() +
  geom_label_repel(aes(label = label_text), size = 3) +
  dark_theme_minimal() +
  scale_color_manual(values = c("#1DB954", "white", "#b3b3b3", "#535353")) +
  labs(
    title = "Grouping My Top Artists by Genres",
    x = "",
    y = ""
  ) +
  theme(legend.position = "none")

# Clustering Top Songs ----

my_top_songs_features_tbl <- my_top_songs_cleaned_tbl %>%
  mutate(audio_features = map(song_id, get_track_audio_features)) %>% 
  unnest(audio_features) %>%
  mutate(duration_m = duration_ms / 1000 / 60) %>% 
  select(-contains("id"), -uri, -track_href, -analysis_url, -type, -duration_ms)

my_top_songs_features_tbl %>% 
  pivot_longer(cols = danceability:duration_m, names_to = "feature", values_to = "measure") %>% 
  ggplot(aes(x = measure)) +
  geom_histogram() +
  facet_wrap(~ feature, scales = "free")

my_top_songs_features_tbl %>% 
  select(song, artist, energy, valence) %>% 
  mutate(plot_name = str_c(song, artist, sep = "\n")) %>% 
  ggplot(aes(x = valence, y = energy, label = plot_name)) +
  geom_point(color = "#1DB954") +
  geom_vline(xintercept = 0.5, color = "black") +
  geom_hline(yintercept = 0.5, color = "black") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

song_features_processed_tbl <- recipe(song + artist ~ ., data = my_top_songs_features_tbl) %>% 
  step_rm(time_signature, my_ranking, duration_m, popularity, instrumentalness, key) %>% 
  step_normalize(all_numeric()) %>%
  prep() %>% 
  juice()

set.seed(1234)
kmeans_songs_mapped_tbl <- tibble(centers = 1:15) %>% 
  mutate(k_means = centers %>% map(kmeans_mapper, data = song_features_processed_tbl %>% select(-song, -artist)),
         glance = k_means %>% map(glance))

kmeans_songs_mapped_tbl %>%
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  ggplot(aes(x = centers, y = tot.withinss)) +
  geom_point(color = "#1DB954", size = 4) +
  geom_line(color = "#1DB954") +
  theme_minimal() +
  labs(title = "Elbow plot",
       subtitle = "Measures the distance that each of the songs are from the K-Means center",
       caption = "Conclusion: Based on the elbow plot, we are going to select six clusters")

set.seed(1234)
umap_songs_obj <- song_features_processed_tbl %>% 
  select(-song, -artist) %>% 
  umap()

umap_songs_results_tbl <- umap_songs_obj$layout %>% 
  as_tibble() %>%
  set_names(c("x", "y")) %>% 
  bind_cols(song_features_processed_tbl %>% 
              select(song, artist))

kmeans_songs_obj <- kmeans_songs_mapped_tbl %>% 
  pull(k_means) %>% 
  pluck(6)

kmeans_clusters_songs_tbl <- kmeans_songs_obj %>% 
  augment(song_features_processed_tbl) %>% 
  select(song, artist, .cluster)

umap_kmeans_results_songs_tbl <- umap_songs_results_tbl %>% 
  left_join(kmeans_clusters_songs_tbl,
            by = c("song", "artist"))

umap_kmeans_results_songs_tbl %>%
  mutate(
    label_text = str_glue("Song: {song}\nArtist: {artist}\nCluster: {.cluster}")
  ) %>% 
  ggplot(aes(x, y, color = .cluster)) +
  geom_point() +
  geom_label_repel(aes(label = label_text), 
                   size = 2, 
                   fill = "black") +
  dark_theme_minimal() +
  labs(
    title = "Grouping My Top Tracks By Audio Features",
    x = "",
    y = ""
  ) +
  theme(legend.position = "none")
