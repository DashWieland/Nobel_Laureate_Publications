library(tidyverse) 
library(udpipe)
library(ggthemes)
library(patchwork)

# Load data, note this is from Kaggle, not the TidyTuesday repository
# https://www.kaggle.com/nobelfoundation/nobel-laureates
nobel_winners <- read.csv(here::here("archive.csv"))

# Run the line below if this is your first time using udpipe
# model <- udpipe_download_model(language = "english")
# Load the model
udmodel_english <- udpipe_load_model(here::here("english-ewt-ud-2.3-181115.udpipe"))

# Split the nobel winners into categories by subject matter
# There is almost certainly a better way to do this
che <- nobel_winners %>%
  filter(Category == "Chemistry")
eco <- nobel_winners %>%
  filter(Category == "Economics")
lit <- nobel_winners %>%
  filter(Category == "Literature")
med <- nobel_winners %>%
  filter(Category == "Medicine")
pea <- nobel_winners %>%
  filter(Category == "Peace")
phy <- nobel_winners %>%
  filter(Category == "Physics")

# Create annotations for each category's dataframe using the udmodel loaded 
# earlier 
che <- as_tibble(udpipe_annotate(udmodel_english, che$Motivation))
eco <- as_tibble(udpipe_annotate(udmodel_english, eco$Motivation))
lit <- as_tibble(udpipe_annotate(udmodel_english, lit$Motivation))
med <- as_tibble(udpipe_annotate(udmodel_english, med$Motivation))
pea <- as_tibble(udpipe_annotate(udmodel_english, pea$Motivation))
phy <- as_tibble(udpipe_annotate(udmodel_english, phy$Motivation))


# Run the Rake keyword extraction algorithm on each dataframe's motivation
# column
che <- as_tibble(keywords_rake(x = che, term = "lemma", group = "doc_id",
                               ngram_max = 3, relevant = che$upos %in% c("NOUN", "ADJ")))
eco <- as_tibble(keywords_rake(x = eco, term = "lemma", group = "doc_id",
                              ngram_max = 3, relevant = eco$upos %in% c("NOUN", "ADJ")))
lit <- as_tibble(keywords_rake(x = lit, term = "lemma", group = "doc_id",
                               ngram_max = 3, relevant = lit$upos %in% c("NOUN", "ADJ")))
med <- as_tibble(keywords_rake(x = med, term = "lemma", group = "doc_id",
                               ngram_max = 3, relevant = med$upos %in% c("NOUN", "ADJ")))
pea <- as_tibble(keywords_rake(x = pea, term = "lemma", group = "doc_id",
                               ngram_max = 3, relevant = pea$upos %in% c("NOUN", "ADJ")))
phy <- as_tibble(keywords_rake(x = phy, term = "lemma", group = "doc_id",
                               ngram_max = 3, relevant = phy$upos %in% c("NOUN", "ADJ")))


che$key <- factor(che$keyword, levels = rev(che$keyword))
eco$key <- factor(eco$keyword, levels = rev(eco$keyword))
lit$key <- factor(lit$keyword, levels = rev(lit$keyword))
med$key <- factor(med$keyword, levels = rev(med$keyword))
pea$key <- factor(pea$keyword, levels = rev(pea$keyword))
phy$key <- factor(phy$keyword, levels = rev(phy$keyword))

che_top_ten <- che %>% head(10) 
eco_top_ten <- eco %>% head(10)
lit_top_ten <- lit %>% head(10)
med_top_ten <- med %>% head(10)
pea_top_ten <- pea %>% head(10)
phy_top_ten <- phy %>% head(10)

# Top keywords for che
p1 <- ggplot(data = che_top_ten) +
  aes(x = reorder(keyword, rake), weight = rake) +
  geom_bar(fill = '#39486b') +
  labs(title = 'Chemistry Motivation Keywords',
       x = 'n-gram') +
  ylim(0,6) + 
  coord_flip() + 
  theme_fivethirtyeight() + 
  theme(plot.title = element_text(size = 12,
                                  hjust = 0))

# Top keywords for eco
p2 <- ggplot(data = eco_top_ten) +
  aes(x = reorder(keyword, rake), weight = rake) +
  geom_bar(fill = '#39486b') +
  labs(title = 'Economics Motivation Keywords',
       x = 'n-gram') +
  ylim(0,6) + 
  coord_flip() + 
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0))

# Top keywords for lit
p3 <- ggplot(data = lit_top_ten) +
  aes(x = reorder(keyword, rake), weight = rake) +
  geom_bar(fill = '#39486b') +
  labs(title = 'Literature Motivation Keywords',
       x = 'n-gram') +
  ylim(0,6) + 
  coord_flip() + 
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0))

# Top keywords for med
p4 <- ggplot(data = med_top_ten) +
  aes(x = reorder(keyword, rake), weight = rake) +
  geom_bar(fill = '#39486b') +
  labs(title = 'Medicine Motivation Keywords',
       x = 'n-gram') +
  ylim(0,6) + 
  coord_flip() + 
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0))

# Top keywords for pea
p5 <- ggplot(data = pea_top_ten) +
  aes(x = reorder(keyword, rake), weight = rake) +
  geom_bar(fill = '#39486b') +
  labs(title = 'Peace Motivation Keywords',
       x = 'n-gram') +
  ylim(0,6) + 
  coord_flip() + 
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0))

# Top keywords for phy
p6 <- ggplot(data = phy_top_ten) +
  aes(x = reorder(keyword, rake), weight = rake) +
  geom_bar(fill = '#39486b')  +
  labs(title = 'Physics Motivation Keywords',
       x = 'n-gram',
       y = 'RAKE') +
  ylim(0,6) + 
  coord_flip() + 
  theme_fivethirtyeight() +
  theme(plot.title = element_text(size = 12,
                                  hjust = 0))

p_assemble <- (p1 | p2 | p3) / (p4 | p5 | p6)
p_assemble <- p_assemble + 
  plot_annotation(title = "Keywords from the motivations of Nobel Prize winners",
                  subtitle = "RAKE keyword extraction and weight. Higher scores are more likely to be a keyword. Keywords and scores are broken out by the category 
of each prize, data includes all Nobel Prize winners who included a motivation statement, 1901-2016",
                  theme = theme_fivethirtyeight())

ggsave("RAKE_nobel_keywords.png")

