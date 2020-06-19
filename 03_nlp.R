################################################################################
# Some NLP. Assumes 01_webscrape_site.R been run
################################################################################

library(ngrams) # for text analysis (Natural Language Processing NLP)

# bigrams -----------------------------------------------------------------

# get the titles as one long string separated by three spaces
title_text_str <- paste0(letters_tbl$title, collapse = "   ")
# remove some stopwords (manual - could use pre-defined corpus)
title_text_str <- str_remove_all(title_text_str, " on | of | to | the | and | in ")
# find all word pairs
ng2 <- ngram(title_text_str, sep = "   ",
             n = 2)
# frequency table
ng2_tbl <- get.phrasetable(ng2) %>% as_tibble()
ng2_tbl # not really any surprises there!


# wordcloud ---------------------------------------------------------------

word_freq <- title_text_str %>%
  ngram(sep = "   ", n = 1) %>%
  get.phrasetable() %>% 
  as_tibble()

word_freq
# doesn't look interesting

