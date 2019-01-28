# http://zevross.com/blog/2015/05/19/scrape-website-data-with-the-new-r-package-rvest/
# http://bradleyboehmke.github.io/2015/12/scraping-html-text.html
# http://r-pkgs.had.co.nz/description.html
# https://gohugo.io/content-management/shortcodes/
# http://blog.tonytsai.name/blog/2017-12-04-detecting-non-breaking-space-in-r/
# https://cryptii.com/pipes/hex-to-text

tg_website <- "https://www.lds.org/scriptures/tg/"

#' @title lds_topics
#' @description Function retrieves the list of topics from the topical guide on lds.org
#' @param letter a letter of the alphabet
#' @example lds_topics("a")
#' @export
lds_topics <- function(letter){
  letter <- str_to_lower(letter)
  url <- str_c("https://www.lds.org/scriptures/tg?lang=eng&letter=", letter)
  webpage <- read_html(url)
  topics <- html_nodes(webpage, '#primary')

  process_topics(topics)
}

#' @title process_topics
#' @description process topics data using the lds_topics function
#' @param topics the ouput from within the `lds_topics` function.
#' @return a tibble with three columns - 'site_text', 'topic', and 'see'
#' @details by using the url 'https://www.lds.org/scriptures/tg/' the value in the 'topic' column can be appended to get to the page with the scripture references for that topic.
process_topics <- function(topics){
  text <- topics %>%
    html_nodes('ul') %>%
    .[[2]] %>%
    html_nodes("li") %>%
    html_text() %>%
    str_remove_all("\n") %>%
    str_squish()

  text_table <- tibble(site_text = text)

  text_table <- text_table %>%
    separate(col = site_text, into = c("topic", "see"), sep = "See ",  remove = FALSE, fill = "right") %>%
    mutate(see = see %>% str_remove("also ") %>% str_trim(), topic = str_trim(topic))

  text_table

}

#' @title lds_topic
#' @description Function retrieves the verses of specific topic from the topical guide on lds.org
#' @param topic a correctly named topic from the topical guide
#' @example lds_topic("God, Omniscience of")
#' @return a tibble with five columns - verse reference from the web, book, reference, text_summary, and full_text
#' @details the reference column matches the values in `scriptures$verse_short_title`
#' @export
lds_topic <- function(topic){

  topic_url <- topic %>% str_remove_all(",") %>% str_replace_all(" ", "-")
  url <- str_c( "https://www.lds.org/scriptures/tg/", topic_url)
  webpage <- read_html(url, encoding = "UTF-8")

  ## references start with other named references that go to topical guide or BD
  references <- html_nodes(webpage, '.scripture-ref') %>% html_text()

  ref_tibble <- tibble(reference_web = references) %>%
    separate(reference_web, into = c("book", "reference"), " [1-9]{1,4}",
             extra = "drop", fill = "left", remove = FALSE) %>%
    fill(book) %>%
    mutate(reference = str_extract(reference_web, "[[:digit:]]+:[[:print:]]+"),
           reference = str_c(book, " ", reference))

    text_summary <- html_nodes(webpage, '.entry') %>% html_text()

  rcommand <- str_c(ref_tibble$reference_web, collapse = "|")

  verse_summary <- text_summary %>% str_extract_all(rcommand)

  text_tibble <- tibble(text_summary = rep(text_summary, verse_summary %>% map(length) %>% unlist()),
         reference_web = unlist(verse_summary))

  final_tibble <- ref_tibble %>%
    left_join(text_tibble) %>%
    mutate(reference = str_replace_all(reference, "\u00A0", " "),
           book = str_replace_all(book,  "\u00A0", " "),
           text_summary = str_replace_all(text_summary, "\u00A0", " ") %>%
             str_replace_all(" … ", " ... "))
  return(final_tibble)
}

#' @title lds_expand_reference
#' @description Function retrieves the verses of specific topic from the topical guide on lds.org
#' @param reference a specific scripture reference as a character string. Should use the `verse_short_title`
#' @example lds_expand_reference("1 Ne. 8:14–15, 18"); lds_expand_reference("1 Ne. 8:14-15, 18")
#' @return a character string with all text from the input verses
#' @details The function can handle `,`, `–`, or `-`. The `–` and `-` will pull all verses between the two numbers.
#' @export
lds_expand_reference <- function(reference = "1 Ne. 8:14–15, 18"){

  if( is.na(reference) ) return(NA)

  references <- reference
  front_ref <- references[1] %>% str_split(":") %>% unlist() %>% .[1]
  # To get comma separated verses
  if( str_detect(references, ",") ) {
    references <- reference %>% str_split(",") %>% unlist()
    references[-1] <- str_c(front_ref, ":", str_trim(references[-1]))
  }
  # To get sequence of verses
  if( any(str_detect(references, "–|-")) ) {
    references <- references %>% str_split("–|-")

    references <- references %>%
      map(~str_remove_all(.x, str_c(front_ref,":"))) %>%
      map(as.numeric) %>%
      map(~head(.x,1):tail(.x,1)) %>%
      map(~str_c(front_ref, ":", .x))
  }

  verses <- references %>% unlist()


  out <- scriptures %>%
    filter(verse_short_title %in% verses) %>%
    .$scripture_text %>%
    str_c(collapse = " ")
  return(out)

}

# scriptures <- read_csv("https://raw.githubusercontent.com/byuistats/M335/master/docs/data/lds-scriptures.csv?token=AF6YxPyvmohk2lj5cCXWUdE9zmoY8UTvks5cV0lgwA%3D%3D")
#
# usethis::use_data(scriptures)
