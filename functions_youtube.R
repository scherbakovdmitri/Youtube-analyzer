
xrayvideos=function(tmp,pattern1="чернышевск*",pattern2="достоевск*"){
  require(quanteda)
  subs.corpus=corpus(tmp,docid_field = "title"  ,text_field = "subtitle")
  
  subs.corpus.all=NULL
  subs.corpus.all=append(subs.corpus.all,list(subs.corpus))
  subs.kwic.1=kwic(subs.corpus,pattern=pattern1)
  subs.kwic.2=kwic(subs.corpus,pattern=pattern2)
  subs.kwic.2=subs.kwic.2 %>% filter(docname %in% subs.kwic.1$docname|(docname %in% sample(subs.kwic.2$docname,ifelse(nrow(subs.kwic.2)>=50,50,nrow(subs.kwic.2))))) 
  if (nrow(subs.kwic.1)>0 | nrow(subs.kwic.2)>0) print(quanteda::textplot_xray(subs.kwic.1,subs.kwic.2)) +
    theme(text = element_text(size = 10))+
    ggplot2::ggtitle(label=paste0("Как часто упоминается ",pattern1," и ",pattern2), subtitle="Встречаемость терминов в разрезе хронометража передачи") 
}

vidcloud=function(vids,pattern="чернышевск"){ 
  chern.all=corpus(vids,docid_field = "title"  ,text_field = "subtitle")[which(str_detect(
    corpus(vids,docid_field = "title"  ,text_field = "subtitle")
    ,gsub('\\*','',pattern)))] %>% 
    str_extract_all("(\\w+ ){0,20}чернышевс.(\\w+ ){0,20}") 
  if (length(chern.all)>0)
    print(quanteda::textplot_wordcloud(max_words=200,min_count=1,dfm(chern.all %>% unlist,remove=c(pattern,4,"это","который",stopwords(language = "russian",source="stopwords-iso")))))
}


my.get_playlist_items=function (filter = NULL, part = "contentDetails", max_results = 50, offset=0,
                                video_id = NULL, page_token = NULL, simplify = TRUE, ...) 
{
  print("max res:")
  print(max_results)
  if (max_results < 0) {
    stop("max_results only takes a value between 0 and 50.")
  }
  if (!(names(filter) %in% c("item_id", "playlist_id"))) {
    stop("filter can only take one of values: item_id, playlist_id.")
  }
  if (length(filter) != 1) 
    stop("filter must be a vector of length 1.")
  translate_filter <- c(item_id = "id", playlist_id = "playlistId")
  yt_filter_name <- as.vector(translate_filter[match(names(filter), 
                                                     names(translate_filter))])
  names(filter) <- yt_filter_name
  querylist <- list(part = part, maxResults = max_results, pageToken = page_token, videoId = video_id)
  querylist <- c(querylist, filter)
  print('querylist')
  print(querylist)
  res <- my.tuber_GET("playlistItems", querylist)
  print('res')
  print(res)
  if (simplify == TRUE) {
    res2 <- do.call(rbind, lapply(unlist(res[which(names(res) == 
                                                     "items")], recursive = FALSE), as.data.frame, stringsAsFactors = FALSE))
  }
  if (max_results > 50) {
    page_token <- res$nextPageToken
    while (is.character(page_token)&nrow(res2)<=max_results) {
      a_res <- my.tuber_GET("playlistItems", list(part = part, 
                                                  playlistId = unname(filter["playlistId"]), maxResults = 50, 
                                                  pageToken = page_token))
      res <- c(res, a_res)
      page_token <- a_res$nextPageToken
      if (simplify == TRUE) {
        res2 <- do.call(rbind, lapply(unlist(res[which(names(res) == 
                                                         "items")], recursive = FALSE), as.data.frame, stringsAsFactors = FALSE))
        res2$page_token=page_token
      }
    }
  }
  if (simplify == TRUE) {
    res <- do.call(rbind, lapply(unlist(res[which(names(res) == 
                                                    "items")], recursive = FALSE), as.data.frame, stringsAsFactors = FALSE))
    res$page_token=page_token
  }
  res
}

my.tuber_GET=function (path, query) 
{
  req <- GET("https://www.googleapis.com", path = paste0("youtube/v3/", 
                                                         path), query = query, 
             httr::config(token=getOption("google_token")))
  res <- content(req)
  res
}
