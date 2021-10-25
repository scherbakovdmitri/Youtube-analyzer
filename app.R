library(shiny)
library(shinyjs)
library(tidyverse)
library(rvest)
library(httr)
library(tuber)
library(lubridate)
library(plotly)
source('functions_youtube.R')
options(shiny.maxRequestSize = 200*1024^2)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Анализ YouTube"),
  h4("В этом разделе можно загрузить субтитры из YouTube канала для последующего анализа"),
  h5("После поиска отобразится таблица с 10 первыми результатами"),

    sidebarLayout(
    sidebarPanel(width=3,
      textInput("Search","Введите адрес плейлиста YouTube", value = "https://www.youtube.com/watch?v=P5lUkD2Yk9Y&list=PL66DIGaegedq6S9AoaC5yTG3ZHcI4cwxP"), 
      textInput("MaxLimit","Максимальное количество результатов", value = 50), 
      actionButton("searchButton", "Выполнить поиск", class = "btn-success"),
      br(),br(),
      hidden(downloadButton("downloadData", "Скачать данные")),
    ),
    mainPanel(
      hidden(h5('Первые 10 результатов поиска')),
      tableOutput("dftotal")
    )
    
  ),

  hr(),
  h4("В этом разделе можно анализировать сохраненные субтитры"),
  h5('знак * в запросе позволяет искать слова с разными окончаниями'),
  sidebarLayout(
    sidebarPanel(width=3,
      fileInput("file1", "Загрузить субтитры", accept = ".csv"),
      textInput("Author1","Введите термин для поиска", value = "Чернышевск*"), 
      textInput("Author2","Введите термин для поиска", value = "Достоевск*"), 
      actionButton("goButton", "Отобразить график", class = "btn-success")
      ),
    mainPanel(
      plotOutput("contents"),
      plotOutput("cloud"),
      plotOutput("compare")
      )
 
  ),

)
server <- function(input, output,session) {
  df1=data.frame()
  df.total=data.frame()
  vids=list()
  
  #### Replace XX with app secret or contact me for details if you are unsure what it is 
  yt_oauth("518171652540-5nqr1m8vlcli7kophhitj98tbkq122ij.apps.googleusercontent.com",
           "XXXXXXXXXXXXXXX",token=".httr-oauth")
  
  
  dataModal <- function(failed = FALSE) {
    
    total=0;
    playlist_id=str_match(input$Search,"list=(.*)$")[[2]]
    print(playlist_id)
    
    vids <- my.get_playlist_items(filter = c(playlist_id = playlist_id), 
                                  max_results = as.integer(input$MaxLimit),offset=0)
    total=nrow(vids)
    print(dim(vids))
    if (total==0) total_time=0;
    total=min(nrow(vids),as.integer(input$MaxLimit))
    total_time=total*1;    

    modalDialog(easyClose=T,
    span(paste0("Найдено: ",total," видео Процесс займет около ",total_time," сек., продолжить?")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok", "ОК")
    )
    )
    
  }
  
  observeEvent(input$goButton,
  {
    
    file1 <- input$file1
    ext <- tools::file_ext(file1$datapath)
    req(file1)
    validate(need(ext == "csv", "Загрузите сюда сохраненный csv файл"))
    df.1=read.csv(file1$datapath)
    df.total=df.1 %>% mutate(Источник=file1$name) 
    
  
   output$contents <- renderPlot({
     if (input$goButton>0) 
     {
       xrayvideos(df.total %>% mutate(title=paste0(publication_date,' ',title)),
                  input$Author1,input$Author2) 
      
     }
       
     })
   
   output$cloud <- renderPlot({
     if (input$goButton>0) 
     {
       vidcloud(df.total %>% mutate(title=paste0(publication_date,' ',title)),
                str_to_lower(input$Author1)) 
       
     }
     
   })
   
   output$compare <- renderPlot({
     if (input$goButton>0) 
     {
       corpus.all=quanteda::corpus(df.total%>% mutate(title=paste0(publication_date,' ',title)),
                docid_field = "title",text_field = "subtitle" )
  
       ntot=length(corpus.all)
       prop=c(
         length(which(str_detect(corpus.all,"чернышевск"))) /ntot,
         length(which(str_detect(corpus.all,"герцен"))) /ntot,
         length(which(str_detect(corpus.all,"достоевск"))) /ntot,
         length(which(str_detect(corpus.all,"добролюбов"))) /ntot,
         length(which(str_detect(corpus.all,"пушкин"))) /ntot,
         length(which(str_detect(corpus.all,"некрасов"))) /ntot,
         length(which(str_detect(corpus.all,"гоголь"))) /ntot,
         length(which(str_detect(corpus.all,"лермонтов"))) /ntot
       )  
       barplot(ylim=c(0,100),ylab="упоминаемость в процентах",col = RColorBrewer::brewer.pal(5, "Set2") ,prop*100,
               names.arg=c("Чернышевский","Герцен","Достоевский","Добролюбов","Пушкин","Некрасов","Гоголь","Лермонтов"),horiz=F)
       
       
     }
     
   })
  
  })
  
  

  
  observeEvent(input$ok,
 {
   removeModal()
  
   print('start analyze')
   mine=F;
   playlist_id=str_match(input$Search,"list=(.*)$")[[2]]
   vids <- my.get_playlist_items(filter = c(playlist_id = playlist_id), 
                                 max_results = as.integer(input$MaxLimit),offset=0)
   vid_ids <- as.vector(vids$contentDetails.videoId)
   res <- lapply(vid_ids, function(x) {get_stats(x);})
   details <- lapply(vid_ids, function(x) {get_video_details(x);})
   res_df <- do.call(what = bind_rows, lapply(res, data.frame))
   details_tot  <- data.frame(id = NA, title = NA, publication_date = NA, 
                              description = NA, channel_id = NA, channel_title = NA)
    for (p in 1:length(details)) {
     id <- details[[p]]$items[[1]]$id
     title <- details[[p]]$items[[1]]$snippet$title
     publication_date <- details[[p]]$items[[1]]$snippet$publishedAt
     description <- details[[p]]$items[[1]]$snippet$description
     channel_id <- details[[p]]$items[[1]]$snippet$channelId
     channel_title <- details[[p]]$items[[1]]$snippet$channelTitle
     detail <- data.frame(id = id, title = title, publication_date = publication_date, 
                          description = description, channel_id = channel_id, 
                          channel_title = channel_title)
     details_tot <- rbind(detail, details_tot)
   }
   res_df$url <- paste0("https://www.youtube.com/watch?v=", 
                        res_df$id)
   res_df <- merge(details_tot, res_df, by = "id")
   tt=res_df
   total=nrow(tt)
   print(dim(tt))
   if (total==0) total_time=0;
   total=min(nrow(tt),as.integer(input$MaxLimit))
   progress <- shiny::Progress$new(session, min=1, max=total)
   on.exit(progress$close())
   progress$set(message = 'Скачивание субтритров ...',
                detail = paste0('Всего результатов: ',total))
   
   
   Biser.df=tt %>% mutate(subtitle5=I(list("")))
   for (i in 1:total)
   {print(i)
     progress$set(value = i)
     caption.get=GET(paste0("https://www.youtube.com/watch?v=",Biser.df$id[i])) 
     caption.url=
       caption.get%>% str_match('https.{10,500}lang=ru') %>% 
       gsub("\\\\u0026","&",x=.) %>% gsub("\\\\","",x=.)
     if (!is.na(caption.url))  
     {
       cap=GET(caption.url)
       Biser.df[i,"subtitle"]=try(content(cap,'parsed') %>% xml2::xml_children() %>% xml2::xml_text() %>% paste0(collapse=' '))
       #this is XML subs
       Biser.df[i,"subtitle5"]=try(list(list(as.character(xml2::read_xml(content(cap,'text')) %>% xml2::xml_children()))))
     }
   }
   corBiser.df=Biser.df %>% rename(subXML=subtitle5) 
   Biser.df=Biser.df %>% filter(!stringi::stri_isempty(subtitle)) 
   Biser.df=Biser.df %>% tibble::rowid_to_column() 
   
   print(dim(Biser.df))
   df.total=Biser.df
   shinyjs::show('downloadData')
   shinyjs::show('TableTitle')
     

   
    output$dftotal <- renderTable({
    
      df.total %>% head(10) %>% 
        select("Название видео"=title,"Название канала"=channel_title,"Дата публикации"=publication_date,"Кол-во просмотров"=viewCount,"Кол-во лайков"=likeCount,"Субтитры"=subtitle) %>% 
        mutate("Субтитры"=str_sub(`Субтитры`,1,100))

    })
   
   output$downloadData <- downloadHandler(
     filename = function() {
       paste(df.total$channel_title[1],"-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       if (nrow(df.total)>0)
       write.csv(df.total %>%
                   select(title,channel_title,publication_date,viewCount,likeCount,subtitle)
                   , file)
     }
   )
                 
                 
  })
  

  
  observeEvent(input$searchButton, {
    showModal(dataModal())
  }) 
  

  
}
shinyApp(ui = ui, server = server)
