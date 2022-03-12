library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(dplyr)



server <- shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    
    # Filtering the books based on genre and rating
    df.search.criteria %>% filter(top_genre %in% as.vector(input$genre)) %>%
      group_by(title) %>% filter(bpm >= as.numeric(input$range[1]), bpm <= as.numeric(input$range[2])) %>%
      arrange(desc(bpm)) %>% 
      select(title,top_genre, bpm, popularity)
  })
  
  datasetInput2 <- reactive({

    # Filtering the books based on year and rating
    df.search.criteria %>% filter(year %in% as.vector(input$year)) %>%
      group_by(title) %>% filter(popularity >= as.numeric(input$range_2[1]), popularity <= as.numeric(input$range_2[2])) %>%
      arrange(desc(popularity)) %>%
      select(title,year, popularity)
  })
  
 

  #Rendering the table
  output$songsreco <- DT::renderDataTable({
    
    DT::datatable(head(datasetInput(), n = 50), escape = FALSE, options = list(scrollX = '1000px'))
  })
  
  
  output$songsreco_genre <- DT::renderDataTable({

    DT::datatable(head(datasetInput2(), n = 100), escape = FALSE, options = list(scrollX = '1000px'))
  })
  
  output$distPlot <- renderPlot({
    
    x    <- df.search.criteria$duration
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = "duration",
         main = "Distribution of song duration")
    
  })
  
})

  














































# similarity.plot <- function(g.sim, stud, assign, min.z.score = 1.64) {
#   edges.to.delete <- E(g.sim)[z_score < min.z.score | assignment != assign]
#   g.sim.filtered <- delete.edges(g.sim, edges.to.delete)
#   g.sim.filtered <- induced.subgraph(g.sim.filtered, neighborhood(g.sim.filtered, order = 2,
#                                                                   nodes = V(g.sim.filtered)[stud])[[1]])
#   
#   V(g.sim.filtered)$label.color <- ifelse(V(g.sim.filtered)$name %in% stud, "sienna1", "turquoise4")
#   V(g.sim.filtered)$name <- dt.student.info[V(g.sim.filtered)$name, first_name]
#   if (length(E(g.sim.filtered) > 0)) {
#     E(g.sim.filtered)$width <- sapply(E(g.sim.filtered)$z_score, function(z) max(c(1, z * 2)))
#     grays <- colorRampPalette(c("light gray", "black"))
#     E(g.sim.filtered)$color <- grays(max(E(g.sim.filtered)$width))[E(g.sim.filtered)$width]
#   }
#   plot(g.sim.filtered, 
#        vertex.shape="circle", 
#        vertex.color = "white", 
#        vertex.frame.color = "white", 
#        vertex.size = 40, 
#        vertex.label.family = "sans",
#        vertex.label.cex = 2,
#        layout = layout.circle)
# }
# 
# overall.similarity.plot <- function(g.sim, stud, assign, overall.min.z.score = 1.64) {
#   
#   edges.to.delete <- E(g.sim)[z_score < overall.min.z.score | assignment != assign]
#   g.sim.filtered <- delete.edges(g.sim, edges.to.delete)
#   V(g.sim.filtered)$color <- ifelse(V(g.sim.filtered)$name %in% stud, "sienna1", "turquoise4")
#   if (length(E(g.sim.filtered) > 0)) {
#     E(g.sim.filtered)$width <- sapply(E(g.sim.filtered)$z_score, function(z) max(c(1, z * 2)))
#     grays <- colorRampPalette(c("light gray", "black"))
#     E(g.sim.filtered)$color <- grays(max(E(g.sim.filtered)$width))[E(g.sim.filtered)$width]
#   }
#   
#   #coords <- layout.kamada.kawai(g.sim.filtered)
#   coords <- layout.fruchterman.reingold(g.sim.filtered)
#   plot(g.sim.filtered, 
#        vertex.shape="circle", 
#        vertex.size=35, 
#        vertex.frame.color = "white", 
#        vertex.label = "",
#        layout = coords, 
#        rescale = FALSE, 
#        xlim = range(coords[,1]), 
#        ylim = range(coords[,2])
#   )
# }
# 
# 
# server <- function(input, output) {
#   
#   output$student.summary <- renderUI({
#     stud <- student.from.key(input$student.key)
#     assign <- input$assignment
#     if (length(stud) > 0) {
#       if (length(similarity.score(stud, assign)) > 0) {
#         list(h2(paste0('Hello ', student.name(stud), '!')),
#              br(),
#              p('Your average similarity score for this assignment is ', 
#                strong(similarity.score(stud, assign)), '.'),
#              p('You rank ', strong(total.students(assign) -
#                                      student.rank(stud, assign) + 1),
#                ' out of ', total.students(assign), 
#                ' students in', em('originality.')),
#              p('You rank ', strong(student.rank(stud, assign)), 
#                ' out of ', total.students(assign), 
#                ' students in how similar you are to the other students.'),
#              br(),
#              h4("These are the students with assignments most similar to yours:"),
#              renderTable(similarity.table(stud, assign)))
#       } else {
#         list(h2(paste0('Hello ', student.name(stud), '!')),
#              br(),
#              p('It seems that you did not hand in this assignment.'))
#       }} else {
#         list(h2('Hey there!'),
#              br(),
#              p('It appears you have entered an invalid student key.'),
#              p('Here are some overall summary statistics for this assignment.'),
#              {
#                dt.summ.stats <-
#                  dt.similarity.pairs[assignment == assign,
#                                      list(Students          = length(unique(c(student_1, student_2))),
#                                           Comparisons       = .N,
#                                           'Avg. Similarity' = mean(similarity),
#                                           Median            = median(similarity),
#                                           'S.D.'            = sd(similarity),
#                                           Min               = min(similarity),
#                                           Max               = max(similarity))]
#                summ.stats.names <- names(dt.summ.stats)
#                dt.summ.stats <- data.table(t(dt.summ.stats))
#                setnames(dt.summ.stats, names(dt.summ.stats), "Value")
#                dt.summ.stats[, Statistic := summ.stats.names]
#                dt.summ.stats <- dt.summ.stats[, list(Statistic, Value)]
#                renderTable(dt.summ.stats)
#              },
#              renderPlot(ggplot() + 
#                           geom_histogram(aes(similarity),
#                                          data=dt.similarity.pairs[assignment == assign & z_score < 1.64],
#                                          fill="dark green", binwidth=0.01) +
#                           geom_histogram(aes(similarity),
#                                          data=dt.similarity.pairs[assignment == assign & z_score >= 1.64 & z_score < 1.96],
#                                          fill="yellow", binwidth=0.01) +
#                           geom_histogram(aes(similarity),
#                                          data=dt.similarity.pairs[assignment == assign & z_score >= 1.96 & z_score < 2.57],
#                                          fill="orange", binwidth=0.01) + 
#                           geom_histogram(aes(similarity),
#                                          data=dt.similarity.pairs[assignment == assign & z_score > 2.57],
#                                          fill="red", binwidth=0.01))
#         )
#       }
#   })
#   
#   output$student.network <- renderUI({
#     stud <- student.from.key(input$student.key)
#     assign <- input$assignment
#     g.sim <- graph.data.frame(dt.similarity.pairs, directed=FALSE)
#     if (length(stud) > 0) {
#       if (length(similarity.score(stud, assign)) > 0) {
#         list(h2(paste0('Hello ', student.name(stud), '!')),
#              br(),
#              selectInput("min.z.score", "Show links with at least the level of suspiciousness:",
#                          c(#"Median" = 0,
#                            "Hmm..." = 1.64,
#                            "Yes" = 1.96,
#                            "Very!" = 2.57)),
#              renderPlot(similarity.plot(g.sim, stud, assign, min.z.score = input$min.z.score))
#         )
#       } else {
#         list(h2(paste0('Hello ', student.name(stud), '!')),
#              br(),
#              p('It seems that you did not hand in this assignment.'))
#       }} else {
#         list(h2('Hey there!'),
#              br(),
#              p('It appears you have entered an invalid student key.'))
#       }
#   })
#   
#   output$overall.network <- renderUI({
#     stud <- student.from.key(input$student.key)
#     assign <- input$assignment
#     g.sim <- graph.data.frame(dt.similarity.pairs[order(z_score)], directed=FALSE)
#     diplay <- list()
#     if (length(stud) > 0) {
#       if (length(similarity.score(stud, assign)) > 0) {
#         display <- list(h2(paste0('Hello ', student.name(stud), '!')),
#                         br())
#       } else {
#         display <- list(h2(paste0('Hello ', student.name(stud), '!')),
#                         br(),
#                         p('It seems that you did not hand in this assignment.'))
#       }} else {
#         display <- list(h2('Hey there!'),
#                         br(),
#                         p('It appears you have entered an invalid student key.'))
#       }
#     list(display,
#          list(
#            selectInput("overall.min.z.score", "Show links with at least the level of suspiciousness:",
#                        c("Above average" = 0,
#                          "Hmm..." = 1.64,
#                          "Yes" = 1.96,
#                          "Very!" = 2.57)),
#            renderPlot(overall.similarity.plot(g.sim, stud, assign,
#                                               overall.min.z.score = input$overall.min.z.score), height=600, width=600)
#          ))
#   })
# }
# 
# server
