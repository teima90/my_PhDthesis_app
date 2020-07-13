library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)

Logged = FALSE;
my_password <- "tesis2020"


ui1 <- function(){
  tagList(
    div(id = "login",
        wellPanel(passwordInput("passwd", h5(strong("Clave"))),
                  br(),actionButton("Login", "Entrar"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui2 <- function(){"holi"}

ui =         fluidPage(
  tags$head(
    tags$style(type = "text/css",
               HTML("th { text-align: center; }")
    )
  ),
  (uiOutput("page")))

#####----SERVER!!-----##############

server = (function(input, output,session) {
  
  USER <- reactiveValues(Logged = Logged)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Password <- isolate(input$passwd)
          Id.password <- which(my_password == Password)
          if (length(Id.password) > 0) {
            if (Id.password) {
              USER$Logged <- TRUE
            } 
          }
        } 
      }
    }    
  })
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      output$page <- renderUI({
        
        navbarPage("Saura-Sanchez, 2020",
                   tabPanel("Informacion",
                            h1("Bienvenides!"),
                            br(),
                            div("Esta aplicacion se ha desarrollado como material de apoyo a la tesis doctoral", 
                                strong("Mecanismos de accion de las proteinas BBX en Arabidopsis thaliana"),
                                "de la Facultad de Farmacia y Bioquimica, Universidad de Buenos Aires.", "Espero les resulte de utilidad.",style="font-size:18px;line-height:30px"),
                            br(),
                            div(h3("Que contiene?"),
                                strong("Anexos:"),"Contiene todos los anexos referidos en el texto de la tesis.",
                                br(),
                                strong("Visualizacion de datos de expresion:"), "Aplicacion que le permite observar de manera interactiva los valores de expresion de su gen (log2FC o log2) 
                              o grupo de genes (Z-score) favoritos a lo largo
                              de los transcriptomas analizados en el capitulo II de esta tesis, asi como descargar los datos asociados.",style="font-size:15px;line-height:25px")),
                   tabPanel("Anexos",
                            h3("Anexos"),
                            br(),
                            downloadLink("anexo_a", strong("Anexo A."))," Capitulo II: Genes regulados por sombra en el genotipo WT Col-0.",
                            br(),
                            br(),
                            downloadLink("anexo_b", strong("Anexo B."))," Capitulo II: Analisis de enriquecimiento ontologico.",
                            br(),
                            br(),
                            downloadLink("anexo_c", strong("Anexo C."))," Capitulo II: Genes regulados por BBX28.",
                            br(),
                            br(),
                            downloadLink("anexo_d", strong("Anexo D."))," Capitulo II: Genes regulados por BBX28, COP1 y PIF.",
                            br(),
                            br(),
                            downloadLink("anexo_e", strong("Anexo E."))," Capitulo III: Genes regulados por BBX24 y MeJA.",
                            br(),
                            br(),
                            h3("Tablas completas de los transcriptomas analizados"),
                            br(),
                            downloadLink("anexo_bbx28", strong("Tabla 1."))," Saura-Sanchez",em("et al.,"),"2020. Datos normalizados de expresion asociados a BBX28.",
                            br(),
                            br(),
                            downloadLink("anexo_pacin", strong("Tabla 2."))," Pacin",em("et al.,"),"2016. Datos normalizados de expresion asociados a COP1 y PIFQ.",
                            br(),
                            br(),
                            downloadLink("anexo_fank", strong("Tabla 3."))," Hornitschek",em("et al.,"),"2012. Datos normalizados de expresion asociados a PIF."),
                   tabPanel("Visualizacion de datos de expresion",
                            sidebarLayout(
                              sidebarPanel(width=2,
                                           selectInput("values",label="Valor de expresion:",choices=list("Relativo al WT (log2FC)" ='2',"Absoluto (log2)"='1')),            
                                           radioButtons("enter_data",label=NULL,choices=list("Un gen"='1',"Lista de genes" ='2'),selected='1'),
                                           conditionalPanel("input.enter_data=='1'",
                                                            textInput("one_gene",label="Inserte codigo AGI:",placeholder = "AT1G06040")),
                                           conditionalPanel("input.enter_data=='2'",
                                                            textAreaInput("list_genes",label="Inserte su lista de genes:",
                                                                          height = '300px'),
                                                            actionButton("update_list","Actualizar"))
                              ),
                              mainPanel(width=10,
                                        tabsetPanel(
                                          tabPanel("Visualizacion",
                                                   fluidPage(column(3,
                                                                    h4("Saura-Sanchez, 2020"),
                                                                    conditionalPanel("input.enter_data=='1'",
                                                                                     plotOutput("barplot_bbx28"),
                                                                                     downloadButton("getbbx28barplot","Guardar .png")),
                                                                    conditionalPanel("input.enter_data=='2'",
                                                                                     plotOutput("boxplot_bbx28"),
                                                                                     downloadButton("getbbx28boxplot","Guardar .png"))
                                                   ),
                                                   column(width=4,
                                                          h4("Pacin, 2016"),
                                                          conditionalPanel("input.enter_data=='1'",
                                                                           plotOutput("barplot_pacin"),
                                                                           downloadButton("getpacinbarplot","Guardar .png")),
                                                          conditionalPanel("input.enter_data=='2'",
                                                                           plotOutput("boxplot_pacin"),
                                                                           downloadButton("getpacinboxplot","Guardar .png"))),
                                                   column(width=5,
                                                          h4("Hornitschek, 2012"),
                                                          conditionalPanel("input.enter_data=='1'",
                                                                           plotOutput("barplot_fank"),
                                                                           downloadButton("getfankbarplot","Guardar .png")),
                                                          conditionalPanel("input.enter_data=='2'",
                                                                           plotOutput("boxplot_fank"),
                                                                           downloadButton("getfankboxplot","Guardar .png"))))
                                                   
                                          ),
                                          tabPanel("Tablas",
                                                   
                                                   h4("Saura-Sanchez, 2020"),
                                                   DT::dataTableOutput('table_bbx28'),
                                                   downloadButton("getbbx28","Guardar"),
                                                   
                                                   h4("Pacin, 2016"),
                                                   DT::dataTableOutput('table_pacin'),
                                                   downloadButton("getpacin","Guardar"),
                                                   
                                                   h4("Hornitschek, 2012"),
                                                   DT::dataTableOutput('table_fank'),
                                                   downloadButton("getfank","Guardar"))
                                        )
                              )
                            )
                   )
        )
      })
      print(ui)
    }
  })
  
  output$anexo_a <- downloadHandler(
    filename = "AnexoA.xlsx",
    content = function(file) {
      file.copy("AnexoA.xlsx", file)
    }
  )
  
  output$anexo_b <- downloadHandler(
    filename = "AnexoB.xlsx",
    content = function(file) {
      file.copy("AnexoB.xlsx", file)
    }
  )
  
  output$anexo_c <- downloadHandler(
    filename = "AnexoC.xlsx",
    content = function(file) {
      file.copy("AnexoC.xlsx", file)
    }
  )
  
  output$anexo_d <- downloadHandler(
    filename = "AnexoD.xlsx",
    content = function(file) {
      file.copy("AnexoD.xlsx", file)
    }
  )
  
  output$anexo_e <- downloadHandler(
    filename = "AnexoE.xlsx",
    content = function(file) {
      file.copy("AnexoE.xlsx", file)
    }
  )
  
  output$anexo_bbx28 <- downloadHandler(
    filename = "Saura2020.txt",
    content = function(file) {
      file.copy("BBX28_data.txt", file)
    }
  )
  output$anexo_pacin <- downloadHandler(
    filename = "Pacin2016.txt",
    content = function(file) {
      file.copy("Pacin_data.txt", file)
    }
  )
  output$anexo_fank <- downloadHandler(
    filename = "Hornitschek2012.txt",
    content = function(file) {
      file.copy("Fank_data.txt", file)
    }
  )
  
  
  ##for stats
  data_summary <- function(data, varname, groupnames){
    require(plyr)
    require(plotrix)
    summary_func <- function(x, col){
      c(mean = mean(x[[col]], na.rm=TRUE),
        sem = std.error(x[[col]], na.rm=TRUE))
    }
    data_sum<-ddply(data, groupnames, .fun=summary_func,
                    varname)
    data_sum <- rename(data_sum, c("mean" = varname))
    return(data_sum)
  }
  
  cal_z_score <- function(x){
    (x - mean(x)) / sd(x)
  }
  
  #----------------------------------------------  
  #----------------DATA_SELECTION----------------
  #----------------------------------------------  
  
  ##Normalized data (Log2)
  data_bbx28_log <- read.table("BBX28_data.txt",header=TRUE)
  data_cop1_log <- read.table("Pacin_data.txt",header=TRUE)
  data_fank_log <- read.table("Fank_data.txt",header=TRUE)
  
  ##Normalized data to Col_WL (log2FC)
  
  data_bbx28_fc<- cbind(AGI=data_bbx28_log[,1],as.data.frame(t(apply(data_bbx28_log[,2:ncol(data_bbx28_log)], 1, function(x) {x - mean(x[1:3])}))))%>% 
    mutate_if(is.numeric, round,digits=2)
  data_cop1_fc<- cbind(data_cop1_log[,1:2],as.data.frame(t(apply(data_cop1_log[,3:ncol(data_cop1_log)], 1, function(x) {x - mean(x[1:3])}))))%>% 
    mutate_if(is.numeric, round,digits=2)
  data_fank_fc<- cbind(data_fank_log[,1:2],as.data.frame(t(apply(data_fank_log[,3:ncol(data_fank_log)], 1, function(x) {x - mean(x[1:3])}))))%>% 
    mutate_if(is.numeric, round,digits=2)
  
  data_bbx28 <- reactive({
    switch(input$values,
           "1" = data_bbx28_log,
           "2" = data_bbx28_fc)
  })
  
  data_cop1 <- reactive({
    switch(input$values,
           "1" = data_cop1_log,
           "2" = data_cop1_fc)
  })
  
  data_fank <- reactive({
    switch(input$values,
           "1" = data_fank_log,
           "2" = data_fank_fc)
  })
  
  
  list_genes_up <- eventReactive(input$update_list,{
    req(input$list_genes)
    genes <- input$list_genes
    genes<- as.data.frame(unlist(strsplit(genes, "\n")))
    return(genes)
  })
  
  output$text1 <- renderPrint({
    list_genes_up()
  })
  
  data_bbx28_selected <- reactive ({
    if(input$enter_data==1){
      data_bbx28 <- data_bbx28()
      data <- data_bbx28[data_bbx28$AGI==input$one_gene,]
      return(data)
    } else {
      genes <- list_genes_up()
      data_bbx28 <- data_bbx28()
      data <- subset(data_bbx28, data_bbx28$AGI %in% genes[,1])
      return(data)
    }
  })
  
  data_pacin_selected <- reactive ({
    if(input$enter_data==1){
      data_cop1 <- data_cop1()
      data <- data_cop1[data_cop1$AGI==input$one_gene,]
      return(data[,-1])
    } else {
      genes <- list_genes_up()
      data_cop1 <- data_cop1()
      data <- subset(data_cop1, data_cop1$AGI %in% genes[,1])
      return(data[,-1])
    }
  })
  
  data_fank_selected <- reactive ({
    if(input$enter_data==1){
      data_fank <- data_fank()
      data <- data_fank[data_fank$AGI==input$one_gene,]
      return(data[,-1])
    } else {
      genes <- list_genes_up()
      data_fank <- data_fank()
      data <- subset(data_fank, data_fank$AGI %in% genes[,1])
      return(data[,-1])
    }
  })
  
  
  
  #----------------------------------------------  
  #-------------DATA_VISUALIZATION---------------
  #----------------------------------------------
  
  #-------ONE_GENE---------
  
  data_bbx28_plot <- reactive({
    if(input$enter_data==1){
      data2 <- data_bbx28_selected()
      data <- as.data.frame(t(data2[,-1]))
      colnames(data)<-"exp"
      data$genotype <- c(rep("Col-0",6),rep("bbx28-5",6))
      data$treatment <- c(rep("LB",3),rep("SS",3),rep("LB",3),rep("SS",3))
      data$genotype = factor(data$genotype, levels = c("Col-0","bbx28-5"))
      data$treatment = factor(data$treatment, levels = c("LB","SS"))
      data2 <- data_summary(data, varname="exp", 
                            groupnames=c("treatment", "genotype"))
      return(data2)
    } 
  })
  
  data_pacin_plot <- reactive({
    if(input$enter_data==1){
      data2 <- data_pacin_selected()
      data <- as.data.frame(t(data2[,-1]))
      colnames(data)<-"exp"
      data$genotype <- c(rep("Col-0",6),rep("cop1-4",6),rep("pifq",6))
      data$treatment <- c(rep("LB",3),rep("SS",3),rep("LB",3),rep("SS",3),rep("LB",3),rep("SS",3))
      data$genotype = factor(data$genotype, levels = c("Col-0","cop1-4","pifq"))
      data$treatment = factor(data$treatment, levels = c("LB","SS"))
      data2 <- data_summary(data, varname="exp", 
                            groupnames=c("treatment", "genotype"))
      return(data2)
    } else {
      return()
    }
  })
  
  data_fank_plot <- reactive({
    if(input$enter_data==1){
      data2 <- data_fank_selected()
      data <- as.data.frame(t(data2[,-1]))
      colnames(data)<-"exp"
      data$genotype <- c(rep("Col-0",6),rep("pif4pif5",6),
                         rep("pif5",6),rep("pif5ox",6))
      data$treatment <- c(rep("LB",3),rep("SS",3),
                          rep("LB",3),rep("SS",3),
                          rep("LB",3),rep("SS",3),
                          rep("LB",3),rep("SS",3))
      data$genotype = factor(data$genotype, levels = c("Col-0","pif4pif5",
                                                       "pif5","pif5ox"))
      data$treatment = factor(data$treatment, levels = c("LB","SS"))
      data2 <- data_summary(data, varname="exp", 
                            groupnames=c("treatment", "genotype"))
      return(data2)
    } else {
      return()
    }
  })
  
  theme_plots <- theme(legend.position="top",
                       legend.title=element_blank(),
                       legend.text = element_text(size=12,colour="black"),
                       axis.text.x  = element_text(size=12,colour="black"),
                       axis.text.y  = element_text(size=12,colour="black"),
                       axis.title.y=element_text(size=12,colour="black",
                                                 margin = margin(t = 0, r = 10, b = 0, l = 0)),
                       panel.border = element_rect(size=0.5,fill=NA,colour="black"),
                       axis.ticks = element_line(colour="black"))
  
  mylabels_bbx28 <- c(expression("Col-0"),
                      expression(italic("bbx28-5")))
  
  mylabels_pacin <- c(expression("Col-0"),
                      expression(italic("cop1-4")),
                      expression(italic("pifq")))
  mylabels_fank <- c(expression("Col-0"),
                     expression(italic("pif4pif5")),
                     expression(italic("pif5")),
                     expression("PIF5ox"))
  
  y_lab <- reactive({
    switch(input$values,
           "1" = "Expresion normalizada (log2)",
           "2" = "Expresion relativa a Col-0 control (log2FC)"
    )
  })
  
  barplot_bbx28_1 <- reactive({
    ggplot(data_bbx28_plot(),aes(x = genotype, y = exp, fill=treatment))+
      geom_bar(stat="identity",position=position_dodge(),width=0.6,color="black",lwd=0.3)+
      geom_errorbar(aes(ymin=exp-sem, ymax=exp+sem), width=.2,
                    position=position_dodge(.6),lwd=0.3)+
      scale_fill_manual(values=c("#FCD16B", "#456355"),
                        breaks=c("LB", "SS"),
                        labels=c("LB", "SS"))+
      theme_light()+
      theme_plots+
      ylab(y_lab())+
      xlab(NULL)+
      scale_x_discrete(labels = mylabels_bbx28)
  })
  
  output$barplot_bbx28 <- renderPlot(if(is.null(data_bbx28_plot())) {
    return()
  } else {
    barplot_bbx28_1()
    
  }, height = 300)
  
  output$getbbx28barplot <- downloadHandler(
    filename = function() { paste(input$one_gene,'_Saura','.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 10, height = 10, res = 300, units = "cm")
      ggsave(file, plot = barplot_bbx28_1(), device = device)
    }
  )
  
  barplot_pacin_1 <- reactive({
    ggplot(data_pacin_plot(),aes(x = genotype, y = exp, fill=treatment))+
      geom_bar(stat="identity",position=position_dodge(),width=0.6,color="black",lwd=0.3)+
      geom_errorbar(aes(ymin=exp-sem, ymax=exp+sem), width=.2,
                    position=position_dodge(.6),lwd=0.3)+
      scale_fill_manual(values=c("#FCD16B", "#456355"),
                        breaks=c("LB", "SS"),
                        labels=c("LB", "SS"))+
      theme_light()+
      theme_plots+
      ylab(y_lab())+
      xlab(NULL)+
      scale_x_discrete(labels = mylabels_pacin)
  })
  
  output$barplot_pacin <- renderPlot(if(is.null(data_pacin_plot())) {
    return()
  } else {
    barplot_pacin_1()+ylab(NULL)
  }, height = 300)
  
  output$getpacinbarplot <- downloadHandler(
    filename = function() { paste(input$one_gene,'_Pacin','.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 12, height = 10, res = 300, units = "cm")
      ggsave(file, plot = barplot_pacin_1(), device = device)
    }
  )
  
  barplot_fank_1 <- reactive({
    ggplot(data_fank_plot(),aes(x = genotype, y = exp, fill=treatment))+
      geom_bar(stat="identity",position=position_dodge(),width=0.6,color="black",lwd=0.3)+
      geom_errorbar(aes(ymin=exp-sem, ymax=exp+sem), width=.2,
                    position=position_dodge(.6),lwd=0.3)+
      scale_fill_manual(values=c("#FCD16B", "#732002"),
                        breaks=c("LB", "SS"),
                        labels=c("Alto R:RL", "Bajo R:RL"))+
      theme_light()+
      theme_plots+
      ylab(y_lab())+
      xlab(NULL)+
      scale_x_discrete(labels = mylabels_fank)
  })
  
  output$barplot_fank <- renderPlot(if(is.null(data_fank_plot())) {
    return()
  } else {
    barplot_fank_1()+ylab(NULL)
  }, height = 300)
  
  output$getfankbarplot <- downloadHandler(
    filename = function() { paste(input$one_gene,'_Hornitschek','.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 14, height = 10, res = 300, units = "cm")
      ggsave(file, plot = barplot_fank_1(), device = device)
    }
  )
  
  
  #-------LIST_OF_GENES---------
  data_bbx28_boxplot <- reactive({
    if(input$enter_data==2){
      data_list <- data_bbx28_selected()[,-1]
      data_zscore <- t(apply(data_list, 1, cal_z_score))
      data <- as.data.frame(t(data_zscore))
      data$genotype <- c(rep("Col-0",6),rep("bbx28-5",6))
      data$treatment <- c(rep("LB",3),rep("SS",3),rep("LB",3),rep("SS",3))
      data <- melt(data)
      data$genotype = factor(data$genotype, levels = c("Col-0","bbx28-5"))
      data$treatment = factor(data$treatment, levels = c("LB","SS"))
      return(data)
    } 
  })
  
  data_pacin_boxplot <- reactive({
    if(input$enter_data==2){
      data_list <- data_pacin_selected()[,-1]
      data_zscore <- t(apply(data_list, 1, cal_z_score))
      data <- as.data.frame(t(data_zscore))
      data$genotype <- c(rep("Col-0",6),rep("cop1-4",6),rep("pifq",6))
      data$treatment <- c(rep("LB",3),rep("SS",3),rep("LB",3),rep("SS",3),rep("LB",3),rep("SS",3))
      data <- melt(data)
      data$genotype = factor(data$genotype, levels = c("Col-0","cop1-4","pifq"))
      data$treatment = factor(data$treatment, levels = c("LB","SS"))
      return(data)
    } 
  })
  
  data_fank_boxplot <- reactive({
    if(input$enter_data==2){
      data_list <- data_fank_selected()[,-1]
      data_zscore <- t(apply(data_list, 1, cal_z_score))
      data <- as.data.frame(t(data_zscore))
      data$genotype <- c(rep("Col-0",6),rep("pif4pif5",6),
                         rep("pif5",6),rep("pif5ox",6))
      data$treatment <- c(rep("LB",3),rep("SS",3),
                          rep("LB",3),rep("SS",3),
                          rep("LB",3),rep("SS",3),
                          rep("LB",3),rep("SS",3))
      data <- melt(data)
      data$genotype = factor(data$genotype, levels = c("Col-0","pif4pif5",
                                                       "pif5","pif5ox"))
      data$treatment = factor(data$treatment, levels = c("LB","SS"))
      return(data)
    } 
  })
  
  boxplot_bbx28_1 <- reactive({
    ggplot(data_bbx28_boxplot(), aes(x = genotype, y = value, fill = treatment))+
      geom_boxplot(aes(fill = treatment),position=position_dodge(0.8),
                   outlier.colour = NA,lwd=0.3,colour="black")+
      scale_color_manual(values=c("#FCD16B", "#456355"),
                         breaks=c("LB", "SS"),
                         labels=c("LB", "SS"))+
      scale_fill_manual(values=c("#FCD16B", "#456355"),
                        breaks=c("LB", "SS"),
                        labels=c("LB", "SS"))+
      theme_light()+
      theme_plots+
      ylab("Expresion normalizada (Z-score)")+
      xlab(NULL)+
      scale_x_discrete(labels = mylabels_bbx28)
  })
  
  output$boxplot_bbx28 <- renderPlot(if(is.null(data_bbx28_boxplot())) {
    return()
  } else {
    boxplot_bbx28_1()
  }, height = 300)
  
  output$getbbx28boxplot <- downloadHandler(
    filename = function() { paste("list",'_Saura','.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 10, height = 10, res = 300, units = "cm")
      ggsave(file, plot = boxplot_bbx28_1(), device = device)
    }
  )
  
  boxplot_pacin_1 <- reactive({
    ggplot(data_pacin_boxplot(), aes(x = genotype, y = value, fill = treatment))+
      geom_boxplot(aes(fill = treatment),position=position_dodge(0.8),
                   outlier.colour = NA,lwd=0.3,colour="black")+
      scale_color_manual(values=c("#FCD16B", "#456355"),
                         breaks=c("LB", "SS"),
                         labels=c("LB", "SS"))+
      scale_fill_manual(values=c("#FCD16B", "#456355"),
                        breaks=c("LB", "SS"),
                        labels=c("LB", "SS"))+
      theme_light()+
      theme_plots+
      ylab("Expresion normalizada (Z-score)")+
      xlab(NULL)+
      scale_x_discrete(labels = mylabels_pacin)
  })
  
  output$boxplot_pacin <- renderPlot(if(is.null(data_pacin_boxplot())) {
    return()
  } else {
    boxplot_pacin_1()+ylab(NULL)
  }, height = 300)
  
  output$getpacinboxplot <- downloadHandler(
    filename = function() { paste("list",'_Pacin','.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 12, height = 10, res = 300, units = "cm")
      ggsave(file, plot = boxplot_pacin_1(), device = device)
    }
  )
  
  boxplot_fank_1<- reactive({
    ggplot(data_fank_boxplot(), aes(x = genotype, y = value, fill = treatment))+
      geom_boxplot(aes(fill = treatment),position=position_dodge(0.8),
                   outlier.colour = NA,lwd=0.3,colour="black")+
      scale_color_manual(values=c("#FCD16B",  "#732002"),
                         breaks=c("LB", "SS"),
                         labels=c("Alto R:RL", "Bajo R:RL"))+
      scale_fill_manual(values=c("#FCD16B", "#732002"),
                        breaks=c("LB", "SS"),
                        labels=c("Alto R:RL", "Bajo R:RL"))+
      theme_light()+
      theme_plots+
      ylab("Expresion normalizada (Z-score)")+
      xlab(NULL)+
      scale_x_discrete(labels = mylabels_fank)
  })
  
  output$boxplot_fank <- renderPlot(if(is.null(data_fank_boxplot())) {
    return()
  } else {
    boxplot_fank_1()+ylab(NULL)
  }, height = 300)
  
  output$getfankboxplot <- downloadHandler(
    filename = function() { paste("list",'_Hornitschek','.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 14, height = 10, res = 300, units = "cm")
      ggsave(file, plot = boxplot_fank_1(), device = device)
    }
  )
  
  #----------------------------------------------  
  #------------------DATA_TABLES-----------------
  #----------------------------------------------
  
  
  #------------BBX28 TABLE----------------
  
  # custom container for table_bbx28
  container1 = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 3, 'AGI'),
        th(colspan = 6, 'Col-0'),
        th(colspan = 6, 'bbx28-5')
      ),
      tr(
        th(colspan = 3, 'LB'),
        th(colspan = 3, 'SS'),
        th(colspan = 3, 'LB'),
        th(colspan = 3, 'SS')
      ),
      tr(
        lapply(rep(c('R1', 'R2','R3'), 4), th)
      )
    )
  ))
  
  output$table_bbx28 <- DT::renderDataTable(
    DT::datatable(data_bbx28_selected(), options = list(pageLength = 5, dom='tip'),
                  container=container1,rownames = FALSE)
  )
  
  output$getbbx28 <- downloadHandler(
    filename = function() {
      paste("SauraSanchez.txt")
    },
    content = function(file) {
      write.table(data_bbx28_selected(), file, row.names = FALSE,sep="\t")
    }
  )
  
  #------------PACIN TABLE-------------
  
  # custom container for table_pacin
  
  container2 = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 3, 'AGI'),
        th(colspan = 6, 'Col-0'),
        th(colspan = 6, 'cop1-4'),
        th(colspan = 6, 'pifq')
      ),
      tr(
        th(colspan = 3, 'LB'),
        th(colspan = 3, 'SS'),
        th(colspan = 3, 'LB'),
        th(colspan = 3, 'SS'),
        th(colspan = 3, 'LB'),
        th(colspan = 3, 'SS')
      ),
      tr(
        lapply(rep(c('R1', 'R2','R3'), 6), th)
      )
    )
  ))
  
  
  
  
  output$table_pacin <- DT::renderDataTable(
    DT::datatable(data_pacin_selected(), options = list(pageLength = 5, dom='tip'),
                  container=container2,rownames = FALSE))
  
  output$getpacin <- downloadHandler(
    filename = function() {
      paste("Pacin.txt")
    },
    content = function(file) {
      write.table(data_pacin_selected(), file, row.names = FALSE,sep="\t")
    }
  )
  
  #------------FANK TABLE-------------
  
  # custom container for table_fank
  
  container3 = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 3, 'AGI'),
        th(colspan = 6, 'Col-0'),
        th(colspan = 6, 'pif4pif5'),
        th(colspan = 6, 'pif5'),
        th(colspan = 6, 'PIF5ox')
      ),
      tr(
        th(colspan = 3, 'LB'),
        th(colspan = 3, 'SS'),
        th(colspan = 3, 'LB'),
        th(colspan = 3, 'SS'),
        th(colspan = 3, 'LB'),
        th(colspan = 3, 'SS'),
        th(colspan = 3, 'LB'),
        th(colspan = 3, 'SS')
      ),
      tr(
        lapply(rep(c('R1', 'R2','R3'), 8), th)
      )
    )
  ))
  
  output$table_fank <- DT::renderDataTable(
    DT::datatable(data_fank_selected(), options = list(pageLength = 5, dom='tip'),
                  container=container3,rownames = FALSE))
  
  output$getfank <- downloadHandler(
    filename = function() {
      paste("Hornitschek",".txt",sep="")
    },
    content = function(file) {
      write.table(data_fank_selected(), file, row.names = FALSE,sep="\t")
    }
  )
  
})

shinyApp(ui = ui, server = server)
