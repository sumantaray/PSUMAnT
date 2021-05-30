library("ggplot2")
library(gridExtra)
library(data.table)
library(xlsx) 
library(stringr)
library(DT)
library(arules)
library(shinyBS)
library(RColorBrewer)
server <- function(input, output, session) {
  
  
  # Return the requested dataset ----
  chooseinput <- reactive({ 
      switch(input$choose,
           "Frequent drugsets (fast)"="F",
           "All possible drugsets (slow)"="S")
    })
 

  
    observeEvent(input$do3, {  
      da <- chooseinput()
      #value=valuefun()
      #d<- as.character(value)
      print(da)
      if (da=="F"){
        withProgress(message = 'Loading in progress',
                     detail = 'This may take a while...', value = 0, {
        load('master_table.RData')
                       
                       showModal(modalDialog(
                         tags$h3('Loading Complete'),
                         footer=tagList(
                           actionButton('OK', 'OK'),
                          # modalButton('cancel')
                         )
                       ))             
                       
                       observeEvent(input$OK, {
                         removeModal()})
                       
                     })
        
    
  datasetInput <- reactive({
    switch(input$meth,
           "fixed bitstring (length 10 bits)"="a",
           "regular expression"="b",
           "approximate regular expression"="r")
                   })
  
  
  
   valuefun <- renderText({ input$text })
   
   observeEvent(input$resetAll, {
     reset("form")
     #removeUI(selector = master_table_cond1)
     output$x1<-NULL
     output$nmatch<-NULL
     output$msg1<-NULL
     output$msg2<-NULL
     output$normalError<-NULL
     
     
   })
   
   observeEvent(input$resetAll, {
     reset("form")
     #removeUI(selector = master_table_cond1)
     output$x2<-NULL
        })
   
   
   observeEvent(input$resetAll, {
     reset("form")
     #removeUI(selector = master_table_cond1)
     output$x3<-NULL
   })
   
      observeEvent(input$do1, {
        
        #load('master_table.RData')
        # dataset <- datasetInput()
        #write.xlsx(dataset,file="data.xlsx",row.names = FALSE)
        
        #data<- read_excel("data.xlsx",col_names = TRUE)
        
        
        withProgress(message = 'Running (please wait)', value = 0, {
          n=4
          
          incProgress(1/n, detail = paste("Doing part1"))
          
          #output$value <- renderText({ input$text })
          #write.xlsx(dataset,file="data.xlsx",row.names = FALSE)
          
          dataset <- datasetInput()
          print(dataset)
          data1=dataset
          
          if(data1 =="a"){
            value=valuefun()
            d<- as.character(value)
            
            print(d)
            
            if(str_length(d)!=10){
              output$normalError<-renderText({return(paste("length must be equal to 10"))
              })}
            
            
            if(str_length(d)==10){
              master_table_cond=matrix(,2000,7)
              colnames(master_table_cond)<-c('bitstring','drugset','drugset-size', 'race','sex','age', 'group')
            k=0
             for (i in 1:nrow(master_table)){
              print(i)
              for(j in 1:ncol(master_table)){
                print(j)
                if (regexpr(pattern =as.character(d),master_table[i,j])[[1]]>0){
                  master_table_cond[(k+1),1]=as.character(master_table[i,j])
                  master_table_cond[(k+1),2]=colnames(master_table)[j]
                  master_table_cond[(k+1),3]=length(strsplit(colnames(master_table)[j],split=" ")[[1]])
                  master_table_cond[(k+1),4]=strsplit(rownames(master_table)[i],split="")[[1]][1]
                  master_table_cond[(k+1),5]=strsplit(rownames(master_table)[i],split="")[[1]][2]
                  master_table_cond[(k+1),6]=strsplit(rownames(master_table)[i],split="")[[1]][3]
                  master_table_cond[(k+1),7]=rownames(master_table)[i]
                  k=k+1
                  print("test")
                }
                  }
             }
            master_table_cond=master_table_cond[!is.na(master_table_cond[,1]),]
            output$nmatch<-renderText({return(paste("<font color=\"#FF0000\"><b>", "NUMBER OF MATCHES:","</b></font>", "<font color=\"#FF0000\"><b>",k,"</b></font>"))})
            output$msg1<-renderText({return(paste("<font color=\"#FF0000\"><b>", "Format: Fixed bitstring (length 10 bits)","</b></font>"))})
            output$msg2<-renderText({return(paste("<font color=\"#FF0000\"><b>", "Input Pattern:","</b></font>", "<font color=\"#FF0000\"><b>",d,"</b></font>"))})
            if(k>0){
            output$x1 = DT::renderDataTable(master_table_cond, server = FALSE)}
            }
            
            
            output$downloadData1 <- downloadHandler(
              filename = function() {
                paste("result", ".csv", sep = "")
              },
              content = function(file) {
                write.csv(master_table_cond, file, row.names = FALSE)
                #   pdf("plot1.pdf")
                #    print(output$plot11)
                #   pdf("plot2.pdf")
                #    print(output$plot1)
                #   dev.off()
              }
            )   
            
            
            output$plot <- renderPlot({
              print("first")
              master_table_cond=as.data.frame(master_table_cond)
              colnames(master_table_cond)<-c('bitstring','drugset',  'drugset-size', 'race','sex','age', 'group')
              #colnames(m_tab)<-c('group','drug','pro')
              #m_tab=as.data.frame(m_tab)
              m_tab=plot_proportion(master_table_cond)
              print("test pp")
              m_tab=as.data.frame(m_tab)
              colnames(m_tab)<-c('V1','catagories','V3')
              print(m_tab)
              #  m_tab=m_tab[which(m_tab$pro!=0),]
              # ggplot(m_tab, aes(fill=drug, y=pro, x=group)) + 
              #    geom_bar(position="stack", stat="identity") +scale_fill_manual(values = mycolors)+ ylab("")+theme_bw()
              
              mycolors1 <- colorRampPalette(brewer.pal(9, "Set1"))(nrow(m_tab))
              
              ggplot(m_tab, aes(fill=catagories, y=as.numeric(V3), x=V1,label=as.numeric(V3))) +
                geom_bar(position="fill", stat="identity",width=0.5) + xlab("")+ylab("")+scale_fill_manual(values = mycolors1)+
                geom_text(aes(label=V3), position=position_fill(vjust = 0.5)) +
                theme_bw() + theme(text = element_text(size=14),axis.text.x = element_text(hjust=1))
              
            })
            
            
            
          }
          
          
          if(data1 =="b"){
            value=valuefun()
            d<- as.character(value)
            master_table_cond1=matrix(,2000,7)
            colnames(master_table_cond1)<-c('bitstring','drugset',  'drugset-size', 'race','sex','age', 'group')
            k=1
            for (i in 1:nrow(master_table)){
            print(i)
            for(j in 1:ncol(master_table)){
              #print(j)
              if (regexpr(pattern =as.character(d),master_table[i,j])[[1]]>0){
                master_table_cond1[k,1]=as.character(master_table[i,j])
                master_table_cond1[k,2]=colnames(master_table)[j]
                master_table_cond1[k,3]=length(strsplit(colnames(master_table)[j],split=" ")[[1]])
                master_table_cond1[k,4]=strsplit(rownames(master_table)[i],split="")[[1]][1]
                master_table_cond1[k,5]=strsplit(rownames(master_table)[i],split="")[[1]][2]
                master_table_cond1[k,6]=strsplit(rownames(master_table)[i],split="")[[1]][3]
                master_table_cond1[k,7]=rownames(master_table)[i]
                k=k+1
              }
            }
            }
            master_table_cond1=master_table_cond1[!is.na(master_table_cond1[,1]),]
            output$nmatch<-renderText({return(paste("<font color=\"#FF0000\"><b>", "NUMBER OF MATCHES:","</b></font>", "<font color=\"#FF0000\"><b>",k,"</b></font>"))})
            output$msg1<-renderText({return(paste("<font color=\"#FF0000\"><b>", "Format: Regular Expression","</b></font>"))})
            output$msg2<-renderText({return(paste("<font color=\"#FF0000\"><b>", "Input Pattern:","</b></font>", "<font color=\"#FF0000\"><b>",d,"</b></font>"))})
            output$x2 = DT::renderDataTable(master_table_cond1, server = FALSE)
            
            # observeEvent(input$resetAll, {
            #   reset("form")
            # #  removeUI("#mytable")
            #   output$x2<-NULL
            # })
            
            
            output$plot <- renderPlot({
              print("first")
              master_table_cond1=as.data.frame(master_table_cond1)
              colnames(master_table_cond1)<-c('bitstring','drugset',  'drugset-size', 'race','sex','age', 'group')
              #colnames(m_tab)<-c('group','drug','pro')
              #m_tab=as.data.frame(m_tab)
              m_tab=plot_proportion(master_table_cond1)
              print("test pp")
              m_tab=as.data.frame(m_tab)
              colnames(m_tab)<-c('V1','catagories','V3')
              print(m_tab)
            #  m_tab=m_tab[which(m_tab$pro!=0),]
             # ggplot(m_tab, aes(fill=drug, y=pro, x=group)) + 
            #    geom_bar(position="stack", stat="identity") +scale_fill_manual(values = mycolors)+ ylab("")+theme_bw()
             
              mycolors1 <- colorRampPalette(brewer.pal(9, "Set1"))(nrow(m_tab))
              
              ggplot(m_tab, aes(fill=catagories, y=as.numeric(V3), x=V1,label=as.numeric(V3))) +
                geom_bar(position="fill", stat="identity",width=0.5) + xlab("")+ylab("")+scale_fill_manual(values = mycolors1)+
                geom_text(aes(label=V3), position=position_fill(vjust = 0.5)) +
                theme_bw() + theme(text = element_text(size=14),axis.text.x = element_text(hjust=1))
               })
            
            
            output$downloadData1 <- downloadHandler(
              filename = function() {
                paste("result", ".csv", sep = "")
              },
              content = function(file) {
                write.csv(master_table_cond1, file, row.names = FALSE)
                #   pdf("plot1.pdf")
                #    print(output$plot11)
                #   pdf("plot2.pdf")
                #    print(output$plot1)
                #   dev.off()
              }
            )    
           
            
            }
          
      
          if(data1 =="r"){
            print("test")
            l <- reactiveValues()
          #  observeEvent(input$reset, {
              # display a modal dialog with a header, textinput and action buttons
              showModal(modalDialog(
                tags$h2('Please enter distance of approximation'),
                textInput('dist', ''),
                footer=tagList(
                  actionButton('submit', 'Submit'),
                  modalButton('cancel')
                )
              ))
           # })
            
            # only store the information if the user clicks submit
            observeEvent(input$submit, {
              removeModal()
              l$dist <- input$dist
              
            
             value=valuefun()
             d<- as.character(value)
             master_table_cond2=matrix(,2000,7)
             colnames(master_table_cond2)<-c('bitstring','drugset','drugset-size', 'race','sex','age', 'group')
             k=1
             for (i in 1:nrow(master_table)){
              # print(i)
               for(j in 1:ncol(master_table)){
                 #print(as.numeric(l$dist))
                 print(as.character(d))
                 print(length(agrep(pattern =as.character(d),master_table[i,j],max.distance=as.numeric(l$dist))))
                 if (length(agrep(pattern =as.character(d),master_table[i,j],max.distance=as.numeric(l$dist)))==0){
                   
                   output$safeError<-renderText({return(paste(" "))
                   })}
                 
                 
                 if (length(agrep(pattern =as.character(d),master_table[i,j],max.distance=as.numeric(l$dist)))==1){
                   master_table_cond2[k,1]=as.character(master_table[i,j])
                   master_table_cond2[k,2]=colnames(master_table)[j]
                   master_table_cond2[k,3]=length(strsplit(colnames(master_table)[j],split=" ")[[1]])
                   master_table_cond2[k,4]=strsplit(rownames(master_table)[i],split="")[[1]][1]
                   master_table_cond2[k,5]=strsplit(rownames(master_table)[i],split="")[[1]][2]
                   master_table_cond2[k,6]=strsplit(rownames(master_table)[i],split="")[[1]][3]
                   master_table_cond2[k,7]=rownames(master_table)[i]
                   k=k+1
                 }
               }
             }
             master_table_cond2=master_table_cond2[!is.na(master_table_cond2[,1]),]
             output$nmatch<-renderText({return(paste("<font color=\"#FF0000\"><b>", "NUMBER OF MATCHES:","</b></font>", "<font color=\"#FF0000\"><b>",k,"</b></font>"))})
             output$msg1<-renderText({return(paste("<font color=\"#FF0000\"><b>", "Format: Approximate Regular Expression","</b></font>"))})
             output$msg2<-renderText({return(paste("<font color=\"#FF0000\"><b>", "Input Pattern:","</b></font>", "<font color=\"#FF0000\"><b>",d,"</b></font>"))})
             output$x3 = DT::renderDataTable(master_table_cond2, server = FALSE)
             
             # observeEvent(input$resetAll, {
             #   reset("form")
             #   #removeUI(selector = master_table_cond1)
             #   output$x3<-NULL
             # })
          
             output$downloadData1 <- downloadHandler(
               filename = function() {
                 paste("result", ".csv", sep = "")
               },
               content = function(file) {
                 write.csv(master_table_cond2, file, row.names = FALSE)
                 #   pdf("plot1.pdf")
                 #    print(output$plot11)
                 #   pdf("plot2.pdf")
                 #    print(output$plot1)
                 #   dev.off()
               }
             )  
             
            
             
             output$plot <- renderPlot({
               print("first")
               master_table_cond2=as.data.frame(master_table_cond2)
               colnames(master_table_cond2)<-c('bitstring','drugset',  'drugset-size', 'race','sex','age', 'group')
               #colnames(m_tab)<-c('group','drug','pro')
               #m_tab=as.data.frame(m_tab)
               m_tab=plot_proportion(master_table_cond2)
               print("test pp")
               m_tab=as.data.frame(m_tab)
               colnames(m_tab)<-c('V1','catagories','V3')
               print(m_tab)
               #  m_tab=m_tab[which(m_tab$pro!=0),]
               # ggplot(m_tab, aes(fill=drug, y=pro, x=group)) + 
               #    geom_bar(position="stack", stat="identity") +scale_fill_manual(values = mycolors)+ ylab("")+theme_bw()
               
               mycolors1 <- colorRampPalette(brewer.pal(9, "Set1"))(nrow(m_tab))
               
               ggplot(m_tab, aes(fill=catagories, y=as.numeric(V3), x=V1,label=as.numeric(V3))) +
                 geom_bar(position="fill", stat="identity",width=0.5) + xlab("")+ylab("")+scale_fill_manual(values = mycolors1)+
                 geom_text(aes(label=V3), position=position_fill(vjust = 0.5)) +
                 theme_bw() + theme(text = element_text(size=14),axis.text.x = element_text(hjust=1))
             })
            
             
             
              })
            
            
            
          }
          
      
      #colnames(master_table_cond)<-colnames(master_table)
      #rownames(master_table_cond)<-rownames(master_table)
      #master_table_=master_table_1001[setdiff(1:32,which(rowSums(master_table_1001)==0)),]
      
      
      
      
      
    })
     
        
        
      
})
      
      
      observeEvent(input$resetAll1, {
        reset("form")
        #removeUI(selector = master_table_cond1)
        output$x4<-NULL
      })
      
      observeEvent(input$do2, {
        nb.cols=16
        mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)
        
        
        withProgress(message = 'Running', value = 0, {
          n=4
          
          incProgress(1/n, detail = paste("please wait"))
        per=input$per
        p=which(periods_non==per)
        drg=input$drg
        drg=strsplit(drg,split=" ")[[1]][1]
        side=input$dir
        print(p)
        print(drg)
        print(side)
        d=stat_tab(result_map_notna,p , drg, side,node)
        print(d)
        colnames(d)<-c('Drug (D)','Drug (D*)', 'Supp (D->D*)','Lift', 'Conf (D->D*)','Conf (D*->D)','Delta_Conf')
          
        output$x4 = DT::renderDataTable(d, server = FALSE,rownames=FALSE)
        
       # pl=ggplot(d, aes(d$`Conf_W (D->D*)`,d$`Conf_W (D*->D)`,size=d$Supp_W,col=d$`Drug (D*)`))+geom_point() +xlab("Conf_W(D->D*)")+ylab("Conf_W(D*->D)") +scale_fill_manual(values = mycolors)   
        
        
        
        output$plot1 <- renderPlot({
         pl1<- ggplot(d, aes(d$`Conf (D->D*)`,d$`Conf (D*->D)`,size=`Supp (D->D*)`,col=`Drug (D*)`))+geom_point()+  ggplot2::annotation_custom(grid::linesGrob(gp = grid::gpar(col = "red", lty = 2)))+ xlab("Conf (D->D*)")+ylab("Conf(D*->D)") +scale_fill_manual(values = mycolors) +theme_bw() #geom_abline(intercept = 0, slope = 1,linetype=2)+
          print(pl1)
        })
        
       # output$info <- renderText({
      #    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
      #  })
        
        output$plot2 <- renderPlot({
          pl2<-ggplot(d, aes(d$`Supp (D->D*)`,d$Delta_Conf,size=Lift,col=`Drug (D*)`))+geom_point() +xlab("Supp_W (D->D*)")+ylab("Delta_Conf_w") +scale_fill_manual(values = mycolors) +   theme_bw()        # print(pl)
        print(pl2)
          })
        
      
})
 
        output$downloadData <- downloadHandler(
          filename = function() {
            paste("result", ".csv", sep = "")
          },
          content = function(file) {
            write.csv(d, file, row.names = FALSE)
         #   pdf("plot1.pdf")
        #    print(output$plot11)
         #   pdf("plot2.pdf")
        #    print(output$plot1)
         #   dev.off()
          }
        )   
        
                       
})
      }
      
  
      if (da=="S"){
      
        
        withProgress(message = 'Loading in progress',
                     detail = 'This may take a while...', value = 0, {
                       load('master_table.RData')
                       
                       showModal(modalDialog(
                         tags$h3('Loading Complete'),
                         footer=tagList(
                           actionButton('OK', 'OK'),
                           # modalButton('cancel')
                         )
                       ))             
                       
                       observeEvent(input$OK, {
                         removeModal()})
                     })
        
        datasetInput <- reactive({
          switch(input$meth,
                 "fixed bitstring (length 10 bits)"="a",
                 "regular expression"="b",
                 "approaximate regular expression"="c")
        })
        
        
        
        # datasetInput <- reactive({
        #   switch(input$per,
        #          "1965-1969"="a",
        #          "1970-1974"="b",
        #          "1975-1979"="c",
        #          "1980-1984"="d",
        #          "1985-1989"="e",
        #          "1990-1994"="f",
        #          "1995-1999"="g",
        #          "2000-2004"="h",
        #          "2005-2009"="i",
        #          "2010-2014"="j",
        #          )
        # })
        # 
        # 
        # datasetInput <- reactive({
        #   switch(input$per,
        #          "1965-1969"="a",
        #          "1970-1974"="b",
        #          "1975-1979"="c",
        #          "1980-1984"="d",
        #          "1985-1989"="e",
        #          "1990-1994"="f",
        #          "1995-1999"="g",
        #          "2000-2004"="h",
        #          "2005-2009"="i",
        #          "2010-2014"="j",
        #   )
        # })
        # 
        
        
        valuefun <- renderText({ input$text })
        
        # datasetInput1 <- reactive({
        #   
        #  
        #   switch(input$patterns,
        #          "pattern1" = p[[1]],
        #          "pattern2" = p[[2]],
        #          "pattern3" = p[[3]])
        #   
        #    })
        
        
        
        
        # datasetInput1 <- reactive({
        # 
        #     
        #   
        #   
        #   
        #     switch(input$patterns,
        #          "pattern1" = print(p[[1]]),
        #          "pattern2" = print(p[[2]]),
        #          "pattern3" = print(p[[1]]))
        #   
        # })
        # })
        
        
        # observeEvent(input$do, {
        #   
        #   load('master_table.RData')
        #  # dataset <- datasetInput()
        #   #write.xlsx(dataset,file="data.xlsx",row.names = FALSE)
        #   
        #   #data<- read_excel("data.xlsx",col_names = TRUE)
        #   
        #   
        #   withProgress(message = 'Making plot', value = 0, {
        #     n=4
        #     
        #     incProgress(1/n, detail = paste("Doing part1"))
        #     
        #     dataset <- datasetInput()
        #     #write.xlsx(dataset,file="data.xlsx",row.names = FALSE)
        #     
        #     data<- dataset #read_excel("data.xlsx",col_names = TRUE)
        #     #print(data)
        #     #data$V1<-NULL
        #     
        #     master_table_cond=matrix(,2000,3)
        #     colnames(master_table_cond)<-c('bit-string','drug_name','group')
        #     k=1
        #     for (i in 1:nrow(master_table)){
        #       print(i)
        #       for(j in 1:ncol(master_table)){
        #         #print(j)
        #         if (regexpr(pattern =as.character(data),master_table[i,j])>0){
        #           master_table_cond[k,1]=as.character(master_table[i,j])
        #           master_table_cond[k,2]=colnames(master_table)[j]
        #           master_table_cond[k,3]=rownames(master_table)[i]
        #           k=k+1
        #         }
        #       }
        #     }
        # 
        #     output$x1 = DT::renderDataTable(master_table_cond, server = FALSE)
        #     
        #     
        #       })
        # })
        #     
        
        observeEvent(input$do1, {
          
          #load('master_table.RData')
          # dataset <- datasetInput()
          #write.xlsx(dataset,file="data.xlsx",row.names = FALSE)
          
          #data<- read_excel("data.xlsx",col_names = TRUE)
          
          
          withProgress(message = 'Making plot', value = 0, {
            n=4
            
            incProgress(1/n, detail = paste("Doing part1"))
            
            #output$value <- renderText({ input$text })
            #write.xlsx(dataset,file="data.xlsx",row.names = FALSE)
            
            dataset <- datasetInput()
            print(dataset)
            data1=dataset
            if(data1 =="a"){
              value=valuefun()
              d<- as.character(value)
              
              print(d)
              
              if(str_length(d)!=10){
                output$normalError<-renderText({return(paste("length must be equal to 10"))
                })}
              
              
              if(str_length(d)==10){
                master_table_cond=matrix(,1,7)
                colnames(master_table_cond)<-c('bitstring','drugset','drugset-size', 'race','sex','age', 'group')
                k=1
                for (i in 1:nrow(master_table)){
                  print(i)
                  for(j in 1:ncol(master_table)){
                    #print(j)
                    if (regexpr(pattern =as.character(d),master_table[i,j])>0){
                      master_table_cond[k,1]=as.character(master_table[i,j])
                      master_table_cond[k,2]=colnames(master_table)[j]
                      master_table_cond[k,3]=length(strsplit(colnames(master_table)[j],split=" ")[[1]])
                      master_table_cond[k,4]=strsplit(rownames(master_table)[i],split="")[[1]][1]
                      master_table_cond[k,5]=strsplit(rownames(master_table)[i],split="")[[1]][2]
                      master_table_cond[k,6]=strsplit(rownames(master_table)[i],split="")[[1]][3]
                      master_table_cond[k,7]=rownames(master_table)[i]
                      
                      
                    }
                  }
                }
                output$nmatch<-renderText({return(paste("<font color=\"#FF0000\"><b>", "NUMBER OF MATCHES:","</b></font>", "<font color=\"#FF0000\"><b>",k,"</b></font>"))})
                output$x1 = DT::renderDataTable(master_table_cond, server = FALSE)
              }
              
            }
            
            if(data1 =="b"){
              value=valuefun()
              d<- as.character(value)
              master_table_cond1=matrix(,2000,7)
              colnames(master_table_cond1)<-c('bitstring','drugset',  'drugset-size', 'race','sex','age', 'group')
              k=1
              for (i in 1:nrow(master_table)){
                print(i)
                for(j in 1:ncol(master_table)){
                  #print(j)
                  if (regexpr(pattern =as.character(d),master_table[i,j])>0){
                    master_table_cond1[k,1]=as.character(master_table[i,j])
                    master_table_cond1[k,2]=colnames(master_table)[j]
                    master_table_cond1[k,3]=length(strsplit(colnames(master_table)[j],split=" ")[[1]])
                    master_table_cond1[k,4]=strsplit(rownames(master_table)[i],split="")[[1]][1]
                    master_table_cond1[k,5]=strsplit(rownames(master_table)[i],split="")[[1]][2]
                    master_table_cond1[k,6]=strsplit(rownames(master_table)[i],split="")[[1]][3]
                    master_table_cond1[k,7]=rownames(master_table)[i]
                    k=k+1
                  }
                }
              }
              output$nmatch<-renderText({return(paste("<font color=\"#FF0000\"><b>", "NUMBER OF MATCHES:","</b></font>", "<font color=\"#FF0000\"><b>",k,"</b></font>"))})
              output$x2 = DT::renderDataTable(master_table_cond1, server = FALSE)
            }
            
            
            if(data1 =="r"){
              
              l <- reactiveValues()
              observeEvent(input$reset, {
                # display a modal dialog with a header, textinput and action buttons
                showModal(modalDialog(
                  tags$h2('Input distance of approximation (non-negative)'),
                  textInput('dist', 'distance'),
                  footer=tagList(
                    actionButton('submit', 'Submit'),
                    modalButton('cancel')
                  )
                ))
              })
              
              # only store the information if the user clicks submit
              observeEvent(input$submit, {
                removeModal()
                l$dist <- input$dist
                
                
                value=valuefun()
                d<- as.character(value)
                master_table_cond2=matrix(,2000,7)
                colnames(master_table_cond2)<-c('bitstring','drugset','drugset-size', 'race','sex','age', 'group')
                k=1
                for (i in 1:nrow(master_table)){
                  print(i)
                  for(j in 1:ncol(master_table)){
                    print(as.numeric(l$dist))
                    print(as.character(d))
                    if (length(agrep(pattern =as.character(d),master_table[i,j],max.distance=as.numeric(l$dist)))==0){
                      
                      output$safeError<-renderText({return(paste("sorry!!No match"))
                      })}
                    
                    
                    if (length(agrep(pattern =as.character(d),master_table[i,j],max.distance=as.numeric(l$dist)))==1){
                      master_table_cond2[k,1]=as.character(master_table[i,j])
                      master_table_cond2[k,2]=colnames(master_table)[j]
                      master_table_cond2[k,3]=length(strsplit(colnames(master_table)[j],split=" ")[[1]])
                      master_table_cond2[k,4]=strsplit(rownames(master_table)[i],split="")[[1]][1]
                      master_table_cond2[k,5]=strsplit(rownames(master_table)[i],split="")[[1]][2]
                      master_table_cond2[k,6]=strsplit(rownames(master_table)[i],split="")[[1]][3]
                      master_table_cond2[k,7]=rownames(master_table)[i]
                      k=k+1
                    }
                  }
                }
                output$nmatch<-renderText({return(paste("<font color=\"#FF0000\"><b>", "NUMBER OF MATCHES:","</b></font>", "<font color=\"#FF0000\"><b>",k,"</b></font>"))})
                output$x3 = DT::renderDataTable(master_table_cond2, server = FALSE)
                
                
                
              })
              
            }
            
            
            #colnames(master_table_cond)<-colnames(master_table)
            #rownames(master_table_cond)<-rownames(master_table)
            #master_table_=master_table_1001[setdiff(1:32,which(rowSums(master_table_1001)==0)),]
            
            
            
            
            
          })
          
        })
        
        
        observeEvent(input$do2, {
          nb.cols=16
          mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)
          
          
          withProgress(message = 'Running', value = 0, {
            n=4
            
            incProgress(1/n, detail = paste("please wait"))
            per=input$per
            p=which(periods_non==per)
            drg=input$drg
            drg=strsplit(drg,split=" ")[[1]][1]
            side=input$dir
            print(p)
            print(drg)
            print(side)
            d=stat_tab(result_map_notna,p , drg, side,node)
            print(d)
            
            
            output$x4 = DT::renderDataTable(d, server = FALSE,rownames=FALSE)
            
            # pl=ggplot(d, aes(d$`Conf_W (D->D*)`,d$`Conf_W (D*->D)`,size=d$Supp_W,col=d$`Drug (D*)`))+geom_point() +xlab("Conf_W(D->D*)")+ylab("Conf_W(D*->D)") +scale_fill_manual(values = mycolors)   
            
            output$plot1 <- renderPlot({
              ggplot(d, aes(d$`Conf_W (D->D*)`,d$`Conf_W (D*->D)`,size=`Supp_W (D->D*)`,col=`Drug (D*)`))+geom_point() +xlim(0:1)+ylim(0:1)+geom_abline(intercept = 0, slope = 1,linetype=2)+ xlab("Conf_W (D->D*)")+ylab("Conf_W(D*->D)") +scale_fill_manual(values = mycolors) +theme_bw()
              # print(pl)
            })
            
            # output$info <- renderText({
            #    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
            #  })
            
            output$plot2 <- renderPlot({
              ggplot(d, aes(d$`Supp_W (D->D*)`,d$Delta_Conf_w,size=Lift_W,col=`Drug (D*)`))+geom_point() +xlim(0:1)+ylim(0:1)+xlab("Supp_W (D->D*)")+ylab("Delta_Conf_w") +scale_fill_manual(values = mycolors) +  geom_abline(intercept = 0, slope = 1,linetype=2)+ theme_bw()        # print(pl)
            })
            
            
          })
          
          output$download <- downloadHandler(
            filename = function(){paste('result',".csv",sep = ' with ')},
            content = function(file){
              write.csv(fittable(),file)})
          
        })
      }
 
    })
}