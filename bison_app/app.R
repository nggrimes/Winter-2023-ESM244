library(shiny)
library(tidyverse)
library(lterdatasampler)
library(shinyWidgets)

# Make function outside for easier reading
gompertz<-function(b1,b2,b3,age){
  BM= b1*exp(-exp(-b2*(age-b3)))
  return(BM)
}

x=seq(from=0, to=20,length.out=100)

#start UI
ui<-fluidPage(
  setSliderColor(rep("#003660",times=3),seq(1,3)),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bm",
                label="Asymptotic Body Mass (b1)",
                min=0,max=2000,
                value=500),
      sliderInput("ig",
                label="Instantaneous growth rate (b2)",
                min=0,max=2,
                value=1,
                step=.2),
      sliderInput("age_in",
                label="Age inflection (b3)",
                min=0,max=10,
                value=5),
    width=4),
    mainPanel(
      plotOutput("gompertz")
    )
  )
)

server<-function(input,output){
  dft<-reactive({
    #make a dataframe that changes
    data.frame(age=x,weight=gompertz(input$bm,input$ig,input$age_in,x))
  })
  #Graph said dataframe using bison data on top of our model dataframe that changes with inputs
  output$gompertz=renderPlot({knz_bison %>% 
    mutate(animal_age = rec_year - animal_yob) %>% 
    filter(animal_sex=="F") %>% 
    ggplot()+
      geom_point(aes(x=animal_age,y=animal_weight),size=2.5,alpha=0.2,color='purple')+
      theme_minimal()+
      xlab('Age')+
      ylab('Weight')+
      ggtitle("Female Bison from Konza Prairie")+
      theme(axis.title = element_text(size=26),axis.text = element_text(size=20))+
      theme(plot.title = element_text(size=28,hjust=0.5))+
    geom_line(data=dft(),aes(x=age,y=weight),color="#79A540",size=2)
  })
  
}
  
shinyApp(ui=ui,server=server)  