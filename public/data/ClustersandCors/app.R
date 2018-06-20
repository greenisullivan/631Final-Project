library(tidyverse)
library(ggradar)
library(extrafont)
library(forcats)
library(here)
library(scales)
library(ggplot2)
library(reshape2)
library(knitr)
library(shiny)

#nfreqs <- read_csv('static/data/redditProject/normal_subset_freqs.csv')
#redfreqs <- read_csv('static/data/redditProject/reduced_normal_subset_freqs')
#clusters <- read_csv('static/data/redditProject/subsetcenters.csv')

nfreqs <- read_csv('../redditProject/normal_subset_freqs.csv')
redfreqs <- read_csv('../redditProject/reduced_normal_subset_freqs')
clusters <- read_csv('../redditProject/subsetcenters.csv') 

od_num=65
pc_num=21

dlabels= c('d1 ~negative', 'd2 ~bio', 'd3 ~feelings', 'd4 ~social, not feelings', 
           'd5 ~pos emote, no functional','d6 ~physics and bio','d7 ~sensing',
           'd8 ~leisure not money', 'd9 ~swearing and money, no bio',
           'd10 ~certain absolutist',
           'd11 ~work, num, no sensing',
           'd12 ~inhibition, sex, no money',
           'd13 ~home, hearing',
           'd14 ~work, acheive, no swearing',
           'd15 ~2nd person, no friends',
           'd16 ~cause, acheive, no death',
           'd17 ~friends',
           'd18 ~family',
           'd19 ~ingest',
           'd20 ~2nd person, num',
           'd21 ~home, death, no religion'
           )

correlations <- cor(nfreqs[c(2:(od_num+1))],redfreqs[c(2:(pc_num+1))])

corlong <- melt(correlations ) %>% 
  setNames(c('orig', 'red_dim', 'correlation'))


corlong <- corlong %>% 
  mutate(dictcat=case_when(
    orig %in% c('i','we','ipron','they','you','shehe','pronoun','ppron')
    ~ 'pronouns',
    orig %in% c('verb','auxverb','past','present','future')
    ~ 'verbs',
    orig %in% c('article','adverb','preps','conj','negate','quant','number','swear','funct')
    ~ 'otherfunctional',
    orig %in% c('social','family','friend','humans')
    ~ 'people',
    orig %in% c('affect','posemo','negemo','anx','anger','sad')
    ~ 'feelings',
    orig %in% c('cogmech','insight','cause','discrep','tentat','certain','inhib','incl','excl')
    ~ 'unlabeled',
    orig %in% c('percept','see','hear','feel')
    ~ 'sense',
    orig %in% c('bio','body','health','sexual','ingest')
    ~ 'bio',
    orig %in% c('relativ','motion','space','time')
    ~ 'physics',
    orig %in% c('work','achieve','leisure','home','money','relig','death')
    ~ 'life',
    orig %in% c('assent','nonfl','filler', 'absolutist')
    ~ 'idk'))


pcs <- distinct(corlong['red_dim'])
cs <- distinct(clusters['X1'])

ui <- fluidPage(
  titlePanel("PCA Correlations"),
  fluidRow(
    column(4,
      selectInput(inputId = "pc",
                  label = "Principle Component/New Dimemsion",
                  choices=pcs,
                  selected='d1')
    )
  ),
  fluidRow(
    column(12,
      plotOutput(outputId = "corPlot")
    )
  ),
  fluidRow(
    column(4,
         selectInput(inputId = "clusterN",
                     label = "Cluster",
                     choices = list("Games, LoL, Overwatch" = 1, 
                                    "Programming, PCs" = 2, 
                                    "Trading" = 3,
                                    "Addiction, Anxiety, Weightloss" = 4, 
                                    "Consoles, Anime, FIFA" = 5, 
                                    "Wrestling, 40k, ??" = 6,
                                    "Sports, trading cards " = 7, 
                                    "NSFW" = 8, 
                                    "Jobs, Futures, Crypto" = 9,
                                    "Small NSW" = 10, 
                                    "MMO types" = 11,
                                    'Tech/Phones'=12,
                                    'Christ, Conspiracy, DnD'=13,
                                    'Dating, Depression'=14,
                                    'Parents, dogs'=15
                      ),
                     selected=1
          )
    )
  ),
  fluidRow(
    column(12,
      textOutput(outputId = "clusterText"),
      plotOutput(outputId = "clusterPlot")
    )
  ) 
)

server <- function(input, output) {
  output$corPlot <- renderPlot({ 
    workingdf <- corlong %>%  
      filter(red_dim==input$pc) %>% 
      mutate(poscor=abs(correlation), sign=case_when(correlation>=0~'pos', correlation<0~'neg'))
  
    ggplot(workingdf, aes(x=fct_reorder(orig,correlation,.desc = TRUE), y=poscor, color=sign, fill=dictcat))+
      geom_col()+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 60, hjust=1,size = 16),legend.text = element_text(size=16) )+
      scale_fill_manual(values=c("#FF5A5F", "#FFB400", "#007A87",  "#8CE071", "#7B0051", "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"))+
      scale_color_manual(values=c('black','white'))+
      labs(x='Dictionaries', y='Corelation Magnitude', color='Correlation Direction',fill="Dictionary Category", title="Correlation between Principal Components and Dictionaries")
    
    
  })
  output$clusterText <- renderText({
    paste("Cluster", input$clusterN, "includes subreddits such as: \t\t\n",clusters[input$clusterN,'X1'])
    
  })
  output$clusterPlot <- renderPlot({
    radar_colour=c("#FF5A5F", "#FFB400", "#007A87",  "#8CE071", "#7B0051", "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20")
    
    clusters <- clusters %>%  
      mutate_at(vars(starts_with('d')),rescale)
    
    clusterN=as.numeric(input$clusterN)
    ggradar(clusters[clusterN,], grid.label.size = 0,
            plot.extent.x.sf = 2,
            group.point.size = 3,
            axis.label.size = 4,
            axis.labels = dlabels,
            legend.text.size = 11,
            group.colours = radar_colour[clusterN])+
      theme(legend.position='none')+
      labs(subtitle="Shape of Selected Cluster in PCA space")
    
  })
  
}  
  
shinyApp(ui = ui, server = server)
