library(tidyverse)
#library(ggalt)
library(ggradar)
library(extrafont)
library(ggfortify)
library(infer)
library(forcats)
library(ggridges)
library(here)
library(scales)
library(ggdendro)
library(treemapify)
library(ggplot2)
library(reshape2)
library(knitr)
library(shiny)
library(RColorBrewer)

nfreqs <- read_csv(here('static','data','redditProject','normal_subset_freqs.csv'))
redfreqs <- read_csv(here('static','data','redditProject','reduced_normal_subset_freqs'))

od_num=65
pc_num=21

correlations <- cor(nfreqs[c(2:(od_num+1))],redfreqs[c(2:(pc_num+1))])

corlong <- melt(correlations ) %>% 
  setNames(c('orig', 'red_dim', 'correlation'))


corlong <- corlong %>% 
  mutate(dictcat=case_when(
    orig %in% c('ifreq','wefreq','ipronfreq','theyfreq','youfreq','shehefreq','pronounfreq','ppronfreq')
    ~ 'pronons',
    orig %in% c('verbfreq','auxverbfreq','pastfreq','presentfreq','futurefreq')
    ~ 'verbs',
    orig %in% c('articlefreq','adverbfreq','prepsfreq','conjfreq','negatefreq','quantfreq','numberfreq','swearfreq','functfreq')
    ~ 'otherfunctional',
    orig %in% c('socialfreq','familyfreq','friendfreq','humansfreq')
    ~ 'people',
    orig %in% c('affectfreq','posemofreq','negemofreq','anxfreq','angerfreq','sadfreq')
    ~ 'feelings',
    orig %in% c('cogmechfreq','insightfreq','causefreq','discrepfreq','tentatfreq','certainfreq','inhibfreq','inclfreq','exclfreq')
    ~ 'unlabeled',
    orig %in% c('perceptfreq','seefreq','hearfreq','feelfreq')
    ~ 'sense',
    orig %in% c('biofreq','bodyfreq','healthfreq','sexualfreq','ingestfreq')
    ~ 'bio',
    orig %in% c('relativfreq','motionfreq','spacefreq','timefreq')
    ~ 'physics',
    orig %in% c('workfreq','achievefreq','leisurefreq','homefreq','moneyfreq','religfreq','deathfreq')
    ~ 'life',
    orig %in% c('assentfreq','nonflfreq','fillerfreq', 'absolutistfreq')
    ~ 'idk'))

clusters <- read_csv(here('static','data','redditProject','centers')) 


pcs <- distinct(corlong['red_dim'])
cs <- distinct(clusters['X1'])

ui <- fluidPage(
  titlePanel("PCA Correlations"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "pc",
                  label = "Principle Component/New Dimemsion",
                  choices=pcs,
                  selected='d1')
    ),
    selectInput(inputId = "clusterN",
                label = "Cluster",
                choices = list("Choice 1" = 1, 
                               "Choice 2" = 2, 
                               "Choice 3" = 3,
                               "Choice 4" = 4, 
                               "Choice 5" = 5, 
                               "Choice 6" = 6,
                               "Choice 7" = 7#, 
                               #"Choice 8" = 8, 
                               #"Choice 9" = 9,
                               #"Choice 10" = 10, 
                               #"Choice 11" = 11
                ),
                selected=1)
  ),
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput(outputId = "corPlot"),
    plotOutput(outputId = "clusterPlot")
  )
) 


server <- function(input, output) {
  output$corPlot <- renderPlot({
    workingdf <- corlong %>%  
      filter(red_dim==input$pc) %>% 
      mutate(poscor=abs(correlation), sign=case_when(correlation>=0~'pos', correlation<0~'neg'))
    
    ggplot(workingdf, aes(x=fct_reorder(orig,poscor,.desc = TRUE), y=poscor, color=dictcat, fill=sign))+
      geom_col()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_fill_manual(values=c("gray80","#FFFFFF"))
    
  })
  
  output$clusterPlot <- renderPlot({
    radar_colour=c("#FF5A5F", "#FFB400", "#007A87",  "#8CE071", "#7B0051", "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20")
    
    clusters <- read_csv(here('static','data','redditProject','centers')) 
    
    clusters <- clusters %>%  
      mutate_at(vars(starts_with('d')),rescale)
    
    clusterN=as.numeric(input$clusterN)
    print(clusterN)
    ggradar(clusters[clusterN,], grid.label.size = 5,
            group.point.size = 3,
            axis.label.size = 4,
            #axis.labels = dlabels,
            legend.text.size = 11,
            group.colours = radar_colour[clusterN])+
      theme(legend.position="bottom") 
  })


shinyApp(ui = ui, server = server)