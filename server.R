library(shiny)
library(ggplot2)
library(here)
library(dplyr)
library(magrittr)
library(purrr)

function(input, output) {
  # helper function to get common dates between gentoypes:
  date_getter <- function(df) {
    purrr::map(input$genotype_name, function(x) {
      df %>% 
        filter(genotype == x) %>%
        droplevels() %>%
        select(date) %>%
        unique()
    }) %>%
      Reduce(function(dtf1,dtf2) inner_join(dtf1,dtf2,by="date"), .)
  }
  gene_names <- read.csv("./Data/gene_names.csv") %>% mutate(gene_name = stringr::str_wrap(gene_name, width = 20))

  
  dataset <- reactive({
    genotype_list <- input$genotype_name
    
    if(input$restrict_dates) {
      dataset <- read.csv("./Data/AH_chemotaxis.csv") %>%
        dplyr::mutate(n = (A.Cue. +  B) + (E + F.EtOH.),
                      CI = ((A.Cue. +  B) - (E + F.EtOH.)) /  n ) %>%
        dplyr::left_join(.,gene_names) %>% dplyr::mutate(genotype = gene_name) %>%
        dplyr::filter(genotype %in% input$genotype_name,
                      #date %in% input$dates,
                      assay %in% input$conditions) %>% droplevels() %>%
        dplyr::mutate(genotype = factor(genotype, levels = input$genotype_name))
      dataset %<>% dplyr::filter(date %in% date_getter(dataset)$date)
    } else {
      if (input$dates != 'None')
      {dataset <- read.csv("./Data/AH_chemotaxis.csv") %>%
        dplyr::mutate(n = (A.Cue. +  B) + (E + F.EtOH.),
               CI = ((A.Cue. +  B) - (E + F.EtOH.)) /  n ) %>%
        dplyr::left_join(.,gene_names) %>% dplyr::mutate(genotype = gene_name) %>%
        dplyr::filter(genotype %in% input$genotype_name,
               date %in% input$dates,
               assay %in% input$conditions) %>% droplevels() %>%
        dplyr::mutate(genotype = factor(genotype, levels = input$genotype_name))
      } else {
      dataset <- read.csv("./Data/AH_chemotaxis.csv") %>%
        dplyr::mutate(n = (A.Cue. +  B) + (E + F.EtOH.),
               CI = ((A.Cue. +  B) - (E + F.EtOH.)) /  n ) %>%
        dplyr::left_join(.,gene_names) %>% dplyr::mutate(genotype = gene_name) %>%
        dplyr::filter(genotype %in% input$genotype_name,
               assay %in% input$conditions) %>% droplevels() %>%
        dplyr::mutate(genotype = factor(genotype, levels = input$genotype_name))
      }
      }
    })


  output$plot <- renderPlot({
    p <- ggplot(dataset(),
                aes_string(x=input$x, y=input$y))

    # optional color fill
    if (input$fill != 'None') {
      p <- p + geom_boxplot(aes_string(fill=input$fill))
    } else {
      p <- p + geom_boxplot()
    }

    # optional point fill and jitter plot
    if (input$color != 'None' & input$jitter) {
      p <- p + geom_jitter(aes_string(color=input$color))
    } else {
      if (input$jitter) {
        p <- p + geom_jitter()
      }
    }

    # optional point fill and smooth point plot
    if (input$color != 'None' & input$smooth) {
      p <- p + geom_point(aes_string(color=input$color))
    } else {
      if (input$smooth) {
        p <- p + geom_point()
      }
      }

    # optional facets
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .') {
      p <- p + facet_grid(facets)
    }


    # set facets text angle for a large numbers of genotypes
    if (length(unique(input$genotype_name)) < 6) {
      p<- p + theme_classic() +
              # viridis::scale_color_viridis(discrete = TRUE, option = "inferno") +
              viridis::scale_fill_viridis(discrete = TRUE, option = "viridis", begin = 0.15) +
              theme(strip.background = element_blank(),
                    text = element_text(size = 20),
                    axis.text.x = element_text(angle = 45,
                                               hjust = 1))
    } else {
      p <- p + theme_classic() +
              # viridis::scale_color_viridis(discrete = TRUE, option = "inferno") +
              viridis::scale_fill_viridis(discrete = TRUE, option = "viridis", begin = 0.15) +
              theme(strip.background = element_blank(),
                    text = element_text(size = 20),
                    strip.text = element_text(size = 12,
                                              angle = 60),
                    axis.text.x = element_text(angle = 45,
                                               hjust = 1))
    }

    print(p)

          }, height=700)

}
