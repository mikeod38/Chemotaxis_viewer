library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(here)
library(purrr)

gene_names <- read.csv("./Data/gene_names.csv") %>% mutate(gene_name = stringr::str_wrap(gene_name, width = 20))

dataset <- read.csv("./Data/AH_chemotaxis.csv") %>%
  dplyr::mutate(n = (A.Cue. +  B) + (E + F.EtOH.),
         CI = ((A.Cue. +  B) - (E + F.EtOH.)) /  n ) %>%
  dplyr::left_join(.,gene_names) %>% dplyr::mutate(genotype = factor(gene_name))

fluidPage(

  titlePanel("Chemotaxis viewer"),

  sidebarPanel(

    selectInput('x', 'X', names(dataset), selected ="assay"),
    selectInput('y', 'Y', names(dataset), selected = "CI"),

    selectizeInput("genotype_name", label = "Genotype", choices = levels(dataset$genotype),
                   multiple = TRUE, selected = "N2"),
    selectizeInput("dates", label = "Subset specific dates", choices = c('None',levels(dataset$date)),
                   multiple = TRUE, selected = 'None'),
    checkboxInput('restrict_dates', 'Restrict to common dates'),
    selectizeInput("conditions", label = "conditions", choices = c('None',levels(dataset$assay)),
                   multiple = TRUE, selected = c('hex', 'hex-IAA')),
    selectInput('color', 'Point color', c('None', names(dataset))),
    selectInput('fill', 'Fill color', c('None', names(dataset))),

    checkboxInput('jitter', 'Jitter'),
    checkboxInput('smooth', 'Smooth'),

    selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
    selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)), selected = 'genotype')
  ),

  mainPanel(
    plotOutput('plot')
  )
)
