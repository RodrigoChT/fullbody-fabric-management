# install.packages('readtext')
# install.packages('plyr')
# install.packages('devtools')
# install.packages('rhandsontable')
# install_git("https://github.com/trestletech/shinyTable")
# install.packages('data.table')
# install.packages('shinythemes')
# install.packages('shinyjs')
# install.packages('shinyalert')
# install.packages('gtools')
# install.packages('DT')
# install.packages('knitr')
# install.packages('pander')
# install.packages('xtable')
# install.packages('lubridate')
# install.packages('rmarkdown')

library(devtools)
library(shinyTable)
library(shinythemes)
library(data.table)
library(rhandsontable)
library(shinyalert)
library(gtools)
library(DT)
library(rmarkdown)
library(knitr)
library(pander)
library(xtable)

#### Global parameters ----
stripped.dt.options <- list(paging = F,
                            searching = F,
                            info = F)

column.style <- 'margin-top: 25px;'
wellPanel.style <- 'padding: 10px'
dt.options <- list(dom = 'Bfrtip',
                   buttons = c('csv',
                               'excel',
                               'print',
                               'colvis'))

#### Utils ----
simple_cap <- function(x) {
  s <- tolower(strsplit(as.character(x), " ")[[1]])
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}

#### Confirmations ----
success.message <- function(succes.type = 'new.element',
                            added.msg = '') {
  
  if(succes.type == 'new.element') {
    ok.message <- 'Elemento agregado.'
  } else if (succes.type == 'operation') {
    ok.message <- 'Operación exitosa'
  }
  
  ok.message <- paste0(ok.message, added.msg)
  
  shinyalert(
    title = "Confirmación",
    text = ok.message,
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "success",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )

}

fail.message <- function(fail.type, prob.elements = NA) {
  
  if (fail.type == 'max.quant') {
    error.message <- paste0('La cantidad elegida es mayor a la cantidad disponible en el inventario (',
                            paste(prob.elements, collapse = ', '),
                            ').')
  } else if (fail.type == 'no.elements') {
    error.message <- 'Elegir por lo menos 1 elemento.'
  } else if (fail.type == 'repeated.elements') {
    error.message <- 'Hay elementos repetidos en la lista.'
  } else if (fail.type == 'wrong.password') {
    error.message <- 'Clave incorrecta.'
  }
  
  
  shinyalert(
    title = "Error!",
    text = error.message,
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "error",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
}


#### Checks ----
positive.number <- function(numbers, names) {
  check.pass <- T
  
  for (i in 1:length(numbers)) {

    if (any(is.na(numbers[i]),
            !is.numeric(numbers[i]),
            numbers[i] < 0)) {
      
      check.pass <- F
      
      shinyalert(
        title = "Error",
        text = paste0(names[i], ' debe ser un número positivo.'),
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      
      break
    }
      
      
  }
  return(check.pass)
}

repeated.names <- function(elements = c()) {
  check.pass <- F
  prob.elements <- c()
  
  for (i in 1:length(elements)) {
    if (simple_cap(elements[[i]][[2]]) %in% sapply(elements[[i]][[3]],
                                                   simple_cap)) {
      prob.elements <- c(prob.elements, elements[[i]][[1]])
    }
  }
  
  if (length(prob.elements) == 0) {
    check.pass <- T
  } else {
    shinyalert(
      title = "Error",
      text = paste0("El nombre del elemento {",
                    paste0(prob.elements, collapse = ", "),
                    "} ya se encuentra registrado."),
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  }
  
  return(check.pass)
  
}

#### Misc ----
reset.inputs <- function(section,
                         input.list) {
  for (i in 1:length(input.list)) {
    input.name <- paste0(section,
                         '.',
                         input.list[i])
    
    shinyjs::reset(input.name)
  }
}