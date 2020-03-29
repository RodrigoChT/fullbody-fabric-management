shinyServer(function(input, output, session) {
  
  #### Load data ----
  
  inserted.op <- c()
  inserted.samples <- c()
  
  #bases.load <- read.csv('./Data/bases.csv')
  movements <- readRDS('./Data/inventory_fabrics.rds') # TURN THE REST TO DATA TABLE
  #saveRDS(movements, './Data/inventory_fabrics.rds')
  fabrics <- readRDS('./Data/fabrics.rds')
  #saveRDS(fabrics, './Data/fabrics.rds')
  fabric.sub.types <- readRDS('./Data/fabric_sub_types.rds')
  #saveRDS(fabric.sub.types, './Data/fabric_sub_types.rds')
  fabric.types <- readRDS('./Data/fabric_types.rds')
  providers <- readRDS('./Data/providers.rds')
  contractors <- readRDS('./Data/contractors.rds')
  garments <- readRDS('./Data/garments.rds')
  pos.inputs <- readRDS('./Data/pos_inputs.rds')
  #saveRDS(pos.inputs, './Data/pos_inputs.rds')
  pos.outputs <- readRDS('./Data/pos_outputs.rds')
  #saveRDS(pos.outputs, './Data/pos_outputs.rds')
  pos.outputs.flow <- readRDS('./Data/pos_outputs_flow.rds')
  #saveRDS(pos.outputs.flow, './Data/pos_outputs_flow.rds')
  
  #### Initalize reactive values ----
  bases <- reactiveValues(fabrics = fabrics,
                          fabric.types = fabric.types,
                          fabric.sub.types = fabric.sub.types,
                          providers = c('Ninguno', providers),
                          movements = movements,
                          contractors = c('Ninguno', contractors),
                          garments = as.character(garments),
                          pos.table = data.table(), # PENDING
                          pos.inputs = pos.inputs,
                          pos.outputs = pos.outputs,
                          pos.outputs.flow = pos.outputs.flow,
                          inv = data.table(),
                          po.costs = data.table())
  
  triggers <- reactiveValues(pos = 0,
                             pos2 = 0)
  
  show.tables <- reactiveValues(pos.output = NA,
                                pos.inputs = NA)
  
  misc <- reactiveValues(po.hand.restart = 1)
  
  #### Static inputs ----
  date.input <- function(id) {
    renderUI(
      dateInput(id,
                'Fecha',
                format = 'dd-mm-yyyy',
                language = 'es')
    )
  }
  
  id.input <- function(id) {
    renderUI(
      textInput(id,
                'ID')
    )
  }
  
  kg.input <- function(id, limit = NA) {
    renderUI(
      numericInput(id,
                   'Kg',
                   value = NA_integer_,
                   max = limit)
    )
  }
  
  mt.input <- function(id, limit = NA) {
    renderUI(
      numericInput(id,
                   'Mt',
                   value = 0,
                   max = limit)
    )
  }
  
  quantity.input <- function(id, limit = NA, value = 0) {
    renderUI(
      numericInput(id,
                   'Cantidad',
                   value = value,
                   max = limit)
    )
  }
  
  price.input <- function(id, limit = NA) {
    renderUI(
      numericInput(id,
                   'Costo (S/.)',
                   value = 0,
                   max = limit)
    )
  }
  
  #### Dynamic inputs ----
  
  type.input <- function(id) {
    renderUI(
      selectizeInput(id,
                     label = 'Tipo',
                     choices = unique(bases$fabric.types))
    )
  }
  
  sub.type.input <- function(id, fabric.type) {
    renderUI(
      selectizeInput(id,
                     label = 'Sub tipo',
                     choices = bases$fabric.sub.types[tipo %in% fabric.type,
                                                      sub_tipo])
    )
  }
  
  
  fabric.input <- function(id, choices = NULL) {
    if (is.null(choices)) {
      renderUI(
        selectizeInput(id,
                       label = 'Tela',
                       choices = bases$fabrics$nombre_bautizado))
    } else {
      renderUI(
        selectizeInput(id,
                       label = 'Tela',
                       choices = choices))
    }
    
  }
  
  provider.input <- function(id) {
    renderUI(
      selectizeInput(id,
                     label = 'Proveedores',
                     choices = bases$providers))
  }
  
  contractor.input <- function(id) {
    renderUI(
      selectizeInput(id,
                     label = 'Contratista',
                     choices = bases$contractors))
  }
  
  garment.input <- function(id) {
    renderUI(
      selectizeInput(id,
                     label = 'Prenda',
                     choices = bases$garments))
  }
  
  num.garment.input <- function(id, limit = NA) {
    renderUI(
      numericInput(id,
                   label = 'Num. prendas',
                   value = 0,
                   max = limit)
    )
  }
  
  po.ids <- function(id) {
    #limit <- max(bases$pos.outputs$id)
    renderUI(
      numericInput(id,
                   label = 'ID de OP',
                   value = max(c(1,  max(bases$pos.outputs$id)), na.rm = T),
                   max =  max(bases$pos.outputs$id))
    )
  }
  
  # List of unlimited inputs for new OPs
  get.inputs <- function(input.button, list.name) {
    observeEvent(input[[input.button]], {
      
      # Create names for elments
      add <- input[[input.button]]
      type.id <- paste('type.id', add)
      fabric.id <- paste0('fabric.id.', add)
      kg.id <- paste0('kg.id.', add)
      remove.id <- paste0('remove.id.', add)
      item.id <- paste0(list.name, 'ItemId', add)
      
      
      # Add widgets
      insertUI(
        selector = paste0('#inputList', list.name),
        ui = tags$div(id = item.id,
                      tags$style(type='text/css', ".selectize-input {height: 42px; }"),
                      tags$div(style = 'margin-top: 20px; display:inline-block;vertical-align:top',
                               fabric.input(id = fabric.id#,
                                            #choices = bases$fabrics[tipo = '', ]
                               )),
                      tags$div(style = 'margin-top: 20px; display:inline-block;vertical-align:top',
                               kg.input(id = kg.id)),
                      tags$div(style = 'margin-top: 45px; display:inline-block;vertical-align:top',
                               actionButton(remove.id, 
                                            label = '',
                                            icon = icon('remove-circle',
                                                        lib = 'glyphicon'))
                      )
                      
        )
      )
      
      if (list.name == 'po') {
        # Remove specific elements
        observeEvent(input[[remove.id]], {
          removeUI(selector = paste0('#', item.id))
          inserted.op <<- setdiff(inserted.op, add)
          #sales.inserted.op <<- setdiff(sales.inserted.op, add)
        })
        
        inserted.op <<- c(inserted.op, add)
        
      } else if (list.name == 'outgoing') {
        # Remove specific elements
        observeEvent(input[[remove.id]], {
          removeUI(selector = paste0('#', item.id))
          inserted.samples <<- setdiff(inserted.samples, add)
          #sales.inserted.samples <<- setdiff(sales.inserted.samples, add)
        })
        
        inserted.samples <<- c(inserted.samples, add)
      }
      
    })
  }
  get.inputs('po.add.input', 'po')
  get.inputs('outgoing.samples.input', 'outgoing')
  
  #### Transaction buttons ----
  add.base.button <- function(type) {
    observeEvent(input[[paste0('add.', type, '.process')]], {
      
      # Get new value
      new.value <- input[[paste0('add.', type, '.name')]]
      
      
      # Check if new value is already present
      type.plural <- paste0(type, 's')
      
      if (tolower(new.value) %in% tolower(bases[[type.plural]])) {
        
        fail.message('repeated.elements')
        
      } else {
        
        # Append new value
        bases[[type.plural]] <- append(bases[[type.plural]], new.value)
        
        # Store new base
        saveRDS(bases[[type.plural]], 
                paste0('./Data/', 
                       type.plural,
                       '.rds'))
        
        # Delete input value
        updateTextInput(session, paste0('add.', type, '.name'), value = '')
        
        # Success message box
        success.message()
      }
      
      
      
    })
  }
  
  #### Incomings ----
  output$incoming.date <- date.input('incoming.date')
  output$incoming.provider <- provider.input('incoming.provider')
  output$incoming.fabric <- fabric.input('incoming.fabric')
  output$incoming.kg <- kg.input('incoming.kg')
  output$incoming.mt <- mt.input('incoming.mt')
  output$incoming.price <- price.input('incoming.price')
  
  # Process incoming fabrics
  observeEvent(input$incoming.process, {
    
    # Check if inputs are positive numbers
    check.pass <- positive.number(c(input$incoming.kg,
                                    input$incoming.mt,
                                    input$incoming.price),
                                  c('Kg.',
                                    'Mt.',
                                    'Costo'))
    
    if (check.pass) {
      
      next.id <- paste0('I_', max(as.integer(c(0, sub('I_', '', bases$movements[grep('I_', id), id]))), na.rm = T) + 1)
      
      # Rows to add
      temp.dt <- data.table(tipo = bases$fabrics[nombre_bautizado == input$incoming.fabric, tipo][1],
                            sub_tipo = bases$fabrics[nombre_bautizado == input$incoming.fabric, sub_tipo][1],
                            nombre_comercial = bases$fabrics[nombre_bautizado == input$incoming.fabric, nombre_comercial][1],
                            nombre_bautizado = input$incoming.fabric,
                            kg = input$incoming.kg,
                            metros = input$incoming.mt,
                            costo = round(input$incoming.price * input$incoming.kg, 2),
                            costo_unitario = round(input$incoming.price / input$incoming.kg, 2),
                            id = next.id,
                            id1 = input$incoming.id1,
                            id2 = input$incoming.id2,
                            contraparte = input$incoming.provider,
                            fecha = as.character(input$incoming.date),
                            operacion = 'Compra')
      
      # Merge and save
      bases$movements <- rbindlist(list(bases$movements,
                                        temp.dt),
                                   fill = T)
      
      saveRDS(bases$movements, './Data/inventory_fabrics.rds')
      
      # Sucess message
      success.message()
      
      # Clean inputs
      reset.inputs('incoming',
                   c('id1',
                     'id2',
                     'date',
                     'provider',
                     'fabric',
                     'kg',
                     'mt',
                     'price'))
    }
    
    
  })
  
  
  #### PO ----
  ## __New PO ----
  output$po.id <- id.input('po.id')
  output$po.date <- date.input('po.date')
  output$po.contractor <- contractor.input('po.contractor')
  #output$po.fabric <- fabric.input('po.fabric')
  #output$po.mt <- mt.input('po.mt') #, bases$inv$metros[bases$inv['nombre_bautizado'] ==  input$po.fabric])
  output$po.garment <- garment.input('po.garment')
  output$po.num.garment <- num.garment.input('po.num.garment')
  
  # Process PO
  observeEvent(input$po.process, {
    
    # Check that we have at least 1 input
    if(length(inserted.op) > 0) {
      
      # Get inputs
      input.names <- c()
      input.names.2nd <- c()
      input.quant <- c()
      for (i in 1:length(inserted.op)) {
        input.names <- c(input.names, input[[paste0('fabric.id.',
                                                    inserted.op[i])]])
        input.names.2nd <- c(input.names.2nd, as.character(bases$fabrics[nombre_bautizado == input.names[i],
                                                            nombre_comercial]))
        input.quant <- c(input.quant, input[[paste0('kg.id.',
                                                    inserted.op[i])]])
      }
      
      # Check that there are no repeated inputs
      
      if (length(input.names) == length(unique(input.names))) {
        
        # Check if inputs are positive numbers
        check.pass <- positive.number(input.quant,
                                      input.names)
        
        if (check.pass) {
        
          # Check that the inventory has enough materials
          prob.elements <- c()
          for (i in 1:length(input.names)) {
            if (!input.names[i] %in% bases$inv$nombre_bautizado) {
              prob.elements <- c(prob.elements, input.names[i])
            } else if (input.quant[i] > bases$inv[nombre_bautizado == input.names[i], cantidad]) {
              prob.elements <- c(prob.elements, input.names[i])
            }
          }
          
          if (length(prob.elements) == 0) {
            # Grab next PO id
            
            if (dim(bases$pos.outputs)[1] == 0) {
              next.id <- 1
            } else {
              next.id <- max(bases$pos.outputs$id) + 1
            }
            
            # Create temporary tables for inputs and output
            temp.dt.input <- data.table(id = next.id,
                                        id_op = paste0('OP_', next.id),
                                        id1 = input$po.id,
                                        fecha = as.character(input$po.date),
                                        nombre_bautizado = input.names,
                                        cantidad_original = input.quant,
                                        entregado = 0,
                                        mano_obra = input$po.labor.cost,
                                        contraparte = input$po.contractor)
            
            temp.dt.output <- data.table(id = next.id,
                                         id_op = paste0('OP_', next.id),
                                         id1 = input$po.id,
                                         fecha = as.character(input$po.date),
                                         prenda = input$po.garment,
                                         cantidad_original = input$po.num.garment,
                                         corte = 0,
                                         recibido = 0,
                                         estado = 'incompleto')
            
            # Append temporary tables to original ones and store them
            bases$pos.inputs <- rbindlist(list(bases$pos.inputs, temp.dt.input), fill = T)
            bases$pos.outputs <- rbindlist(list(bases$pos.outputs, temp.dt.output), fill = T)
            
            saveRDS(bases$pos.inputs, './Data/pos_inputs.rds')
            saveRDS(bases$pos.outputs, './Data/pos_outputs.rds')
            
            # Add to movements the handing of materials
            temp.dt <- data.table(nombre_bautizado = input.names,
                                  nombre_comercial = input.names.2nd,
                                  kg = input.quant,
                                  fecha = as.character(input$po.date),
                                  id = paste0('OP_', next.id),
                                  operacion = 'OP-Entrega')
            
            bases$movements <- rbindlist(list(bases$movements, temp.dt), fill = T)
            
            saveRDS(bases$movements, './Data/inventory_fabrics.rds')
            
            # Clean inputs
            reset.inputs('po',
                         c('id',
                           'date',
                           'contractor',
                           'garment',
                           'num.garment',
                           'type',
                           'labor.cost'))
            
            # Remove items from inserted list
            for (item in inserted.op) {
              removeUI(selector = paste0('#poItemId', item))
            }
            
            # Clean inserted list
            inserted.op <<- c()
            
            # Sucess message
            success.message(added.msg = paste0('\nCódigo de OP: ',
                                               next.id))
            
            # Activate trigger
            triggers$pos <- triggers$pos + 1
            
            # Hide table
            shinyjs::hide('po.check')
            
          } else {
            fail.message('max.quant', prob.elements)
          }
        
        }
        
        
      } else {
        fail.message('repeated.elements')
      }
      
    } else {
      fail.message('no.elements')
    }
    
  })
  
  # Check PO
  observeEvent(input$po.check.button, {
    
    # Get inputs
    input.names <- c()
    input.names.2nd <- c()
    input.quant <- c()
    
    if (length(inserted.op) > 0) {
      for (i in 1:length(inserted.op)) {
        input.names <- c(input.names, input[[paste0('fabric.id.',
                                                    inserted.op[i])]])
        input.names.2nd <- c(input.names.2nd, as.character(bases$fabrics[nombre_bautizado == input.names[i],
                                                                         nombre_comercial]))
        input.quant <- c(input.quant, input[[paste0('kg.id.',
                                                    inserted.op[i])]])
      }
      
      # Create temporary tables for inputs and output
      temp.dt.input <- data.table(nombre_bautizado = input.names,
                                  cantidad = input.quant)
      temp.dt.input <- rbindlist(list(temp.dt.input,
                                      data.table(nombre_bautizado = 'Total',
                                                 cantidad = sum(temp.dt.input$cantidad, na.rm = T))))
      output$po.check <- renderTable(temp.dt.input)
    }
    
    # Show table
    shinyjs::show('po.check')
    
  })
  
  ## __Hand materials ----
  output$po.hand.id <- po.ids('po.hand.id')
  output$po.hand.fabric <- fabric.input('po.hand.fabric', c(1))
  output$po.hand.date <- date.input('po.hand.date')
  
  # What tables to show and options to hand out materials
  observeEvent(c(input$po.hand.id,
                 input$po.supplies.modify.process,
                 bases$pos.inputs), {
                   
                   # Sub tables for POs
                   show.tables$pos.inputs <- bases$pos.inputs[id %in% input$po.hand.id,]
                   
                   output$po.hand.input <- renderDataTable({
                     show.tables$pos.inputs[id == input$po.hand.id, .(id_op,
                                                                      fecha,
                                                                      nombre_bautizado, 
                                                                      cantidad_original,
                                                                      entregado)]
                   },
                   options = stripped.dt.options)
                   
                   # Input widgets
                   output$po.hand.fabric <- fabric.input('po.hand.fabric'#,
                                                         #show.tables$pos.inputs$nombre_bautizado
                                                         )
                 })
  
  # Update max quantity for input (based in current inventory)
  observeEvent(c(input$po.hand.fabric,
                 input$po.hand.process,
                 bases$movements), {
                   output$po.hand.quant <- kg.input('po.hand.quant',
                                                    bases$inv[nombre_bautizado == input$po.hand.fabric, cantidad])
                 })
  
  # Process
  observeEvent(input$po.hand.process, {
    
    # Check if inputs are positive numbers
    check.pass <- positive.number(c(input$po.hand.quant),
                                  c('Kg.'))
    
    if (check.pass) {
      
      # Check if there is enough inventory for the operation
      
      if (input$po.hand.quant <= bases$inv[nombre_bautizado == input$po.hand.fabric, cantidad]) {
        
        # Make changes in movements
        temp.dt <- data.table(nombre_bautizado = input$po.hand.fabric,
                              nombre_comercial = bases$fabrics[nombre_bautizado == input$po.hand.fabric, nombre_comercial],
                              kg = input$po.hand.quant,
                              fecha = as.character(input$po.hand.date),
                              id = paste0('OP_', input$po.hand.id),
                              operacion = 'OP-Entrega')
        
        bases$movements <- rbindlist(list(bases$movements, temp.dt), fill = T)
        
        saveRDS(bases$movements, './Data/inventory_fabrics.rds')
        
        # Update pos.inputs table
        if (!input$po.hand.fabric %in% bases$pos.inputs[id == input$po.hand.id, nombre_bautizado]) {
          temp.dt.input <- data.table(id = input$po.hand.id,
                                      id_op = paste0('OP_', input$po.hand.id),
                                      id1 = bases$pos.inputs[id == input$po.hand.id, id1][1],
                                      fecha = as.character(input$po.hand.id),
                                      nombre_bautizado = input$po.hand.fabric,
                                      cantidad_original = 0,
                                      entregado = input$po.hand.quant,
                                      mano_obra = bases$pos.inputs[id == input$po.hand.id, mano_obra][1],
                                      contraparte = bases$pos.inputs[id == input$po.hand.id, contraparte][1])
          
          bases$pos.inputs <- rbindlist(list(bases$pos.inputs, temp.dt.input), fill = T)

          saveRDS(bases$pos.inputs, './Data/pos_inputs.rds')

        }
        
        
        # Clean inputs
        reset.inputs('po.hand',
                     c('fabric',
                       'date',
                       'quant'))
        
        # Success messages
        success.message('operation')
        
        
        
      } else {
        # Fail message
        fail.message('max.quant',
                     input$po.hand.fabric)
      }
    }
    
    
    
    
    
  })
  
  ## __Receive garments ----
  output$po.receive.id <- po.ids('po.receive.id')
  output$po.receive.date <- date.input('po.receive.date')
  
  
  # Tables to show
  observeEvent(c(input$po.receive.id,
                 input$po.products.modify.password,
                 bases$pos.outputs.flow), {
                   
                   # Show output of correct PO id
                   show.tables$pos.outputs <- bases$pos.outputs[id %in% input$po.receive.id,]
                   
                   output$po.receive.quant <- quantity.input('po.receive.quant',
                                                             value = NA)
                   
                   output$po.receive.output <- renderDataTable({
                     show.tables$pos.outputs[id == input$po.receive.id, .(id_op,
                                                                       fecha,
                                                                       prenda,
                                                                       cantidad_original,
                                                                       corte,
                                                                       recibido,
                                                                       estado)]
                   },
                   options = stripped.dt.options)
                 })
  
  # Process
  observeEvent(input$po.receive.process, {
    
    # Check if inputs are positive numbers
    check.pass <- positive.number(c(input$po.receive.quant),
                                  c('Cantidad'))
    
    if (check.pass) {
      
      # Update movements of garments
      temp.dt <- data.table(id = paste0('OP_', input$po.receive.id), 
                            id1 = input$po.receive.id1, 
                            prenda = bases$pos.outputs[id == input$po.receive.id,
                                                       prenda][1],
                            fecha = as.character(input$po.receive.date), 
                            recibido = input$po.receive.quant)
      
      bases$pos.outputs.flow <- rbindlist(list(bases$pos.outputs.flow,
                                               temp.dt),
                                          fill = T)
      
      saveRDS(bases$pos.outputs.flow, './Data/pos_outputs_flow.rds')
      
      # Clean inputs
      reset.inputs('po.receive',
                   c('fabric',
                     'id1',
                     'date',
                     'quant'))
      
      # Activate trigger
      triggers$pos <- triggers$pos + 1
      
      # Success message
      success.message('operation')
    }
    
  })
  
  # Update total received garments in pos.outputs table
  observeEvent(bases$pos.outputs.flow, {
    
    # Update and save table
    bases$pos.outputs <- merge(bases$pos.outputs,
                               bases$pos.outputs.flow[,
                                                      .(total = sum(recibido)),
                                                      by = id],
                               by.x = 'id_op',
                               by.y = 'id',
                               all.x = T)
    
    bases$pos.outputs[is.na(total), total := 0]
    bases$pos.outputs[, recibido := total]
    bases$pos.outputs[, total := NULL]
    
    saveRDS(bases$pos.outputs, './Data/pos_outputs.rds')
    
  })
  
  
  # Change completion status of OP
  observeEvent(input$po.receive.complete, {
    
    current.status <- bases$pos.outputs[id == input$po.receive.id, estado]
    
    if (current.status == 'completo') {
      bases$pos.outputs[id == input$po.receive.id, estado := 'incompleto']
    } else if (current.status == 'incompleto') {
      bases$pos.outputs[id == input$po.receive.id, estado := 'completo']
    }
    
    # Save table
    saveRDS(bases$pos.outputs, './Data/pos_outputs.rds')
    
    # Update showed table
    show.tables$pos.outputs <- bases$pos.outputs[id == input$po.receive.id,]
    
    # Activate trigger
    triggers$pos <- triggers$pos + 1
    
    # Success message
    success.message('operation')
    
  })
  
  ## __Cut garments ----
  output$po.cuts.id <- po.ids('po.cuts.id')
  
  
  # Tables to show
  observeEvent(c(input$po.cuts.id,
                 input$po.products.modify.password), {
                   
                   # Quantity widget
                   output$po.cuts.quant <- quantity.input('po.cuts.quant')
              
                   # Show output of correct PO id
                   show.tables$pos.outputs <- bases$pos.outputs[id %in% input$po.cuts.id,]
                   output$po.cuts.output <- renderDataTable({
                     show.tables$pos.outputs[id %in% input$po.cuts.id, .(id_op,
                                                                       fecha,
                                                                       prenda,
                                                                       cantidad_original,
                                                                       corte,
                                                                       recibido,
                                                                       estado)]
                   },
                   options = stripped.dt.options)
                 })
  
  # Process
  observeEvent(input$po.cuts.process, {
    
    # Check if inputs are positive numbers
    check.pass <- positive.number(c(input$po.cuts.quant),
                                  c('Cantidad'))
    
    if (check.pass) {
      # Update and save data
      bases$pos.outputs[id == input$po.cuts.id, corte := input$po.cuts.quant]
      saveRDS(bases$pos.outputs, './Data/pos_outputs.rds')
      
      # Update showed table
      show.tables$pos.outputs <- bases$pos.outputs[id == input$po.cuts.id,]
      
      # Clean inputs
      reset.inputs('po.cuts',
                   c('quant'))
      
      # Activate trigger
      triggers$pos <- triggers$pos + 1
      
      # Success message
      success.message('operation')
    }
    
  })
  
  ## __ Info (and delete) OP ----
  output$po.info.id <- po.ids('po.info.id')
  
  # Show tables of selected PO
  observeEvent(c(input$po.info.id,
                 bases$pos.inputs,
                 bases$pos.outputs,
                 bases$pos.outputs.flow,
                 input$po.cuts.process,
                 input$incoming.process,
                 triggers$pos2), {
                   
                   show.tables$po.info.output <- bases$pos.outputs[id %in% input$po.info.id,]
                   
                   output$po.info.output <- renderDataTable({
                     show.tables$po.info.output
                   })
                   
                   output$po.info.general <- renderDataTable({
                     datatable(data.table(a = c('ID sistema',
                                                'ID 1',
                                                'Fecha',
                                                'Contratista',
                                                'Mano de obra',
                                                'Costo total',
                                                'Costo unitario', 
                                                'Estado'
                                                ),
                                          b = c(input$po.info.id,
                                                bases$pos.inputs[id == input$po.info.id, id1][1],
                                                bases$pos.inputs[id == input$po.info.id, fecha][1],
                                                bases$pos.inputs[id == input$po.info.id, contraparte][1],
                                                bases$pos.inputs[id == input$po.info.id, mano_obra][1],
                                                round(bases$po.costs[id_op == paste0('OP_',
                                                                                     input$po.info.id),
                                                                     costo_total],
                                                      digits = 2),
                                                round(bases$po.costs[id_op == paste0('OP_',
                                                                                     input$po.info.id),
                                                                     costo_unitario],
                                                      digits = 2),
                                                bases$po.costs[id_op == paste0('OP_',
                                                                               input$po.info.id),
                                                               estado]
                                                )
                                          ),
                               colnames = c('', ''),
                               options = stripped.dt.options,
                               rownames = F
                               )
                   }
                   )
                   
                   output$po.info.materials <- renderDataTable({
                     bases$pos.inputs[id == input$po.info.id, .(nombre_bautizado, 
                                                                       cantidad_original,
                                                                       entregado,
                                                                       costo = round(costo, 2))]
                   },
                   options = stripped.dt.options)
                   
                   output$po.info.garments <- renderDataTable({
                     data = bases$pos.outputs[id == input$po.info.id, .(prenda,
                                                                        cantidad_original,
                                                                        corte,
                                                                        recibido)]
                   },
                   options = stripped.dt.options)
                   
                   output$po.info.garments.flow <- renderDataTable({
                     data = bases$pos.outputs.flow[id == paste0('OP_', input$po.info.id),
                                                   .(id, 
                                                     id1,
                                                     fecha,
                                                     prenda,
                                                     recibido)]
                   },
                   options = stripped.dt.options)
                 })
  
  # download PDF
  observeEvent(input$po.info.download,
               {
                 Sys.setenv(RSTUDIO_PANDOC = 'C:/Program Files/RStudio/bin/pandoc')
                 render(input = 'op_document.Rmd',
                        output_format = 'pdf_document',
                        output_file = paste0('OP_',
                                             input$po.info.id),
                        output_dir = 'Reportes',
                        params = list(po.id = input$po.info.id,
                                      po.info.general = data.table(a = c('ID sistema',
                                                                         'ID alternativo',
                                                                         'Fecha registro',
                                                                         'Fecha impresión',
                                                                         'Contratista',
                                                                         'Mano de obra',
                                                                         'Costo total',
                                                                         'Costo unitario', 
                                                                         'Estado'
                                                                         ),
                                                                   b = c(input$po.info.id,
                                                                         bases$pos.inputs[id == input$po.info.id, id1][1],
                                                                         bases$pos.inputs[id == input$po.info.id, fecha][1],
                                                                         as.character(lubridate::today()), 
                                                                         bases$pos.inputs[id == input$po.info.id, contraparte][1],
                                                                         bases$pos.inputs[id == input$po.info.id, mano_obra][1],
                                                                         round(bases$po.costs[id_op == paste0('OP_',
                                                                                                        input$po.info.id),
                                                                                        costo_total],
                                                                               digits = 2),
                                                                         round(bases$po.costs[id_op == paste0('OP_',
                                                                                                              input$po.info.id),
                                                                                              costo_unitario],
                                                                               digits = 2),
                                                                         bases$po.costs[id_op == paste0('OP_',
                                                                                                        input$po.info.id),
                                                                                        estado]
                                                                         )
                                                                   ),
                                      po.info.garments = bases$pos.outputs[id == input$po.info.id, .(prenda,
                                                                                                     cantidad_original,
                                                                                                     corte,
                                                                                                     recibido)],
                                      po.info.materials = bases$pos.inputs[id == input$po.info.id, .(nombre_bautizado, 
                                                                                                     cantidad_original,
                                                                                                     entregado,
                                                                                                     round(costo, 2))],
                                      po.info.garments.flow = bases$pos.outputs.flow[id == paste0('OP_', input$po.info.id),
                                                                                     .(id, 
                                                                                       id1,
                                                                                       fecha,
                                                                                       prenda,
                                                                                       recibido)]
                                      
                                      )
                        )
                 system(paste0('open ".\\Reportes\\OP_',
                               input$po.info.id,
                               '.pdf"'))
                 success.message('operation')
               })
  
  # Delete all data and transactions related to selected OP
  observeEvent(input$po.info.delete, {
    if (input$po.info.password %in% c('clave')) {
      
      bases$pos.inputs <- bases$pos.inputs[id != input$po.info.id, ]
      saveRDS(pos.inputs, './Data/pos_inputs.rds')

      bases$pos.outputs <- bases$pos.outputs[id != input$po.info.id, ]
      saveRDS(pos.outputs, './Data/pos_outputs.rds')
      
      bases$pos.outputs.flow <- bases$pos.outputs.flow[id != paste0('OP_', input$po.info.id), ]
      saveRDS(pos.outputs.flow, './Data/pos_outputs_flow.rds')

      bases$movements <- bases$movements[id != paste0('OP_', input$po.info.id), ]
      
      success.message('operation')
      
    } else {
      fail.message('wrong.password')
    }
  })
  
  
  #### Adjustments ----
  ## __ Wild card ----
  output$adjustments.date <- date.input('adjustments.date')
  output$adjustments.fabric <- fabric.input('adjustments.fabric')
  #output$adjustments.kg <- kg.input('adjustments.kg')
  #output$adjustments.mt <- mt.input('adjustments.mt')
  #output$adjustments.provider <- provider.input('adjustments.provider')
  
  # Update max quantity for input (based in current inventory)
  observeEvent(c(input$adjustments.fabric,
                 input$adjustments.process,
                 input$adjustments.operation.type,
                 bases$movements), {
                   
                   kg.limit <- NA
                   if (input$adjustments.operation.type == 'Resta') {
                     kg.limit <- bases$inv[nombre_bautizado == input$adjustments.fabric, cantidad]
                     if (length(kg.limit) == 0) {
                       kg.limit <- 0
                     }
                   } 
                   
                   output$adjustments.kg <- kg.input('adjustments.quant',
                                                     kg.limit)
                 })
  
  # Process adjustments fabrics
  observeEvent(input$adjustments.process, {
    
    # Check if inputs are positive numbers
    check.pass <- positive.number(c(input$adjustments.quant),
                                  c('Cantidad'))
    
    if (check.pass) {
      
      # Check if there is enough inventory for the operation
      if (any(input$adjustments.quant <= bases$inv[nombre_bautizado == input$adjustments.fabric, cantidad],
          input$adjustments.operation.type == 'Suma')) {
        
        # Build correct id
        if (input$adjustments.operation.type == 'Suma') {
          id.code <- 'AS_'
        } else if (input$adjustments.operation.type == 'Resta') {
          id.code <- 'AR_'
        } else {
          stop('Unsupported operation.')
        }
        
        
        next.id <- paste0(id.code,
                          max(as.integer(c(0, sub(id.code,
                                                  '', 
                                                  bases$movements[grep(id.code, id), id]))), na.rm = T) + 1)
        
        # Rows to add
        temp.dt <- data.table(tipo = bases$fabrics[nombre_bautizado == input$adjustments.fabric, tipo],
                              sub_tipo = bases$fabrics[nombre_bautizado == input$adjustments.fabric, sub_tipo],
                              nombre_comercial = bases$fabrics[nombre_bautizado == input$adjustments.fabric, nombre_comercial],
                              nombre_bautizado = input$adjustments.fabric,
                              kg = input$adjustments.quant,
                              id = next.id,
                              #metros = input$adjustments.mt,
                              #costo = input$adjustments.price,
                              #costo_unitario = input$adjustments.price / input$adjustments.kg,
                              #id = input$adjustments.id1,
                              #id2 = input$adjustments.id2,
                              #contraparte = input$adjustments.provider,
                              fecha = as.character(input$adjustments.date),
                              detalle = input$adjustments.detail,
                              operacion = paste0('Ajuste-', input$adjustments.operation.type))
        
        # Merge and save
        bases$movements <- rbindlist(list(bases$movements,
                                          temp.dt),
                                     fill = T)
        
        saveRDS(bases$movements, './Data/inventory_fabrics.rds')
        
        # Sucess message
        success.message()
        
        # Clean inputs
        reset.inputs('adjustments',
                     c('date',
                       'fabric',
                       'operation.type',
                       'kg',
                       'detail'))
      } else {
        
        # Fail message
        fail.message('max.quant',
                     input$adjustments.fabric)
      }
    }
  })
  
  ## __ Samples ----
  output$outgoing.samples.date <- date.input('outgoing.samples.date')
  output$outgoing.samples.contractor <- contractor.input('outgoing.samples.contractor')
  
  # Process samples
  observeEvent(input$outgoing.samples.process, {
    
    # Check that we have at least 1 input
    if(length(inserted.samples) > 0) {
      
      # Get names of inputs
      input.names <- c()
      input.quant <- c()
      commercial.names <- c()
      
      for (i in 1:length(inserted.samples)) {
        input.names <- c(input.names, input[[paste0('fabric.id.',
                                                    inserted.samples[i])]])
        input.quant <- c(input.quant, input[[paste0('kg.id.',
                                                    inserted.samples[i])]])
        commercial.names <- c(commercial.names,
                              as.character(bases$fabrics[nombre_bautizado == input.names[i], nombre_comercial]))
      }
      
      # Check that there are no repeated inputs
      
      if (length(input.names) == length(unique(input.names))) {
        
        
        # Check if inputs are positive numbers
        check.pass <- positive.number(input.quant,
                                      input.names)
        
        if (check.pass) {
          
          # Check that the selected quantities do not exceed the inventory amounts
          
          prob.elements <- c()
          for (i in 1:length(input.names)) {
            if (!input.names[i] %in% bases$inv$nombre_bautizado) {
              prob.elements <- c(prob.elements, input.names[i])
            } else if (input.quant[i] > bases$inv[nombre_bautizado == input.names[i], cantidad]) {
              prob.elements <- c(prob.elements, input.names[i])
            }
          }
          
          
          if (length(prob.elements) == 0) {
            
            # Grab nextid
            next.id <- max(as.integer(c(0, sub('M_', '', bases$movements[grep('M_', id), id]))), na.rm = T) + 1
            
            # Create temporary tables for inputs and output
            temp.dt <- data.table(id = paste0('M_', next.id),
                                  nombre_bautizado = input.names,
                                  nombre_comercial = commercial.names,
                                  kg = input.quant,
                                  contraparte = input$outgoing.samples.provider,
                                  fecha = as.character(input$outgoing.samples.date),
                                  detalle = input$outgoing.samples.detail,
                                  operacion = 'Muestra')
            
            
            # Append temporary tables to original ones and store them
            bases$movements <- rbindlist(list(bases$movements, temp.dt), fill = T)
            
            saveRDS(bases$movements, './Data/inventory_fabrics.rds')
            
            # Clean inputs
            reset.inputs('outgoing.samples',
                         c('date',
                           'fabric',
                           'provider',
                           'garment'))
            
            # Remove items from inserted list
            for (item in inserted.samples) {
              removeUI(selector = paste0('#outgoingItemId', item))
            }
            
            # Clean inserted list
            inserted.samples <<- c()      
            # Sucess message
            success.message()
            
          } else {
            fail.message('max.quant', prob.elements)
          }
        }
        
      } else {
        fail.message('repeated.elements')
      }
      
    } else {
      fail.message('no.elements')
    }
  })
  
  # Check samples
  observeEvent(input$outgoing.samples.check, {
    
    # Get inputs
    input.names <- c()
    input.quant <- c()
    commercial.names <- c()
    
    for (i in 1:length(inserted.samples)) {
      input.names <- c(input.names, input[[paste0('fabric.id.',
                                                  inserted.samples[i])]])
      input.quant <- c(input.quant, input[[paste0('kg.id.',
                                                  inserted.samples[i])]])
      commercial.names <- c(commercial.names,
                            as.character(bases$fabrics[nombre_bautizado == input.names[i], nombre_comercial]))
    }
    
    # Create temporary tables for inputs and output
    temp.dt.input <- data.table(nombre_bautizado = input.names,
                                cantidad = input.quant)
    temp.dt.input <- rbindlist(list(temp.dt.input,
                                    data.table(nombre_bautizado = 'Total',
                                               cantidad = sum(temp.dt.input$cantidad, na.rm = T))))
    output$outgoing.samples.check <- renderTable(temp.dt.input)
    
  })
  
  ## __ Direct modifications ----
  ## _____ Incoming ----
  observeEvent(input$incoming.modify.process, {
    if (input$incoming.modify.password %in% c('clave')) {
      incoming.modify.hot.tbl <- isolate(input$incoming.modify.hot.tbl)
      if (!is.null(incoming.modify.hot.tbl)) {
        bases$movements <- hot_to_r(incoming.modify.hot.tbl)
        bases$movements[, costo_unitario := costo/kg]
        saveRDS(bases$movements, './Data/inventory_fabrics.rds')
      }
      success.message('operation')
      # Activate trigger
      triggers$pos <- triggers$pos + 1
    } else {
      fail.message('wrong.password')
    }
    updateTextInput(session,
                    'incoming.modify.password',
                    value = '')
    
  })
  
  output$incoming.modify.hot.tbl <- renderRHandsontable({
    DF <- bases$movements
    rhandsontable(DF)
  })
  
  ## _____ OP ----
  # Received garments
  observeEvent(input$po.received.modify.process, {
    if (input$po.received.modify.password %in% c('clave')) {
      po.received.modify.hot.tbl <- isolate(input$po.received.modify.hot.tbl)
      if (!is.null(po.received.modify.hot.tbl)) {
        bases$pos.outputs.flow <- hot_to_r(po.received.modify.hot.tbl)
        saveRDS(bases$pos.outputs.flow, './Data/pos_outputs_flow.rds')
      }
      success.message('operation')
      # Activate trigger
      triggers$pos <- triggers$pos + 1
    } else {
      fail.message('Clave incorrecta.')
    }
    updateTextInput(session,
                    'po.received.modify.password',
                    value = '')
    
  })
  
  observeEvent(bases$pos.inputs, {
    output$po.received.modify.hot.tbl <- renderRHandsontable({
      DF <- bases$pos.outputs.flow
      rhandsontable(DF)
    })
  })
  
  #### Update inventory table (and dependents) ----
  observeEvent(bases$movements, {
    
    # Update unitary cost in movements table
    bases$movements[!is.na(costo) & !is.na(kg), costo_unitario := round(costo/kg, 2)]
    bases$movements[!is.na(costo), costo := round(costo, 2)]
    
    # Update quantities in inventory table
    bases$inv <- copy(bases$movements)
    
    bases$inv <- merge(bases$inv[operacion %in% c('Ajuste-Suma',
                                                  'Compra'),
                                 .(added = sum(kg, na.rm = T)),
                                 by = nombre_bautizado],
                       bases$inv[operacion %in% c('Ajuste-Resta',
                                                  'Muestra',
                                                  'OP-Entrega'),
                                 .(substracted = sum(kg, na.rm = T)),
                                 by = nombre_bautizado],
                       by = 'nombre_bautizado',
                       all = T)
    
    bases$inv[is.na(added), added := 0]
    bases$inv[is.na(substracted), substracted := 0]
    bases$inv[, cantidad := round(added - substracted, 2)]
    bases$inv[, c('added', 'substracted') := NULL]
    
    # Grab type and subtype for each fabric
    bases$inv <- merge(bases$inv,
                       bases$fabrics,
                       by = 'nombre_bautizado',
                       all.x = T)
    
    # Update cost in inventory table
    tmp <- copy(bases$movements)
    tmp <- tmp[operacion == 'Compra',]
    setorder(tmp,
             fecha,
             nombre_bautizado)
    
    tmp <- tmp[, .SD[.N, costo_unitario], by = nombre_bautizado]
    
    bases$inv <- merge(bases$inv,
                       tmp,
                       by = 'nombre_bautizado',
                       all.x = T)
    bases$inv[, valor_estimado := round(V1 * cantidad, 2)]
    bases$inv[, V1 := NULL]
    bases$inv <- bases$inv[cantidad != 0, ] 
    
    ## Update pos.inputs table
    # Count up handed materials
    aa <- bases$movements[grepl('OP_', id),
                          .(po.hand.quant = sum(kg, na.rm = T),
                            fecha = min(fecha)),
                          by = .(nombre_bautizado, id)]
    aa[, id := as.numeric(sub('OP_', '', id))]
    
    bases$pos.inputs <- merge(bases$pos.inputs[,
                                               -'fecha',
                                               with = F],
                              aa,
                              by = c('nombre_bautizado', 'id'),
                              all.x = T)
    #bases$pos.inputs[is.na(fecha) & id == id]
    
    bases$pos.inputs[, entregado := NULL]
    setnames(bases$pos.inputs, 'po.hand.quant', 'entregado')

    # Link unitary cost to each handing of materials
    # PENDING: improve efficiency of this process
    for (i in 1:dim(bases$pos.inputs)[1]) {
      material <- as.character(bases$pos.inputs[i, nombre_bautizado])
      op.date <- as.Date(bases$pos.inputs[i, fecha])
      max.incoming.date <- max(bases$movements[grepl('Compra', operacion) &
                                           nombre_bautizado == material &
                                           as.Date(fecha) <= op.date, fecha], na.rm = T)
      unitary.cost <- max(bases$movements[grepl('Compra', operacion) &
                                      nombre_bautizado == material &
                                      as.Date(fecha) == max.incoming.date,
                                    costo_unitario],
                          na.rm = T)
      
      if (is.infinite(unitary.cost)) {
        unitary.cost <- NA
      }
      
      if (dim(pos.inputs)[1])  {
        bases$pos.inputs[i, costo := entregado * unitary.cost]
      }
      
    }
    
    # Save changes
    saveRDS(bases$pos.inputs, './Data/pos_inputs.rds')
    
    # Make table to show
    output$inv <- renderDataTable({
      datatable(bases$inv[, .(nombre_bautizado,
                              nombre_comercial,
                              tipo,
                              sub_tipo,
                              cantidad,
                              valor_estimado,
                              valor_estimado_unidad = round(valor_estimado / cantidad, 2))],
                filter = 'top',
                extensions = c('Buttons'),
                rownames = F,
                options = dt.options
                ) %>% 
        formatRound(6:7,2)
    },
    server = F,
    options = c(dt.options,
                list(columnDefs = list(list(targets = c(), visible = F))))
    )
    # Activate trigger
    triggers$pos <- triggers$pos + 1
  })
  
  
  #### Garment estimated costs ----
  observeEvent(triggers$pos, {
                   
                   # Sum up total cost
                   bases$po.costs <- merge(bases$pos.outputs,
                                           bases$pos.inputs[,
                                                            .(costo_total = round(sum(costo), 2),
                                                              contraparte = contraparte[1],
                                                              mano_obra = mano_obra[1]),
                                                            by = id_op],
                                           by = 'id_op',
                                           all = T)[, .(id_op,
                                                        fecha,
                                                        contraparte.y,
                                                        prenda,
                                                        costo_total,
                                                        estado,
                                                        corte,
                                                        cantidad_original,
                                                        recibido,
                                                        mano_obra)]
                   
                   
                   bases$po.costs[, cantidad_para_costo := NULL] # PENDING: remove
                   bases$po.costs[, cantidad_para_costo := NA_real_]
                   
                   # Determine what quantity to use
                   bases$po.costs[corte == 0 &
                                       estado == 'incompleto',
                                     cantidad_para_costo := cantidad_original]
                   bases$po.costs[corte > 0 &
                                       estado == 'incompleto',
                                     cantidad_para_costo := corte]
                   bases$po.costs[estado == 'completo',
                                     cantidad_para_costo := recibido]
                   
                   # Include workforce cost
                   bases$po.costs[, costo_total := round(costo_total + mano_obra * cantidad_para_costo, 2)]
                   #bases$po.costs[, mano_obra := NULL]
                   
                   # Determine unitary cost
                   bases$po.costs[, costo_unitario := round(costo_total / cantidad_para_costo, 2)]
                   
                   # Clean table
                   bases$po.costs <- bases$po.costs[!is.na(fecha), ] # PENDING: remove
                   bases$po.costs[, fecha := as.Date(fecha)]
                   setnames(bases$po.costs, 'contraparte.y', 'contraparte')
                   setorder(bases$po.costs, -fecha)
                   
                   # Save data
                   saveRDS(bases$po.costs, './Data/po_costs.rds')

                   # Output table
                   output$pocosts <- DT::renderDataTable(({
                     datatable(bases$po.costs[, .(id_op,
                                                  fecha,
                                                  contraparte,
                                                  prenda,
                                                  estado,
                                                  cantidad_original,
                                                  corte,
                                                  recibido,
                                                  cantidad_para_costo,
                                                  costo_total,
                                                  costo_unitario,
                                                  mano_obra)],
                               filter = 'top',
                               extensions = c('Buttons'),
                               rownames = F,
                               options = c(dt.options,
                                           list(columnDefs = list(list(targets = 5:7, visible = F)))) # changed because rownames = F messes the order up
                               ) %>% 
                       formatRound(10:11,2)
                   }),
                   server = F
                   ) 
                   # Activate trigger
                   triggers$pos2 <- triggers$pos2 + 1
                   
                 })
  
  #### Bases ----
  ## __ New fabrics ----
  output$new.fabric.type <- type.input('new.fabric.type')
  
  observeEvent(input$new.fabric.type, {
    output$new.fabric.sub.type <- sub.type.input('new.fabric.sub.type',
                                                 input$new.fabric.type)
  })
  
  
  # Process new fabric type
  observeEvent(input$new.type.fabric.process, {
    
    # Check that name is not repeated
    if (simple_cap(input$new.type.fabric) %in% sapply(bases$fabric.types, simple_cap)) {
      fail.message('repeated.elements')
    } else {
      
      # Update fabric type list
      bases$fabric.types <- c(bases$fabric.types, simple_cap(input$new.type.fabric))
      saveRDS(bases$fabric.types, './Data/fabric_types.rds')
      
      # Clear inputs
      shinyjs::reset('new.type.fabric')
      
      # Success message
      success.message('new.element')
    }
    
  })
  
  # Process new fabric sub type
  output$new.fabric.sub.type.type <- type.input('new.fabric.sub.type.type')
  
  observeEvent(input$new.sub.type.fabric.process, {
    
    # Check that name is not repeated
    if (simple_cap(input$new.sub.type.fabric) %in% sapply(bases$fabric.sub.types[tipo == input$new.fabric.sub.type.type,
                                                                                 sub_tipo],
                                                          simple_cap)) {
      fail.message('repeated.elements')
    } else {
      
      # Update fabric type list
      bases$fabric.sub.types <- rbindlist(list(bases$fabric.sub.types,
                                               data.table(tipo = input$new.fabric.sub.type.type,
                                                          sub_tipo = simple_cap(input$new.sub.type.fabric))))
      saveRDS(bases$fabric.sub.types, './Data/fabric_sub_types.rds')
      
      # Clear inputs
      shinyjs::reset('new.sub.type.fabric')
      
      # Success message
      success.message('new.element')
    }
    
  })
  
  # Process new fabric
  observeEvent(input$new.fabric.process, {
    
    # Check that names are not repeated
    
    check.pass <- repeated.names(list(list('nombre comercial',
                                           input$new.fabric.name1,
                                           bases$fabrics$nombre_comercial),
                                      list('nombre bautizado',
                                           input$new.fabric.name2,
                                           bases$fabrics$nombre_bautizado))
    )
    
    if (check.pass) {
      
      # Update fabric list
      temp.dt <- data.table(tipo = input$new.fabric.type,
                            sub_tipo = input$new.fabric.sub.type,
                            nombre_comercial = simple_cap(input$new.fabric.name1),
                            nombre_bautizado = simple_cap(input$new.fabric.name2))
      
      bases$fabrics <- rbindlist(list(bases$fabrics,
                                      temp.dt),
                                 fill = T)
      
      saveRDS(bases$fabrics, './Data/fabrics.rds')
      
      # Sucess message
      success.message()
      
      # Clean inputs
      reset.inputs('new.fabric',
                   c('type',
                     'sub.type',
                     'name1',
                     'name2'))
    }
    
  })
  
  ## __ Others ----
  add.base.button('provider')
  add.base.button('garment')
  add.base.button('contractor')
  
  #### Tables ----

  observeEvent(bases$movements, { # pending: is the observeEvent needed
    output$movements <- renderDataTable({
      setorder(bases$movements,
               -fecha,
               -id)
      # PENDING: remove date filter
      datatable(bases$movements[, .(id,
                                    id1,
                                    id2,
                                    operacion,
                                    fecha = as.Date(fecha),
                                    contraparte,
                                    tipo,
                                    sub_tipo,
                                    nombre_bautizado,
                                    nombre_comercial,
                                    kg,
                                    metros,
                                    costo,
                                    costo_unitario,
                                    detalle)],
                filter = 'top',
                extensions = c('Buttons'),
                rownames = F,
                options = c(dt.options,
                            list(columnDefs = list(list(targets = c(#1, # changed because rownames = F messes the order up
                                                                    2,
                                                                    6,
                                                                    7,
                                                                    9,
                                                                    11),
                                                        visible = F)))
                )) %>% 
        formatRound(11:14, 2)
      },
    server = F
    
      )
    
  })
  
  output$garments.flow <- renderDataTable({
    setorder(bases$pos.outputs.flow,
             -fecha,
             -id)
    bases$pos.outputs.flow[, .(id,
                               id1,
                               id2,
                               fecha,
                               prenda,
                               recibido)]
  },
  filter = 'top',
  extensions = c('Buttons'),
  server = F,
  rownames = F,
  options = c(dt.options,
              list(columnDefs = list(list(targets = 1:2, visible = F))))) # changed because rownames = F messes the order up
  
})
