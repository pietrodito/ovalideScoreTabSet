tableDesignerServer <- function(id, nature) {

  moduleServer(id, function(input, output, session) {
    ns <- NS(id)




    output$score <- shiny::renderUI({
      ovalide::load_score(nature)
      output$table <-
        DT::renderDT(ovalide::score(nature),
                     rownames = FALSE,
                     selection = list(mode = "single", target = "cell"),
                     options   = list(dom  = "t"     , pageLength = -1))
      DT::DTOutput(ns("table"))
    })

    r <- reactiveValues()

    r$table_name_in_config <- NULL

    observe({
      req(r$table_name_in_config)
      ovalideTableDesigner::tableDesignerServer("conf",
                                                r$table_name_in_config,
                                                nature)
    })

    observe({
      req(input$table_cells_selected)
      row <- input$table_cells_selected[1]
      r$etablissement <- ovalide::score(nature)[row, 1]
      r$finess <- ovalide::score(nature)[row, 2]
      r$column_nb <- input$table_cells_selected[2] + 1 #js index starts at 0...
      r$column_name <- names(ovalide::score(nature))[r$column_nb]
      r$cell_value <- ovalide::score(nature)[row, r$column_nb]
      shiny::updateTabsetPanel(session, "tabSet", selected = "Tableaux")
    })

    output$tabs <- shiny::renderUI({

      ovalide::load_ovalide_tables(nature)

      observeEvent(input$table_choosen, {
        r$associated_tables <- unique(
          c(r$associated_tables, input$table_select))
        shiny::removeModal()
      })

      output$table_list_text <- shiny::renderText({
        r$associated_tables
      })

      observeEvent(input$add_table, {
        shiny::showModal(shiny::modalDialog(
          shiny::selectInput(ns("table_select"),
                             "Choisissez une table...",
                             choices = names(ovalide::ovalide_tables(nature))),
          footer = tagList(
            shiny::modalButton("Annuler"),
            shiny::actionButton(ns("table_choosen"), "OK")
          )
        ))
      })

      add_config_event <- function(associated_table) {
        to_eval <- glue::glue('
          observeEvent(input$conf_<<associated_table>>, {
            r$table_name_in_config <- "<<associated_table>>"
          })', .open = "<<", .close = ">>")

        eval(parse(text = to_eval))
      }

      purrr::walk(r$associated_tables, add_config_event)

      list(
        shiny::wellPanel(
          shiny::h1(id = ns("etab_label"), r$etablissement),
          shiny::h2(id = ns("finess_label"), r$finess),
          shiny::h3(id = ns("column_label"), r$column_name),
          shiny::h3(id = ns("value_label"), r$cell_value)
        ),
        shiny::wellPanel(
          shiny::textOutput(ns("table_list_text")),

          (
            r$associated_tables
            |> purrr::map(\(x)  ovalide::ovalide_tables(ovalide::nature())[[x]])
            |> purrr::map(\(t)  DT::renderDT(t))
            |> purrr::map2(r$associated_tables, \(dt, tab_name)
                           shiny::wellPanel(
                             dt,
                             shiny::actionButton(ns(paste0("conf_", tab_name)),
                                                 "Config"),
                             shiny::actionButton(ns(paste0("rm_", tab_name)),
                                                 "Supprime table")))
          ),
          shiny::actionButton(ns("add_table"), "Ajouter une table")
        ))
    })
  })
}


