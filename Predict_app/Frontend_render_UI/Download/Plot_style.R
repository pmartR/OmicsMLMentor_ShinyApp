## Plot edits

output$edit_plot <- renderUI({
  req(download_preview$plot)
  isolate(plot_style <- plot_table_current$plot_options[[download_preview$name]])
  
  collapseBoxGroup(
    id = "download_plot_options_UI",
    collapseBox(
      "Plot Options",
      value = "axes_options",
      tagList(
        splitLayout(textInput(paste0("download_title"), "Title", value = plot_style$title$text, placeholder = "(default)"),
                    numericInput(paste0("download_title_fontsize"), "Title font size", value = 14),
                    sliderInput(paste0("download_title_hjust"), "Title alignment", ticks = FALSE,
                                value = 0, min = 0, max = 1, step = 0.5),
                    numericInput(paste0("download_legend_fontsize"), "Legend font size", value = 14),
                    cellWidths = rep("25%", 4)
        ),
        splitLayout(textInput(paste0("download_xlab"), "X-axis label", value = plot_style$xaxis$title$text, placeholder = "(default)"),
                    textInput(paste0("download_ylab"), "Y-axis label", value = plot_style$yaxis$title$text, placeholder = "(default)"),
                    numericInput(paste0("download_x_fontsize"), "X-axis font size", value = 11),
                    numericInput(paste0("download_y_fontsize"), "Y-axis font size", value = 11),
                    cellWidths = rep("25%", 4)
        ),
        splitLayout(numericInput(paste0("download_xangle"), "X-axis tick angle", value = NULL),
                    numericInput(paste0("download_yangle"), "Y-axis tick angle", value = NULL),
                    numericInput(paste0("download_x_ticksize"), "X-axis tick size", value = NULL),
                    numericInput(paste0("download_y_ticksize"), "Y-axis tick size", value = NULL),
                    cellWidths = rep("25%", 4)
        ),
        ### TODO: Fix the color scaling (issue #84)
        ### - ECG 5/16/2024
        # splitLayout(
        #   prettySwitch("download_use_color_scale", "Use color scaling", value = F, fill = T, status = "primary"),
        #   conditionalPanel(
        #     condition = "input.download_use_color_scale",
        #     prettySwitch("download_color_scale_palette_flip", "Flip gradient direction", value = F, fill = T, status = "primary"),
        #   ),
        #   conditionalPanel(
        #     condition = "input.download_use_color_scale",
        #     tags$b("Palette"),
        #     pickerInput("download_color_scale_palette",
        #                 choices = row.names(brewer.pal.info),
        #                 selected = "Blues")
        #   ),
        #   cellWidths = c("30%", "30%", "40%")
        # ),
        splitLayout(
          div(
            tags$b("Export format"),
            pickerInput(paste0("download_export_format"),
                        choices = c("Loading..."),
                        label = NULL, selected = "png")
          ),
          div(
            br(),
            prettySwitch("download_plot_interactive", "Interactive", value = F, fill = T, status = "primary")
          )
        )
      ),
      
      div(appButton(inputId = "download_apply_style_plot", label = "Update plot"),
          appButton(inputId = "download_reset_style_plot", label = "Reset plot"))
    )
  )
})

observeEvent(c(input$download_plot_interactive, download_preview$current), {
  req(download_preview$plot)
  req(!is.null(download_preview$current))
  req(!is.null(input$download_plot_interactive))
  
  if (inherits(download_preview$current, "plotly")) {
    isolate(updatePrettySwitch(session, "download_plot_interactive", value = T))
    updatePickerInput(session, "download_export_format", choices = c("HTML (interactive)" = "html"), selected = "html")
    shinyjs::disable("download_plot_interactive")
    shinyjs::disable("download_export_format")
    return()
  }
  
  if (input$download_plot_interactive > 0) {
    updatePickerInput(session, "download_export_format", choices = c("HTML (interactive)" = "html"), selected = "html")
    shinyjs::disable("download_export_format")
  } else {
    updatePickerInput(session, "download_export_format", choices = c("PNG" = "png", "JPEG" = "jpg", "SVG" = "svg"))
    shinyjs::enable("download_export_format")
  }
})

observeEvent(input$download_apply_style_plot, {
  req(!is.null(download_preview$current))
  req(!is.null(download_preview$name))
  req(download_preview$plot)
  
  plot_style <- plot_table_current$plot_options[[download_preview$name]]
  
  if (is.null(plot_style)) {
    adjustments <- list(
      xaxis = list(
        title = list(
          font = list()
        ),
        tickfont = list()
      ),
      yaxis = list(
        title = list(
          font = list()
        ),
        tickfont = list()
      ),
      title = list(
        font = list()
      )
    )
  } else {
    adjustments <- plot_style
  }
  
  if (!is.null(input$download_xlab) && input$download_xlab != "") {
    adjustments$xaxis$title$text <- input$download_xlab
  } else {
    adjustments$xaxis$title$text <- NULL
  }
  
  if (!is.null(input$download_ylab) && input$download_ylab != "") {
    adjustments$yaxis$title$text <- input$download_ylab
  } else {
    adjustments$yaxis$title$text <- NULL
  }
  
  if (!is.na(input$download_x_fontsize)) {
    adjustments$xaxis$title$font$size <- input$download_x_fontsize
  } else {
    adjustments$xaxis$title$font$size <- NULL
  }
  
  if (!is.na(input$download_y_fontsize)) {
    adjustments$yaxis$title$font$size <- input$download_y_fontsize
  } else {
    adjustments$yaxis$title$font$size <- NULL
  }
  
  if (!is.na(input$download_xangle)) {
    adjustments$xaxis$tickangle <- input$download_xangle
  } else {
    adjustments$xaxis$tickangle <- NULL
  }
  
  if (!is.na(input$download_yangle)) {
    adjustments$yaxis$tickangle <- input$download_yangle
  } else {
    adjustments$yaxis$tickangle <- NULL
  }
  
  if (!is.na(input$download_x_ticksize)) {
    adjustments$xaxis$tickfont$size <- input$download_x_ticksize
  } else {
    adjustments$xaxis$tickfont$size <- NULL
  }
  
  if (!is.na(input$download_y_ticksize)) {
    adjustments$yaxis$tickfont$size <- input$download_y_ticksize
  } else {
    adjustments$yaxis$tickfont$size <- NULL
  }
  
  if (input$download_title != "") {
    adjustments$title$text <- input$download_title
  } else {
    adjustments$title$text <- NULL
  }
  
  if (!is.na(input$download_title_fontsize)) {
    adjustments$title$font$size <- input$download_title_fontsize
  } else {
    adjustments$title$font$size <- NULL
  }
  
  if (!is.na(input$download_title_hjust)) {
    adjustments$title$x <- input$download_title_hjust
  } else {
    adjustments$title$x <- NULL
  }
  
  if (!is.na(input$download_legend_fontsize)) {
    adjustments$legend$font$size <- input$download_legend_fontsize
  } else {
    adjustments$legend$font$size <- NULL
  }
  
  if (!is.null(input$download_color_scale_palette) && input$download_use_color_scale) {
    adjustments$colorway <- brewer.pal(
      nrow(layer_data(download_preview$current, 1)),
      input$download_color_scale_palette
    )
    if (input$download_color_scale_palette_flip) {
      adjustments$colorway <- rev(adjustments$colorway)
    }
  } else {
    adjustments$colorway <- NULL
  }
  
  adjustments$meta <- c(input$download_export_format)
  
  plot_table_current$plot_options[[download_preview$name]] <- adjustments
})

observeEvent(input$download_reset_style_plot, {
  req(input$download_reset_style_plot > 0)
  req(download_preview$plot)
  plot_table_current$plot_options[[download_preview$name]] <- NULL
})

apply_plot_style <- function(p, plot_style) {
  if (is.null(plot_style))
    return(p)
  
  if (inherits(p, "plotly")) {
    args <- plot_style
    args$p <- p
    return(do.call(plotly::layout, args))
  }
  
  if (is.null(plot_style$colorway)) {
    scale <- theme()
  } else if (inherits(layer_scales(p), "ScaleContinuous")) {
    scale <- scale_fill_gradientn(colors = plot_style$colorway)
  } else {
    scale <- scale_fill_manual(values = plot_style$colorway)
  }
  
  title <- if (is.null(plot_style$title$text))
              ggplot2::waiver() 
            else
              plot_style$title$text
  xtitle <- if (is.null(plot_style$xaxis$title$text))
              ggplot2::waiver() 
            else
              plot_style$xaxis$title$text
  ytitle <- if (is.null(plot_style$yaxis$title$text))
              ggplot2::waiver() 
            else
              plot_style$yaxis$title$text
  
  p + 
    ggtitle(title) +
    xlab(xtitle) +
    ylab(ytitle) +
    scale +
    theme(
      plot.title = element_text(
        size = plot_style$title$font$size,
        hjust = plot_style$title$x
      ),
      axis.title.x = element_text(
        size = plot_style$xaxis$title$font$size
      ),
      axis.title.y = element_text(
        size = plot_style$yaxis$title$font$size
      ),
      axis.text.x = element_text(
        angle = plot_style$xaxis$tickangle,
        size = plot_style$xaxis$tickfont$size
      ),
      axis.text.y = element_text(
        angle = plot_style$yaxis$tickangle,
        size = plot_style$yaxis$tickfont$size
      ),
      legend.text = element_text(
        size = plot_style$legend$font$size
      ),
      legend.title = element_text(
        size = plot_style$legend$font$size
      )
    )
}

