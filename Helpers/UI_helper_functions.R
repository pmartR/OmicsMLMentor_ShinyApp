#'@description Convenience function to assign a class to a tab in a shiny 
#'navbar.
#'
#'@param name The name of the navbar tab, given by the \code{value} argument in
#'\code{tabPanel}
#'@param class CSS class to apply to the tab.  Defaults to 'disabled'.
#'@param condition Logical indicating whether to enable or disable the tab.
#'TRUE = enabled, FALSE = disabled
toggleTab <- function(name, condition, class="disabled") {
  if(condition) {
    js$enableTab(name, class)
  } else js$disableTab(name, class)
}

#'@details Creates a sequence of inputs as html, usually to be added the column
#'of a table.
buttonInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

collapseBoxGroup <- function(..., id = NULL) {
  if (is.null(id)) {
    id <- sprintf("collapse-group-%07i", as.integer(runif(1,0,9999999)))
  }
  div(id = id, class = "collapse-box-group", ...)
}

collapseBox <- function(titletext, value, ..., collapsed = TRUE,
                        icon = NULL, icon_id = NULL, icon_style = NULL, 
                        icon_hidden = TRUE, icon_tooltip_text = NULL) {
  div(
    id = value,
    class = "collapse-box",
    box(
      title = {
        if (!is.null(icon_tooltip_text)) {
          icon <- add_prompt(icon, message = icon_tooltip_text)
        }
        div(
          div(
            titletext,
            `data-panel` = value,
            class = "collapse-title",
            onclick = "updateBoxCollapse(this)"
          ),
          if (is.null(value)) {
            ""
          } else {
            icon_div <- div(
              id = icon_id,
              style = if (is.null(icon_style)) "color:red;float:right;position:absolute;right:10px;top:10px;" else icon_style,
              icon
            )
            if (icon_hidden) hidden(icon_div) else icon_div
          }
        )
      },
      width = 12,
      collapsible = TRUE,
      collapsed = collapsed,
      `data-panel` = value,
      ...
    )
  )
}

updateBoxCollapse <- function(session, id, open = NULL, close = NULL, toggle = NULL) {
  id <- if (id != "") paste0("#", id) else "*"
  
  if (is.logical(open) || is.logical(close)) {
    runjs(sprintf("let e=$('%s').find('.box-body[value=\"%s\"]');if (e.css('overflow')=='hidden') {e.parent().toggleClass('collapsed-box');}", id, e))
    runjs(sprintf("let e=$('%s').find('.box-body[value=\"%s\"]');if (%se.parent().hasClass('collapsed-box')) {e.parent().toggleBox();}", id, e, isTRUE(open) ? "" : "!"))
  } else {
    # Force animation to finish
    for (e in c(open, close, toggle)) {
      runjs(sprintf("let e=$('%s').find('.box-body[data-panel=\"%s\"]');if (e.css('overflow')=='hidden') {e.parent().toggleClass('collapsed-box');}", id, e))
    }
    
    for (e in open) {
      runjs(sprintf("let e=$('%s').find('.box-body[data-panel=\"%s\"]');if (e.parent().hasClass('collapsed-box')) {e.parent().toggleBox();}", id, e))
    }
    
    for (e in close) {
      runjs(sprintf("let e=$('%s').find('.box-body[data-panel=\"%s\"]');if (!e.parent().hasClass('collapsed-box')) {e.parent().toggleBox();}", id, e))
    }
    
    for (e in toggle) {
      runjs(sprintf("$('%s').find('.box-body[data-panel=\"%s\"]').parent().toggleBox();", id, e))
    }
  }
}

fluidSplitLayout <- function(col1_content, col2_content, width1 = 6, width2 = 6) {
  fluidRow(
    column(
      width1,
      col1_content
    ),
    column(
      width2,
      col2_content
    )
  )
}

toggleTooltip <- function(session, id, condition, tooltip_text, selector = NULL, position="bottom") {
  if (!is.null(selector)) {
    jquery = selector
  } else {
    jquery = paste0("#", id)
  }
  if (condition) {
    addPrompter(session, id, tooltip_text, placement = position)
  }
  else {
    # removes all classes starting with "hint--" from selected element
    shinyjs::runjs(
      sprintf("$('%s').attr('class', function(index, className) {if (className !== undefined) return className.replace(/(^|\\s)hint--\\S+/g,'')})", jquery)
    )
  }
}


# show an element and add a tooltip if condition is met
show_add_tooltip <- function(session, id, condition, tooltip_text,
                             selector=NULL, position="bottom"){
  toggleElement(id = id, condition = condition, selector = selector)
  toggleTooltip(session, id, condition, tooltip_text, selector = selector, position= position)
}

# disable/enable sub-elements of a div and display a tooltip based on condition.
### NOTE:  THIS CANNOT ADD A TOOLTIP TO THE SAME ELEMENT IT DISABLES
### DISABLED ELEMENTS DO NOT LIKE HAVING TOOLTIPS ADDED TO THEM FOR SOME REASON.
togglestate_add_tooltip <- function(session, id, condition, tooltip_text, 
                                    selector=NULL, position="bottom") {
  toggleState(id = id, condition = condition, selector = selector)
  toggleTooltip(session, id, !condition, tooltip_text, selector = selector, position= position)
}

# a version of req that returns a value instead of 'cancelling execution'
reqNull <- function(..., returnvalue = NULL) {
  args <- list(...)
  for (el in args) {
    if (!isTruthy(el)) {
      shiny:::reactiveStop(class = "validation")
      return(returnvalue)
    }
  }
  invisible()
}

# subsection_header <- function(titletext, id, icon, style = '', hide_icon = T, icon_pos = 'right'){
#   icondiv = div(id = id, style = style, icon)
#   icondiv = ifelse(hide_icon, hidden(icondiv), icondiv)
#
#   if(icon_pos == 'right'){
#     item1 = titletext
#     item2 = icondiv
#   }
#   else if(icon_pos == 'left'){
#     item2 = titletext
#     item1 = icondiv
#   }
#
#   div(item1, item2)
# }


style_UI <- function(pagename) {
  tagList(
    splitLayout(textInput(paste0(pagename, "_xlab"), "X-axis label"),
      textInput(paste0(pagename, "_ylab"), "Y-axis label"),
      numericInput(paste0(pagename, "_x_fontsize"), "X-axis font size", value = 11),
      numericInput(paste0(pagename, "_y_fontsize"), "Y-axis font size", value = 11),
      cellWidths = rep("25%", 4)
    ),
    splitLayout(numericInput(paste0(pagename, "_xangle"), "X-axis tick angle", value = NULL),
      numericInput(paste0(pagename, "_yangle"), "Y-axis tick angle", value = NULL),
      numericInput(paste0(pagename, "_x_ticksize"), "X-axis tick size", value = NULL),
      numericInput(paste0(pagename, "_y_ticksize"), "Y-axis tick size", value = NULL),
      cellWidths = rep("25%", 4)
    ),
    splitLayout(textInput(paste0(pagename, "_title"), "Title"),
      numericInput(paste0(pagename, "_title_fontsize"), "Title font size", value = 14),
      cellWidths = c("25%", "25%")
    )
  )
}

# Create a popup that requires input to close
bsModalNoClose <- function(...) {
  b <- bsModal(...)
  b[[2]]$`data-backdrop` <- "static"
  b[[2]]$`data-keyboard` <- "false"
  return(b)
}


# return the input value for a particular set of inputs that are replicated across all datatypes
get_inputs <- function(session, names, prefix = "", postfix = "") {
  sapply(names, function(name) {
    get("input", envir = session)[[sprintf("%s%s%s", prefix, name, postfix)]]
  })
}

# create a replacement for the addTooltip feature
# note: if the prompter package gets updated, this may break
addPrompter <- function(session, id, title, placement = "bottom", type = NULL, size = NULL, rounded = TRUE) {
  attributes <- paste0(" hint--", placement)
  
  if (!is.null(type)) {
    attributes <- paste0(attributes, " hint--", type)
  }
  
  if (!is.null(size)) {
    attributes <- paste0(attributes, " hint--", size)
  }
  
  if (rounded) {
    attributes <- paste0(attributes, " hint--", "rounded")
  }
  
  session$sendCustomMessage(type = "addPrompter", list(target = id, attributes = attributes, label = title))
}

# create a generator function so that we don't have the same HTML repeated in 50 different places
generate_warning_tooltip <- function(id, color = "red", icon = "exclamation-sign") {
  return(sprintf("<div style='display: inline; margin-right:3px;'><span id = '%s', class='glyphicon glyphicon-%s', style='color:%s;'></span></div>", id, icon, color))
}


LVmol_filt <- function(omicsData){
  
  df <- omicsData$e_data
  rm_col <- which(colnames(df) %in% pmartR::get_edata_cname(omicsData))
  
  df <- df[-rm_col]
  
  density(apply(df, 1, var, na.rm = T))
}


LVSam_filt <- function(omicsData){
  
  df <- omicsData$e_data
  rm_col <- which(colnames(df) %in% pmartR::get_edata_cname(omicsData))
  
  df <- df[-rm_col]
  
  density(apply(df, 2, var, na.rm = T))
}

## ggdendro help
# https://rstudio-pubs-static.s3.amazonaws.com/758901_c8bdf7cf647d4c1795045d071a2a4941.html
# https://rpubs.com/TX-YXL/662586

dendro_data_k <- function(hc, k = NULL, h = NULL, custom_color = NULL) {
  
  hcdata    <-  ggdendro::dendro_data(hc, type = "rectangle")
  seg       <-  hcdata$segments
  
  if(!is.null(k)){
    labclust  <-  cutree(hc, k = k)[hc$order]
  } else if(!is.null(h)){
    labclust  <-  cutree(hc, h = h)[hc$order]
  }
  
  if(!is.null(custom_color)){
    labclust <- custom_color[names(labclust)]
  }
  
  k <- max(as.numeric(labclust))
  
  segclust  <-  rep(0L, nrow(seg))
  heights   <-  sort(hc$height, decreasing = TRUE)
  height    <-  mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
  
  for (i in 1:k) {
    xi      <-  hcdata$labels$x[labclust == i]
    consecutive <- split(xi, cumsum(c(1, diff(xi) != 1)))
    for(cons in consecutive){
      idx1    <-  seg$x    >= min(cons) & seg$x    <= max(cons)
      idx2    <-  seg$xend >= min(cons) & seg$xend <= max(cons)
      idx3    <-  seg$yend < height
      idx     <-  idx1 & idx2 & idx3
      segclust[idx] <- i
    }
  }
  
  idx                    <-  which(segclust == 0L)
  segclust[idx]          <-  segclust[idx + 1L]
  hcdata$segments$clust  <-  segclust
  hcdata$segments$line   <-  as.integer(segclust < 1L)
  hcdata$labels$clust    <-  labclust
  
  hcdata
}
###################################cluster
set_labels_params <- function(nbLabels) {

    angle       <-  rep(0, nbLabels) + 90
    hjust       <-  1
    
  list(angle = angle, hjust = hjust, vjust = 0.5)
}

plot_ggdendro_multi <- function(hcdata,
                                branch.size = 1,
                                label.size  = 3,
                                nudge.label = 0.01,
                                expand.y    = 0.1) {

  ybreaks   <- pretty(segment(hcdata)$y, n = 5)
  ymax      <- max(segment(hcdata)$y)
  
  ## branches
  p <- ggplot() +
    geom_segment(data         =  segment(hcdata),
                 aes(x        =  x,
                     y        =  y,
                     xend     =  xend,
                     yend     =  yend,
                     linetype =  "solid",
                     colour   =  factor(clust)),
                 lineend      =  "round",
                 show.legend  =  FALSE,
                 size         =  branch.size)
  
  ## orientation
    p <- p + scale_x_continuous(breaks = NULL)
      p <- p + scale_y_continuous(breaks = ybreaks)
      nudge.label <- -(nudge.label)

  
  # labels
  labelParams <- set_labels_params(nrow(hcdata$labels))
  hcdata$labels$angle <- labelParams$angle
  
  
  
  p <- p +
    geom_text(data        =  label(hcdata),
              aes(x       =  x,
                  y       =  y,
                  label   =  label,
                  angle   =  angle,
              colour  =  factor(clust)),
              vjust       =  labelParams$vjust,
              hjust       =  labelParams$hjust,
              nudge_y     =  ymax * nudge.label,
              size        =  label.size,
              show.legend =  FALSE)
  
  # colors and limits
  
  ylim <- -round(ymax * expand.y, 1)
  p    <- p + expand_limits(y = ylim)
  
  p + labs(x = "Samples", y = "Height") + theme_bw()
}

