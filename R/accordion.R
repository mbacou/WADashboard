#' Bootstrap 4 accordion
#'
#' This is a modified version of [bs4Dash::accordion()].
#'
#' @param ... slot for \link{accordionItem}.
#' @param id unique accordion id.
#' @param width the width of the accordion.
#'
#' @rdname accordion
#' @export
accordion <- function(..., id, width = 12) {

  items = list(...)

  # patch that enables a proper accordion behavior
  # we add the data-parent non standard attribute to each
  # item. Each accordion must have a unique id.
  items = lapply(seq_along(items), function(i) {
    items[[i]]$children[[2]]$attribs[["data-parent"]] = paste0("#", id)
    items[[i]]$children[[1]]$children[[1]]$children[[1]]$attribs$`data-target` =
      paste("#collapse", id, i, sep="_")
    items[[i]]$children[[2]]$attribs[["id"]] = paste("collapse", id, i, sep="_")
    return(items[[i]])
  })

  div(class="accordion", id=id, items)
}


#' Bootstrap 4 accordion item
#'
#' [accordionItem] to be inserted in a [accordion] container. This is a modified
#' version of [bs4Dash::accordionItem()].
#'
#' @inheritParams bs4Dash::bs4AccordionItem
#' @param icon optional icon
#' @param class additional classes to apply to the parent card element
#' @param bg background color
#'
#' @rdname accordion
#' @importFrom shiny a div icon
#' @export
accordionItem <- function(...,
  title=NULL, icon=NULL, status=NULL,
  collapsed=TRUE, solidHeader=TRUE, class=NULL, bg=NULL) {

  cl = "card"
  if(!is.null(status)) cl = paste(cl, status, sep="-")
  if(!solidHeader) cl = paste(cl, "card-outline", sep=" ")
  if(!is.null(class)) cl = paste(cl, class, sep=" ")
  cl_body = "card-body p-0 text-muted collapse"
  if(!collapsed) cl_body = paste(cl_body, "show", sep=" ")
  if(!is.null(bg)) cl_body = paste(cl_body, bg, sep=" bg-")

  div(class=cl,
    # box header
    div(class="card-header bg-gray px-3", span(
      a(class=if(collapsed) "collapsed" else "",
        href="#", `data-toggle`="collapse",
        `aria-expanded`=if(collapsed) "false" else "true",
        span(class="text-muted mr-2", icon), title))
    ),
    div(class=cl_body, ...)
  )
}


#' Update accordion
#'
#' Change the selected value of an accordion input on the client. Copied from
#' [bs4Dash::updateAccordion()].
#'
#' @inheritParams shiny::updateSelectInput
#'
#' @rdname accordion
#' @importFrom shiny getDefaultReactiveDomain
#' @export
#'
#' @examples
updateAccordion <- function(id, selected, session = getDefaultReactiveDomain()) {
  session$sendInputMessage(id, selected)
}
