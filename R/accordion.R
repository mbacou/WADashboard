#' Bootstrap 4 accordion container
#'
#' [accordionItem] is to be inserted in a [accordion]. This is a modified
#' version of [bs4Dash::accordion()].
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
#' \link{accordionItem} is to be inserted in a \link{accordion}. This is a modified
#' version of [bs4Dash::accordionItem()].
#'
#' @inheritParams bs4Dash::bs4AccordionItem
#' @param class additional classes to apply to the parent card element
#' @partam bg background color
#'
#' @rdname accordion
#' @importFrom shiny div
#' @export
accordionItem <- function(..., title,
  status=NULL, collapsed=TRUE, solidHeader=TRUE, class=NULL, bg=NULL) {

  cl = "card"
  if(!is.null(status)) cl = paste(cl, status, sep="-")
  if(!solidHeader) cl = paste(cl, "card-outline", sep=" ")
  if(!is.null(class)) cl = paste(cl, class, sep=" ")
  cl_body = "card-body p-0 text-muted collapse"
  if(!collapsed) cl_body = paste(cl_body, "show", sep=" ")
  if(!is.null(bg)) cl_body = paste(cl_body, bg, sep=" bg-")

  div(class=cl,
    # box header
    div(class="card-header waved3 px-3 pt-1 pb-2",
      div(class="w-100",
        a(class="d-block w-100", href="#",
          `data-toggle`="collapse", `aria-expanded`=if(collapsed) "false" else "true",
          class=if(collapsed) "collapsed",
          title)
      )
    ),
    div(class=cl_body, ...)
  )
}


#' Update accordion
#'
#' Alias for [bs4Dash::updateAccordion()]
#'
#' @inheritParams bs4Dash::updateAccordion
#'
#' @rdname accordion
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom bs4Dash updateAccordion
#' @export
#'
#' @examples
updateAccordion <- function(id, selected, session=getDefaultReactiveDomain())
  updateAccordion(id=id, selected=selected, session=session)
