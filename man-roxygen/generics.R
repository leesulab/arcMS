#' @details \code{<%=func%>} <%=desc%>
#' <% cl <- patRoon:::getAllMethods(func) %>
#' \itemize{
#'     \item Methods are defined for: <%= { paste0(sprintf("\\code{\\link[=%s,%s-method]{%s}}", func, cl, cl), collapse = "; ") } %>.
#' }
#' @rdname generics
#' @aliases <%=func%>
