#' A Team Load Function
#'
#' This function allows you to download team data from Hokej.cz
#' @param pageURL Hokej.cz page url with team stats table.
#' @param removeRow Removes top row from the source table if it contains two headers. Default to TRUE.
#' @keywords player
#' @export
#' @examples
#' pageUrl <- "http://hokej.cz/tipsport-extraliga/stats-center/team-analytic-stats?season=2017&competition=6026&stats-section=corsi"
#' corsiTeam <- teamLoad(pageUrl,removeRow = F)

teamLoad <- function(pageUrl,removeRow=TRUE) {
    html <- xml2::read_html(pageUrl)
    cast <- rvest::html_nodes(html, ".table-stats")
    df <- rvest::html_table(cast,fill = T)
    x <- df[[1]]
    if(removeRow) {
        colnames(x) <- x[1,]
        x <- x[-1,]
        x[,-c(1:2)] <- apply(x[,-c(1:2)],2,as.numeric)
    } else (x)
    x$downloadData <- rep(Sys.Date(),nrow(x))
    x
}
