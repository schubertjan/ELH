#' A Player Load Function
#'
#' This function allows you to download player data from Hokej.cz
#' @param pageURL Hokej.cz page url with player stats table.
#' @param removeRow Removes top row from the source table if it contains two headers. Default to TRUE.
#' @keywords player
#' @export
#' @examples
#' pageUrl <- "http://hokej.cz/tipsport-extraliga/stats-center/player-analytic?season=2017&competition=6026&yearFrom=&stranger=0&state=&stats-section=shots&stats-all=1"
#' shots <- playerLoad(pageUrl)


playerLoad <- function(pageUrl,removeRow=TRUE) {
    html <- xml2::read_html(pageUrl)
    cast <- rvest::html_nodes(html, ".table-stats")
    df <- rvest::html_table(cast,fill = T)
    x <- df[[1]]
    if(removeRow) {
        colnames(x) <- x[1,]
        x <- x[-1,]
        x[,-c(1:6)] <- apply(x[,-c(1:6)],2,as.numeric)
        x$GP <- as.numeric(x$GP)
    } else (x)
    x$TOI <- lubridate::ms(x$TOI)
    x$minutes <- lubridate::minute(x$TOI)+(lubridate::second(x$TOI)/60)
    x$downloadData <- rep(Sys.Date(),nrow(x))
    x
}
