#' A Team Load Function
#'
#' This function allows you to download team data from Hokej.cz
#' @param pageURL Hokej.cz page url with team stats table.
#' @param removeRow Removes top row from the source table if it contains two headers. Default to TRUE.
#' @keywords player
#' @export
#' @examples
#' pageUrl <- "http://hokej.cz/tipsport-extraliga/stats-center/team-analytic-stats?season=2017&competition=6026&stats-section=corsi"
#' teamLoad(pageUrl,removeRow = F)

teamLoad <- function(pageUrl,removeRow=TRUE,node=c(".table-stats",".table-soupiska")) {
    html <- xml2::read_html(pageUrl)
    cast <- rvest::html_nodes(html, node)
    df <- rvest::html_table(cast,fill = T)
    x <- df[[1]]
    if(removeRow) {
        colnames(x) <- x[1,]
        x <- x[-1,]
        x[,-c(1:2)] <- apply(x[,-c(1:2)],2,as.numeric)
    } else (x)
    #clean variables
    colnames(x) <- iconv(colnames(x), to='ASCII//TRANSLIT')
    colnames(x) <- gsub("%","pr",colnames(x))
    colnames(x) <- gsub("[/]","per",colnames(x))
    colnames(x) <- gsub("[#]","Rank",colnames(x))
    colnames(x) <- gsub("[.]","_",colnames(x))
    colnames(x) <- gsub(" ","_",colnames(x))
    x <- x[,-1]
    colnames(x)[1] <- "Team"
    #add variables
#    x$downloadData <- rep(Sys.Date(),nrow(x))
    x$seasonStart <- getSeason(pageUrl)
    compId <- getCompetition(pageUrl)
    data(tableKey)
    x$competitionType <- tableKey[which(tableKey$Competition == compId),3]
    x
}
