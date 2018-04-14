#' A Get Competition Type Function
#'
#' This function returns the table competition id from the Hokej.cz url
#' @param pageURL Hokej.cz page url with team/player stats table.
#' @keywords team,player
#' @export
#' @examples
#' pageUrl <- "http://hokej.cz/tipsport-extraliga/stats-center/team-analytic-stats?season=2017&competition=6026&stats-section=corsi"
#' getCompetition(pageUrl)

getCompetition <- function(pageUrl) {
    x <- urltools::url_parse(pageUrl)
    y <- unlist(strsplit(x$parameter,"&"))
    unlist(strsplit(y[grep("competition",y)],"="))[2]
}
