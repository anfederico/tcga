library(httr)
library(jsonlite)

query_geneshot <- function(terms, type=c("generif", "autorif")) {
    type <- match.arg(type)

    if (type == "generif") {
        url.b <- "https://amp.pharm.mssm.edu/geneshot/api/search/"
    }
    if (type == "autorif") {
        url.b <- "https://amp.pharm.mssm.edu/geneshot/api/search/auto/"
    }
    
    first <- TRUE
    for (i in terms) {
        i <- gsub(" ", "%20", i)
        if (first){
            first <- FALSE
        } else {
            url.b <- paste(url.b, "%20AND%20", sep="")
        }
        url.b <- paste(url.b, i, sep="")
    }
    
    r <- GET(url.b)
    json <- content(r, "text", encoding="ISO-8859-1")
    data <- fromJSON(json)
    
    df <- data.frame(names(data$gene_count),
                     as.numeric(lapply(data$gene_count, function(x) x[1])),
                     as.numeric(lapply(data$gene_count, function(x) x[2])))
    
    colnames(df) <- c('Gene', "Publications", "Frequency")
    
    return(df)
}

# Example
df <- query_geneshot(terms=c("breast cancer", "mammary tissue"))
                                       
