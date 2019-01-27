library(httr)
library(biomaRt)

load_biomaRt <- function() {
    ensembl <- useEnsembl(biomart="ensembl", dataset="hsapiens_gene_ensembl")
    return(ensembl)
}

get_ensembl_id <- function(ensembl, symbol) {
    out <- getBM(attributes=c('ensembl_gene_id_version'), filters='external_gene_name', values=c(symbol), mart=ensembl)
    id <- as.character(out$ensembl_gene_id_version)
    return(id)
}

get_regulators <- function(ensembl, gene_symbol, upstream=1, downstream=1) {
    # Get gene id with ensembl version
    ref_gene_id <- get_ensembl_id(ensembl, gene_symbol)
    
    # Build url
    url.b <- "http://rna.sysu.edu.cn/chipbase/download.php?base_page=regulator&organism=&assembly=hg38"
    url.1 <- paste("&ref_gene_id=", ref_gene_id, sep="")
    url.2 <- paste("&gene_symbol=", gene_symbol, sep="")
    url.3 <- "&protein=0&regulator_type=tf"
    url.4 <- paste("&upstream=", upstream, "kb", sep="")
    url.5 <- paste("&downstream=", downstream, "kb", sep="")
    url.e <- "&up_down_flag=0&motif_status=Y&sample_flag=0&protein_flag=0&Ftype=tab"
    url <- paste(url.b, url.1, url.2, url.3, url.4, url.5, url.e, sep="")    
    
    # Build dataframe from data
    r <- GET(url)
    text <- content(r, "text", encoding = "ISO-8859-1")
    text.split <- strsplit(text, "\n")[[1]]
    data.split <- text.split[8:length(text.split)]
    data <- paste(data.split, collapse = '\n')
    df <- read.table(text=data, sep="\t", header=1, stringsAsFactors=FALSE)
    return(df)
}

# Example
ensembl <- load_biomaRt()
df <- get_regulators(ensembl, "YAP1")
