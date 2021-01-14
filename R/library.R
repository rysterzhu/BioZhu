suppressWarnings(library(ggplot2))
suppressWarnings(library(data.table))
#suppressWarnings(library(reshape2, warn.conflicts = FALSE)) #conflicts with data.table
suppressWarnings(library(magrittr))
suppressWarnings(library(pheatmap))
suppressWarnings(library(stringr))
suppressWarnings(library(RColorBrewer))

# NFC=c("MII","sperm", "zygote", "2cell", "4cell", "8cell", "morula", "ICM", "TE")
# NTC=c("cc", "6h", "14h", "2cell", "4cell", "8cell", "morula", "icm", "te", "blast")
# SNT=c('14h', '2cell', '4cell', '6h', '8cell', 'blast', 'cc', 'icm', 'morula','te')
# SNF=c('2cell', '4cell', '8cell', 'ICM', 'MII', 'morula', 'sperm', 'TE', 'zygote')
#
# NFN=c("MII","Sperm", "PN3-Zygote", "NF-2cell", "NF-4cell", "NF-8cell", "NF-Morula", "NF-ICM", "NF-TE")
# NTN=c("CC", "6-hpa", "14-hpa", "NT-2cell", "NT-4cell", "NT-8cell", "NT-Morula", "NT-ICM", "NT-TE", "NT-Blastocyst")

options(scipen=200,sep = "\t")
formals(fwrite)$sep <- "\t"
formals(data.frame)$check.names <- F
formals(data.frame)$stringsAsFactors <- F
formals(write.table)$row.names=F
formals(write.table)$quote=F
formals(write.table)$sep="\t"
formals(ggsave)$useDingbats=F

is.blank <- function(x, false.triggers=FALSE){
    if(is.function(x)) return(FALSE) # Some of the tests below trigger
    # warnings when used on functions
    return(
        is.null(x) ||                # Actually this line is unnecessary since
            length(x) == 0 ||            # length(NULL) = 0, but I like to be clear
            all(is.na(x)) ||
            all(x=="") ||
            (false.triggers && all(!x))
    )
}

set.data.frame <- function(dt,row.names=key(dt)){
    temp<-data.frame(dt,check.names = F)
    if(is.blank(row.names)){
        rownames(temp)<-1:dim(temp)[1]
        temp
    }else if(is.numeric(row.names)){
        rownames(temp)<-temp[,row.names]
        temp[,-row.names,drop=F]
    }else{
        rownames(temp)<-temp[,row.names]
        temp[,colnames(temp)!=row.names,drop=F]
    }
}

write.tab <- function(x,file="",quote=F,row.names=F,col.names=T,sep="\t",...)
    write.table(x,file = file,quote = quote,row.names = row.names,col.names = col.names,sep = sep,...)
read.tab <- function(...,row.names=NA) data.table::fread(...) %>%
    set.data.frame(row.names = row.names)
str.split <- function(string, pattern, col.names=NA, n = Inf, simplify = T)
    str_split(string, pattern, n, simplify) %>%
    data.frame() %>% `colnames<-`(if(is.blank(col.names)) 1:dim(.)[2] else col.names)


theme_zhu = theme_bw() + theme(panel.grid= element_blank(),
                               aspect.ratio = 0.75,
                               text = element_text(family = "Arial",size=11))

message("library done.")
