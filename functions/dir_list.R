# Script name: dir_list.R
# Purpose: Restituisce i file contenuti in una cartella
# Used packages: -
# Author: Francesco Marella
# Date: 22-03-2014

dir.list <- function (directory = ".", recursive = FALSE, ignore.case = FALSE)
{
    d <- dir(directory, full.names = TRUE, recursive = recursive, ignore.case = ignore.case)
    if (length(d) == 0)
        stop("empty directory")
    isfile <- logical(length(d))
    for (i in seq_along(d)) isfile[i] <- !file.info(d[i])["isdir"]
    s <- d[isfile]
    s
}
