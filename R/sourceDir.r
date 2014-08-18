# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   1:49 PM Wednesday, 13 March 2013
# * Copyright: AS IS
# *
# * $Revision: 4296 $
# * $Id: sourceDir.r 4296 2014-08-18 04:32:50Z zhe00a $
# * $Author: zhe00a $
# * $Date: 2014-08-18 14:32:50 +1000 (Mon, 18 Aug 2014) $

#'  Sourcing a bunch of files located from different folders/subfolders.
#' 
#' @param path "Character". It is the path you want to source files.
#' @param recursive "Logical". If TRUE, files under subfolders will also be sourced.
#' @param only.funs Locigal. If TRUE, only functions will be sourced.
#' @param ... type help("source")
#' @export
source_dir <- function(path, recursive = TRUE, only.funs = TRUE, ...)
{
    if(missing(path))
    {
        path <- getwd()
    }
    for(i in 1:length(path))
    {
        files <- list.files(path[i], pattern = "\\.[Rr]$", recursive = recursive, full.names = TRUE)
        for (nm in files)
        {
            if (only.funs)
            {
                source_funs(nm, ...)
            } else
            {
                source(nm, ...)
            }
        }
    }
}

#' Source functions from a File or a connection
#' 
#' This function is same as source, only including functions. Modified from source function in R
#' @param file a connection or a character string giving the pathname of the file or URL to read from. "" indicates the connection stdin().
#' @param local See source
#' @param echo See source
#' @param print.eval See source
#' @param verbose See source
#' @param prompt.echo See source
#' @param max.deparse.length See source
#' @param chdir See source
#' @param encoding See source
#' @param continue.echo See source
#' @param skip.echo See source
#' @param keep.source See source
#' @export
source_funs <- function (file, local = FALSE, echo = verbose, print.eval = echo, 
    verbose = getOption("verbose"), prompt.echo = getOption("prompt"), 
    max.deparse.length = 150, chdir = FALSE, encoding = getOption("encoding"), 
    continue.echo = getOption("continue"), skip.echo = 0, keep.source = getOption("keep.source")) 
{
    library(methods)
    library(utils)
    envir <- if (isTRUE(local)) 
    {
        parent.frame()
    } else if (identical(local, FALSE)) 
    {
        .GlobalEnv
    } else if (is.environment(local)) 
    {
        local
    } else 
    {
        stop("'local' must be TRUE, FALSE or an environment")
    }
    have_encoding <- !missing(encoding) && encoding != "unknown"
    if (!missing(echo)) 
    {
        if (!is.logical(echo)) 
        {
            stop("'echo' must be logical")
        }
        if (!echo && verbose) 
        {
            warning("'verbose' is TRUE, 'echo' not; ... coercing 'echo <- TRUE'")
            echo <- TRUE
        }
    }
    
    if (verbose) 
    {
        cat("'envir' chosen:")
        print(envir)
    }
    ofile <- file
    from_file <- FALSE
    srcfile <- NULL
    if (is.character(file)) 
    {
        if (identical(encoding, "unknown")) 
        {
            enc <- utils::localeToCharset()
            encoding <- enc[length(enc)]
        } else 
        {
            enc <- encoding
        }
        if (length(enc) > 1L) 
        {
            encoding <- NA
            owarn <- options("warn")
            options(warn = 2)
            for (e in enc) 
            {
                if (is.na(e)) 
                  next
                zz <- file(file, encoding = e)
                res <- tryCatch(readLines(zz, warn = FALSE), 
                  error = identity)
                close(zz)
                if (!inherits(res, "error")) 
                {
                    encoding <- e
                    break
                }
            }
            options(owarn)
        }
        if (is.na(encoding)) 
        {
            stop("unable to find a plausible encoding")
        }
        if (verbose) 
        {
            cat(gettextf("encoding = \"%s\" chosen", encoding), 
                "\n", sep = "")
        }
        if (file == "") 
        {
            file <- stdin()
        } else 
        {
            filename <- file
            file <- file(filename, "r", encoding = encoding)
            on.exit(close(file))
            if (isTRUE(keep.source)) 
            {
                lines <- readLines(file, warn = FALSE)
                on.exit()
                close(file)
                srcfile <- srcfilecopy(filename, lines, file.info(filename)[1, 
                  "mtime"], isFile = TRUE)
            } else 
            {
                from_file <- TRUE
            }
            loc <- utils::localeToCharset()[1L]
            encoding <- if (have_encoding) 
            {
                switch(loc, `UTF-8` = "UTF-8", `ISO8859-1` = "latin1", 
                  "unknown")
            } else
            {
                "unknown"
            }
        }
    } else 
    {
        lines <- readLines(file, warn = FALSE)
        if (isTRUE(keep.source)) 
        {
            srcfile <- srcfilecopy(deparse(substitute(file)), 
                lines)
        }
    }
    if (!isTRUE(keep.source)) 
    {
        op <- options(keep.source = FALSE)
        on.exit(options(op), add = TRUE)
    } else 
    {
        op <- NULL
    }
    exprs <- if (!from_file) 
    {
        if (length(lines)) 
        {
            .Internal(parse(stdin(), n = -1, lines, "?", srcfile, 
                encoding))
        } else 
        { 
            expression()
        }
    } else 
    {
        .Internal(parse(file, n = -1, NULL, "?", srcfile, encoding))
    }
    on.exit()
    if (from_file) 
    {
        close(file)
    }
    if (!is.null(op)) 
    {
        options(op)
    }
    Ne <- length(exprs)
    if (verbose) 
    {
        cat("--> parsed", Ne, "expressions; now eval(.)ing them:\n")
    }
    if (chdir) 
    {
        if (is.character(ofile)) 
        {
            isURL <- length(grep("^(ftp|http|file)://", ofile)) > 
                0L
            if (isURL) 
            {
                warning("'chdir = TRUE' makes no sense for a URL")
            }
            if (!isURL && (path <- dirname(ofile)) != ".") 
            {
                owd <- getwd()
                if (is.null(owd)) 
                  stop("cannot 'chdir' as current directory is unknown")
                on.exit(setwd(owd), add = TRUE)
                setwd(path)
            }
        } else 
        {
            warning("'chdir = TRUE' makes no sense for a connection")
        }
    }
    if (echo) 
    {
        sd <- "\""
        nos <- "[^\"]*"
        oddsd <- paste("^", nos, sd, "(", nos, sd, nos, sd, ")*", 
            nos, "$", sep = "")
        trySrcLines <- function(srcfile, showfrom, showto) 
        {
            lines <- try(suppressWarnings(getSrcLines(srcfile, 
                showfrom, showto)), silent = TRUE)
            if (inherits(lines, "try-error")) 
                lines <- character()
            lines
        }
    }
    yy <- NULL
    lastshown <- 0
    srcrefs <- attr(exprs, "srcref")
    for (i in seq_len(Ne + echo)) 
    {
        if (length(exprs[[i]]) == 3 && class(exprs[[i]]) == '<-' && 
            length(grep('^function\\(.*$', as.character(exprs[[i]][3]))) > 0)
        {
            tail <- i > Ne
            if (!tail) 
            {
                if (verbose) 
                    cat("\n>>>> eval(expression_nr.", i, ")\n\t\t =================\n")
                ei <- exprs[i]
            }
            if (echo) 
            {
                srcref <- NULL
                nd <- 0
                if (tail) 
                    srcref <- attr(exprs, "wholeSrcref")
                else if (i <= length(srcrefs)) 
                    srcref <- srcrefs[[i]]
                if (!is.null(srcref)) 
                {
                    if (i == 1) 
                    {
                        lastshown <- min(skip.echo, srcref[3L] - 1)
                    }
                    if (lastshown < srcref[3L]) 
                    {
                        srcfile <- attr(srcref, "srcfile")
                        dep <- trySrcLines(srcfile, lastshown + 1, 
                        srcref[3L])
                        if (length(dep)) 
                        {
                            if (tail) 
                            {
                                leading <- length(dep)
                            } else
                            {
                                leading <- srcref[1L] - lastshown
                            }
                            lastshown <- srcref[3L]
                            while (length(dep) && length(grep("^[[:blank:]]*$", 
                              dep[1L]))) {
                              dep <- dep[-1L]
                              leading <- leading - 1L
                        }
                        dep <- paste(rep.int(c(prompt.echo, continue.echo), 
                          c(leading, length(dep) - leading)), dep, 
                          sep = "", collapse = "\n")
                        nd <- nchar(dep, "c")
                        } else
                        { 
                        srcref <- NULL
                        }
                    }
                }
                if (is.null(srcref)) 
                {
                    if (!tail) {
                      dep <- substr(paste(deparse(ei, control = "showAttributes"), 
                        collapse = "\n"), 12L, 1000000L)
                      dep <- paste0(prompt.echo, gsub("\n", paste0("\n", 
                        continue.echo), dep))
                      nd <- nchar(dep, "c") - 1L
                    }
                }
                if (nd) {
                    do.trunc <- nd > max.deparse.length
                    dep <- substr(dep, 1L, if (do.trunc) 
                      max.deparse.length
                    else nd)
                    cat("\n", dep, if (do.trunc) 
                      paste(if (length(grep(sd, dep)) && length(grep(oddsd, 
                        dep))) 
                        " ...\" ..."
                      else " ....", "[TRUNCATED] "), "\n", sep = "")
                }
            }
            if (!tail) 
            {
                yy <- withVisible(eval(ei, envir))
                i.symbol <- mode(ei[[1L]]) == "name"
                if (!i.symbol) 
                {
                    curr.fun <- ei[[1L]][[1L]]
                    if (verbose) {
                      cat("curr.fun:")
                      utils::str(curr.fun)
                    }
                }
                if (verbose >= 2) 
                {
                    cat(".... mode(ei[[1L]])=", mode(ei[[1L]]), "; paste(curr.fun)=")
                    utils::str(paste(curr.fun))
                }
                if (print.eval && yy$visible) 
                {
                    if (isS4(yy$value)) 
                      methods::show(yy$value)
                    else print(yy$value)
                }
                if (verbose) 
                {
                    cat(" .. after ", sQuote(deparse(ei, control = c("showAttributes", 
                      "useSource"))), "\n", sep = "")
                }
            }
        }
    }
    invisible(yy)
}
