# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   02:21 PM Monday, 18 August 2014
# * Copyright: AS IS
# *



#' Load a project, the first function will be called when working for a project
#' 
#' @param project The project name
#' @export
project_load <- function(project = NULL, base = getwd())
{
    para <- project_read_para(project = project, base = base)
}

#' Source the function in the project
#'
#' @param force Force to reload all functions
#' @param project The project name
#' @param all Source all functions
#' @export
project_fun <- function(force = FALSE, project = NULL, all = FALSE)
{
    is_source <- FALSE
    
    if(!project_get_para('source_fun', project = project) | force)
    {
        is_source <- TRUE
    }
    if (is_source)
    {
        if (!all)
        {
            file <- project_filepath('Rcode', 
                sprintf('%sFunctions.R', project_get_para('prefix', project = project)), 
                project = project)
            if (file.exists(file))
            {
                source(file)
            }
        } else
        {
            source_dir(project_filepath('Rcode', project = project))
        }
        project_set_pata('source_fun', TRUE, project = project)
    }
}


#' Construct the path to a file from components in the project
#' 
#' @param ... character vectors
#' @param project The project name
#' @export
project_filepath <- function(..., project = NULL)
{
    base <- project_get_para('base', project = project)
    do.call(file.path, c(list(base), list(...)))
}

#' Read a project file (csv, RData, ...)
#' 
#' @param file a character string giving the name of the file to load
#' @param project The project name
#' @export
project_read <- function(file, project = NULL, var = NULL)
{
    type <- tolower(
        gsub('.*\\.(.*)', '\\1', file))
    file <- project_filepath(file, project = project)
    res <- NULL
    if (type == 'csv')
    {
        if (!is.null(var))
        {
            if (exists(var, .GlobalEnv))
            {
                res <- get(var, .GlobalEnv)
            } else
            {
                res <- read.csv(file, as.is = TRUE)
                assign(var, res, .GlobalEnv)
            }
        } else
        {
            res <- read.csv(file, as.is = TRUE)
        }
        return(res)
    } else if (type == 'rdata')
    {
        load(file, envir = parent.frame())
        # return()
    } else if (type == 'rds')
    {
        return(readRDS(file))
    } else
    {
        stop('NOT IMPLEMENTED')
    }
}

#' Search the file from current folder to root 
#' 
#' @param filename the filename for search
project_findfile <- function(filename, base = getwd())
{
    config <- file.path(base, filename)
    if (!file.exists(config))
    {
        library(stringr)
        base <- gsub('\\\\', '/', base)
        c_file <- unlist(lapply(1:str_count(base, '/'), function(x)
            {
                c_file <- paste(rep('..', x), collapse = '/')
                c_file <- file.path(base, c_file, filename)
                c_file
            }))
        
        pos <- which(file.exists(c_file))
        if (length(pos) == 0)
        {
            stop(paste0(filename, ' does not exist'))
        }
        config <- c_file[pos[1]]
        base <- dirname(config)
    }
    return(config)
}

#' Read ini file
#'
#' @param filename The ini file
project_readini <- function(filename)
{
    a <- readLines(filename)
    a <- a[-grep('^#', a)]
    a <- a[nchar(a) > 0]
    a <- sub('^\\s+', '', a)
    a <- sub('\\s+$', '', a)
    # for normal parameter
    a1 <- a[grep(' *= *', a)]
    pars1 <- list()
    if (length(a1) > 0)
    {
        a1 <- strsplit(a1, ' *= *')
        pars1 <- lapply(a1, function(x) x[2]) 
        names(pars1) <- as.character(lapply(a1, function(x) x[1]))
        pars1 <- lapply(pars1, function(x)
            {    
                if (length(grep('\\\\\\\\', x)) == 0)
                {
                    x <- gsub('\\\\', '/', x)
                    if (length(grep('/', x)) > 0 & length(grep(':', x)) == 0)
                    {
                        x <- file.path(dirname(filename), x)
                    }
                }
                return (x)
            })
    }
    a2 <- a[grep(' *<- *', a)]
    pars2 <- list()
    if (length(a2) > 0)
    {
        a2 <- strsplit(a2, ' *<- *')
        pars2 <- lapply(a2, function(x) x[2]) 
        names(pars2) <- as.character(lapply(a2, function(x) x[1]))
        pars2 <- lapply(pars2, function(x)
            {    
                x <- eval(parse(text = as.character(x)))
                return (x)
            })
    }
    pars <- c(pars1, pars2)
    pars
}

#' Get the project list
#'
project_list <- function(base = getwd())
{
    var_name <- 'global_project_list'
    if (exists(var_name))
    {
        p_list <- get(var_name, envir = .GlobalEnv)
    } else
    {
        pro_file <- project_findfile('project.ini', base = base)
        p_list <- project_readini(pro_file)
        p_list$base <- dirname(pro_file)
        assign(var_name, p_list, envir = .GlobalEnv)
    }
    p_list
}

#' Read parameter files
#'
#' @param project The project name
#' @export
project_read_para <- function(project = NULL, base = NULL)
{
    if (is.null(base))
    {
        if (is.null(project))
        {
            base <- getwd()
        } else
        {
            base <- project_get_para('base')
        }
    } 
    if (is.null(project))
    {
        config <- project_findfile('config.ini', base = base)
    } else
    {
        pro_conf <- project_list(base)
        if (is.null(pro_conf[[project]]))
        {
            stop(paste0('Project "', project, '" doesn\'t exist'))
        }
        config <- project_findfile('config.ini', 
            file.path(pro_conf$base, pro_conf[[project]]))
    }
    
    # project <- ifelse(is.null(project), paras$config, project)
    var_name <- project_variable_name(project)
    if (exists(var_name))
    {
        pars <- get(var_name, envir = .GlobalEnv)
        return (pars)
    }
    
    pars <- project_readini(config)
    pars$base <- dirname(config)
    pars$source_fun <- FALSE
    assign(var_name, pars, envir = .GlobalEnv)
    return(pars)
}

#' Get parameter from config file
#'
#' @param name The parameter name
#' @param project The project name
#' @export
project_get_para <- function(name = NULL, project = NULL)
{
    var_name <- project_variable_name(project)
    if (!exists(var_name))
    {
        project_read_para(project = project)
    }
    pars <- get(var_name, envir = .GlobalEnv)
    if (is.null(name))
    {
        return (pars)
    }
    value <- pars[[name]]
    if (is.null(value))
    {
        stop(sprintf('%s does not exist', name))
    }
    return (value)
}

#' Set parameter
#'
#' @param name The parameter name
#' @param value The parameter value
#' @param project The project name
#' @export
project_set_pata <- function(name, value, project = NULL)
{
    var_name <- project_variable_name(project)
    pars <- get(var_name, envir = .GlobalEnv)
    pars[[name]] <- value
    assign(var_name, pars, envir = .GlobalEnv)
}

#' Generate variable name for global environment
#' @param project The project name
project_variable_name <- function(project = NULL)
{
    if (is.null(project))
    {
        project <- 'current'
    }
    paste0('global_pid_parameters_', project)
}

