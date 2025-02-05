# runbLSlurm main function
runbLSlurm <- function(input_list, cat_path, ..., wait = TRUE) {
    
    # print usage without arguments
    if ((missing(input_list) || missing(cat_path) || missing(...))) {
        # print usage here (especially what to pass as ...)
        cat('"runbLSlurm" arguments:
        \r\tinput_list - list with model input (ModelInput in runbLS)
        \r\tcat_path   - path to catalog directory (Cat.Path in runbLS)
        \r\t...        - slurm options which are transfered to the sbatch file
        \r\t             mandatory is "mem". 
        \r\t             "partition" and "nodes" can be provided. 
        \r\t             If they are not provided, the best partition/node/cpu combination will be chosen.
        \r\t             "cpus", resp. "cpus-per-task" is derived from the number of rows that need to be calculated.
        \r\t             These arguments can also be provided as list().
        \r\twait       - if TRUE (default), the function will wait for the slurm job to finish
        \r\t             and collect the slurm job results before returning it.
        \r\t             If FALSE, the job-id and the job-directory will be returned such that the user
        \r\t             can collect the results via the function "collect_results".\n')
        return(invisible(NULL))
    }

    # get current time
    current_time <- Sys.time()

    # get dot arguments
    dots <- list(...)

    # check if list of options has been provided
    if (any(sapply(dots, is.list))) {
        dots <- unlist(dots, recursive = FALSE)
    }

    # remove NA values
    isna <- as.logical(rowSums(is.na(input_list$Interval[, 1:13])))
    input_list$Interval <- input_list$Interval[!isna, ]
    if (any(isna)) cat('Removed', sum(isna), 'rows due to NA values\n')

    # get nrows of Interval
    ntasks <- NROW(input_list$Interval)

    # remove -n --ntasks and give warning
    dots <- clean_ntasks(dots)

    # jobname (-J --job-name)
    job_name <- get_jobname(dots, current_time)

    # memory usage (--mem --mem-per-cpu)
    mem <- get_mem(dots)
    # an option could be, to distribute mem and tasks
    # to different nodes with different cpus
    # for now, finding the most cpus by using equal mem/cpu
    # distribution should be sufficient

    # find partition and nodes
    # partition name (-p --partition)
    # number of nodes (-N --nodes)
    # cpus per task
    part <- find_partition(mem, ntasks, dots)

    # create temporary directory
    # check if -D --chdir exists
    # otherwise default to $HOME/.slurm/$jobname
    tmp_dir <- get_sopt(dots, 'D', 'chdir', alternative = {
        # get home directory
        home <- Sys.getenv('HOME')
        # set path to $HOME/.slurm
        file.path(home, '.slurm', job_name)
    })

    # create tmp_dir if directory doesn't exists
    if (!dir.exists(tmp_dir)) {
        dir.create(tmp_dir, recursive = TRUE)
    } else {
        cat(paste0('Directory "', tmp_dir, '" already exists with content:\n'))
        system(paste('ls -la', tmp_dir))
        cat('Make sure that you have removed any unwanted files in it!\n')
        ans <- 'ask'
        while (!(ans %in% c('y', 'Y', 'yes', 'N', 'n', 'no', ''))) {
            ans <- readline('Do you want to proceed? [Y/n]: ')
        }
        if (any(ans %in% c('N', 'n', 'no'))) return(invisible(NULL))
    }

    # split Intervals and save to rds files
    il <- split_int(input_list$Interval, part)
    for (i in seq_along(il)) {
        saveRDS(il[[i]], file.path(tmp_dir, paste0('int', i, '.rds')))
    }

    # remove Interval and save model input list
    input_list$Interval <- NULL
    saveRDS(input_list, file.path(tmp_dir, 'input_list.rds'))

    # create script with argument
    rscript_file <- write_script(tmp_dir, cat_path, part[, cpus_per_task])

    # dots without partition, nodes, cpus_per_task
    dots <- get_sopt(dots, 
        'p', 'partition', 'N', 'nodes', 'cpus.*', 'J', 'job-name', 
        remove = TRUE) 

    # create sbatch file
    sbatch_file <- write_sbatch(
        tmp_dir, 
        rscript_file,
        'job-name' = job_name,
        partition = part[, Part],
        nodes = part[, nodes],
        'cpus-per-task' = part[, cpus_per_task],
        dots
    )

    # run sbatch file
    re <- system(paste('sbatch', sbatch_file), intern = TRUE)
    cat(re, '\n')
    # capture job id
    job_id <- sub('[^0-9]*([0-9]*)[^0-9]*', '\\1', re)

    if (wait) {
        # wait for job to finish
        # how can we know that jobs finished?
        # squeue -j $job_id
        sq <- system(paste0('squeue -j ', job_id), intern = TRUE)
        # be verbose
        cat(sq[1], '\n')
        cat(sq[2])
        # initiate seconds to wait
        sleep_secs <- 1
        # loop until job is done
        while (length(sq) == 2) {
            # wait some time
            Sys.sleep(sleep_secs)
            # get job state
            sq <- system(paste0('squeue -j ', job_id), intern = TRUE)
            if (length(sq) == 2) {
                cat('\r', sq[2])
                # TODO: grep for job state and start time recording once job started
            }
            # increase waiting time by 1 second
            sleep_secs <- sleep_secs + 1
        }
        cat('\njob finished.\n')

        # if else: either wait for job to finish or save job id and collect later

        # collect and return results
        res <- collect_results(tmp_dir)

        # duration of job?
        dur <- Sys.time() - current_time
        cat('Time since sending job: ', round(dur, 2), attr(dur, 'units'), '\n')

        # return value
        res

    } else {
        # be verbose
        cat('Not waiting for job to finish & returning job info.
        \rUse "collect_results(x)" where argument "x" is either
        \r\tthe returned object or
        \r\tthe path to the job directory
        \r')
        # return job id and tmp_dir
        list(
            'job-dir' = tmp_dir,
            'job-id' = job_id,
            'job-name' = job_name,
            'partition' = part[, Part],
            'nodes' = part[, nodes],
            'nodelist' = part[, node_names],
            'cpus-per-task' = part[, cpus_per_task]
        )
    }
}

# write sbatch file function
write_sbatch <- function(tmpdir, rscript, ...) {
    # have a look at sample slurm scripts: https://help.rc.ufl.edu/doc/Sample_SLURM_Scripts
    # and here!: https://support.ceci-hpc.be/doc/_contents/QuickStart/SubmittingJobs/SlurmTutorial.html
    # capture sbatch arguments
    arg_values <- list(...)
    # check if ... has list
    if (any(sapply(arg_values, is.list))) {
        arg_values <- unlist(arg_values, recursive = FALSE)
    }
    # check for argument names
    arg_names <- names(arg_values)
    # error if unnamed argument(s) exist
    if (is.null(arg_names) || any(arg_names %in% '')) {
        stop('Unnamed SBATCH arguments')
    }
    # get temporary file name
    tmp <- tempfile(pattern = 'sbatch', tmpdir = tmpdir)
    # find Rscript path
    rscript_path <- file.path(R.home(), 'bin/Rscript')
    # write to file
    writeLines(c(
        '#!/bin/bash',
        # change working directory
        paste0('#SBATCH --chdir=', tmpdir),
        # loop over arguments
        sapply(arg_names, function(a) {
            # remove dash if nchar is 2
            if (nchar(a) == 2) a <- sub('[-]', '', a)
            # check argument
            arg <- switch(as.character(nchar(a))
                # add '-' if one char
                , '1' = paste0('-', a)
                # prepend '--'
                , sub('^[-]{0,2}', '--', a)
            )
            # concatenate string
            paste0('#SBATCH ', arg, '=', arg_values[[a]])
        }),
        paste0('for file in ', tmpdir, '/int*.rds\ndo'),
        paste0('srun -n1 -N1 ', rscript_path, ' --vanilla ', rscript, ' $file &\ndone\nwait')
        ),
        tmp
    )
    # return sbatch file path
    tmp
}

# split Interval into chunks
split_int <- function(int, p){
    # get rows
    nr <- NROW(int)
    # get nodes
    nodes <- p[, nodes]
    # minimum of tasks per node
    mt <- floor(nr / nodes)
    # residual tasks
    rt <- nr %% nodes
    # number of tasks per node
    nt <- c(rep(mt + 1, rt), rep(mt, nodes - rt))
    # upper
    up <- cumsum(nt)
    # lower
    lo <- c(1, up[-length(up)] + 1)
    # index
    ind <- mapply(':', lo, up, SIMPLIFY = FALSE)
    # split
    lapply(ind, function(i) int[i, ])
}

# write R script
write_script <- function(tmpdir, cpath, ncores) {
    # get tmpfile name
    tmp <- tempfile(pattern = 'Rscript', tmpdir = tmpdir, fileext = '.R')
    # write R script to tmp file
    writeLines(
        c(
            'library(bLSmodelR)',
            # format of file: int%i.rds
            'ifile <- commandArgs(TRUE)',
            paste0('int <- readRDS(ifile)'),
            paste0('inlist <- readRDS(file.path("', tmpdir, '", "input_list.rds"))'),
            'inlist$Interval <- int',
            paste0('res <- runbLS(inlist, "', cpath, '", ncores = ', ncores, ')'),
            # get index from int%i.rds
            paste0('saveRDS(res, sub("/int([0-9]{1,2}[.]rds)", "/res\\\\1", ifile))')
        ), 
        tmp
    )
    # make file executable
    # Sys.chmod could be a better option?
    system(paste('chmod +x', tmp))
    # return tmpfile name
    tmp
}

# get slurm option
find_sopt <- function(x, ...) {
    if (length(x)) {
        # loop over arguments
        out <- sapply(list(...), function(x) {
            # remove trailing dashes
            if (nchar(x) == 2) {
                x <- sub('[-]', '', x)
            } else {
                x <- sub('^[-]{2}', '', x)
            }
            # add optional dashes
            switch(as.character(nchar(x))
                , '1' = paste0('^[-]?', x, '$')
                , paste0('^([-]{2})?', x, '$')
            )
        })
        # paste with OR |
        rexpr <- paste0('(', paste(out, collapse = '|'), ')')
        grepl(rexpr, names(x))
    } else {
        NULL
    }
}

# get slurm option value
get_sopt <- function(x, ..., alternative = x, remove = FALSE) {
    ind <- find_sopt(x, ...)
    if (any(ind)) {
        if (remove) {
            x[!ind]
        } else {
            if (sum(ind) > 1) {
                x[ind]
            } else {
                x[[which(ind)]]
            }
        }
    } else {
        alternative
    }
}

# get job name function
get_jobname <- function(x, ctime) {
    jn <- find_sopt(x, 'J', 'job-name')
    if (any(jn)) {
        x[[which(jn)]]
    } else {
        format(ctime, 'job_%y%m%d_%H%M%S')
    }
}

# get memory option
get_mem <- function(x) {
    mn <- find_sopt(x, 'mem')
    if (any(mn)) {
        x[[which(mn)]]
    } else {
        stop('Please provide the amount of memory to allocate for each node (--mem=..?)')
    }
}

clean_ntasks <- function(x) {
    tn <- find_sopt(x, 'n', 'ntasks')
    if (any(tn)) {
        warning('argument "', names(x)[tn], '" will be ignored since the number of tasks is given by the number of "Interval" rows')
        x[[-which(tn)]]
    } else {
        x
    }
}

# find partition
find_partition <- function(memory, ...) {
    # ni call
    ni_call <- 'ni'
    # capture dots
    dts <- list(...)
    # check if list of options has been provided
    if (any(sapply(dts, is.list))) {
        dts <- unlist(dts, recursive = FALSE)
    }
    # print table without arguments
    if (missing(memory) && length(dts) == 0) {
        return(system('ni'))
    } else if (is.na(suppressWarnings(as.numeric(sub('[A-Z]$', '', memory))))) {
        pattern <- paste0('.*', memory, '.*|$')
        return(system(paste0('ni | grep --color -E \'', pattern, '\'')))
    }
    # check ntasks
    ntasks <- get_sopt(dts, 'n', 'ntasks', alternative = Inf)
    # correct memory
    if (is.character(memory)) {
        mem <- suppressWarnings(as.numeric(sub('(T|G|M|K)$', '', memory)))
        memory <- mem * switch(sub('.*(T|G|M|K)$', '\\1', memory)
            , 'T' = 1e6
            , 'G' = 1e3
            , 'M' = 1
            , 'K' = 1e-3
            , stop('Memory unit not recognized')
        )
    }
    # check for partition in options
    part <- get_sopt(dts, 'p', 'partition', alternative = NULL)
    # check for number of nodes in partition
    Nodes <- get_sopt(dts, 'N', 'nodes', alternative = NULL)
    # read partition table
    ni <- data.table::fread(cmd = ni_call)
    # check memory
    ni_mem <- ni[!(State %in% 'alloc') & (MFree * 1e3) >= memory]
    if (nrow(ni_mem) == 0) {
        stop({find_partition(); paste0('No partition with enough memory available.')})
    }
    # summarize
    ni_sum <- ni_mem[, {
        Cav <- unique(CIdle)
        rbindlist(lapply(Cav, function(cav) {
            # print node names if colored below
            ind <- CIdle >= cav
            Nodes <- 1:sum(ind)
            .(
                node_names = list(Node[ind]),
                nodes = Nodes,
                cpus = Nodes * cav,
                cpus_per_task = cav
            )
        }))
    }, by = Part][nodes <= ntasks]
    # check partition
    if (is.null(part)) {
        # exclude alloc
        out <- ni_sum[order(cpus, cpus_per_task, decreasing = TRUE)[1], ]
    } else {
        # select part
        out <- ni_sum[Part %in% part]
        # check if partition is available
        if (nrow(out) == 0) {
            stop({find_partition(part); paste0('Specified partition "', part, '" has not enough resources available.')})
        }
        # nodes specified or not?
        if (is.null(Nodes)) {
            out <- out[1, ]
        } else {
            # check if nodes number is equal
            if (out[, any(ind <- nodes == Nodes)]) {
                out <- out[ind][which.max(cpus_per_task),]
            } else {
                # else error
                stop({find_partition(part); paste0('Specified partition "', part, '" has not specified number of nodes available.')})
            }
        }
    }
    # print system call with selected nodes highlighted
    patterns <- out[, paste(c(paste0(unlist(node_names), '.*'), '$'), collapse = '|')]
    system(paste0(ni_call, ' | grep --color -E \'', patterns, '\''))
    # return selected as data.table
    out
}

collect_results <- function(job_dir) {
    # job_dir as list with jobid
    if (is.list(job_dir)) job_dir <- job_dir[['job-dir']]
    # be verbose about collecting from path and job
    cat('Collecting results from:', job_dir, '\n')
    # get res.*rds file paths
    res_files <- dir(job_dir, pattern = 'res.*[.]rds', full.names = TRUE)
    # read in
    res_list <- lapply(res_files, readRDS)
    # check them
    if (any(lengths(res_list) == 0) || any(sapply(res_list, nrow) == 0)) {
        # what now?
        browser()
    }
    # get original rn values
    rn_values <- unlist(lapply(res_list, function(x) x[, rn]))
    # join them to one
    res <- do.call(join, res_list)
    # get new rn values
    names(rn_values) <- res[, rn]
    # restore rn values
    res[, rn := rn_values]
    # also in attributes
    attr(res, 'CalcSteps')[, rn := rn_values[rn]]
    attr(res, 'Catalogs')[, rn := rn_values[rn]]
    # return
    res
}
