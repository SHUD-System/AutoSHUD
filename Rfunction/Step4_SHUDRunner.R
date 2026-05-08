AUTOSHUD_STEP4_REQUIRED_INPUTS <- c(
  "sp.mesh", "sp.riv", "sp.att", "sp.rivseg",
  "cfg.para", "cfg.calib", "cfg.ic", "tsd.forc"
)

AUTOSHUD_STEP4_REQUIRED_OUTPUTS <- c(
  "eleysurf", "eleyunsat", "eleygw", "elevprcp", "elevetp",
  "elevinfil", "elevrech", "eleqsurf", "elevettr", "elevetic",
  "elevetev", "rivystage", "rivqdown", "rivqsub", "rivqsurf"
)

AUTOSHUD_STEP4_REQUIRED_OUTPUT_CONFIG <- c(
  DT_YE_SURF = 1440, DT_YE_UNSAT = 1440, DT_YE_GW = 1440,
  DT_QE_PRCP = 1440, DT_QE_INFIL = 1440,
  DT_QE_RECH = 1440, DT_QE_SURF = 1440, DT_QE_ET = 1440,
  DT_YR_STAGE = 1440, DT_QR_DOWN = 1440, DT_QR_SUB = 1440,
  DT_QR_SURF = 1440
)

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || all(is.na(x)) || !nzchar(x[[1]])) y else x
  }
}

autoshud_step4_validate_prjname <- function(prjname) {
  if (!is.character(prjname) || length(prjname) != 1L ||
      is.na(prjname) || !nzchar(prjname)) {
    stop("Unsafe Step4 project name: prjname must be a non-empty string.",
         call. = FALSE)
  }
  if (grepl("[[:cntrl:]/\\\\]", prjname) ||
      grepl("^~", prjname) ||
      grepl("^[A-Za-z]:", prjname) ||
      prjname %in% c(".", "..") ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", prjname)) {
    stop("Unsafe Step4 project name: ", prjname,
         ". Use a safe basename containing only letters, digits, '.', '_' or '-'.",
         call. = FALSE)
  }
  prjname
}

autoshud_step4_normalize_path <- function(path, base = getwd()) {
  path <- path.expand(as.character(path))
  is_absolute <- grepl("^/", path) || grepl("^[A-Za-z]:[/\\\\]", path)
  if (!is_absolute) {
    path <- file.path(base, path)
  }
  path <- gsub("\\\\", "/", path)
  prefix <- ""
  rest <- path
  if (grepl("^[A-Za-z]:/", path)) {
    prefix <- substr(path, 1L, 3L)
    rest <- sub("^[A-Za-z]:/+", "", path)
  } else if (startsWith(path, "/")) {
    prefix <- "/"
    rest <- sub("^/+", "", path)
  }
  parts <- unlist(strsplit(rest, "/+", perl = TRUE), use.names = FALSE)
  stack <- character()
  for (part in parts) {
    if (!nzchar(part) || identical(part, ".")) {
      next
    }
    if (identical(part, "..")) {
      if (length(stack)) {
        stack <- stack[-length(stack)]
      }
      next
    }
    stack <- c(stack, part)
  }
  if (identical(prefix, "/")) {
    if (length(stack)) paste0("/", paste(stack, collapse = "/")) else "/"
  } else {
    paste0(prefix, paste(stack, collapse = "/"))
  }
}

autoshud_step4_path_inside <- function(path, root) {
  path <- autoshud_step4_normalize_path(path)
  root <- autoshud_step4_normalize_path(root)
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

autoshud_step4_assert_path_inside <- function(path, root, label) {
  if (!autoshud_step4_path_inside(path, root)) {
    stop(label, " must stay under Step4 run_dir. path: ",
         autoshud_step4_normalize_path(path),
         "; run_dir: ", autoshud_step4_normalize_path(root),
         call. = FALSE)
  }
  invisible(autoshud_step4_normalize_path(path))
}

autoshud_step4_default_repo_root <- function() {
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (file.exists(file.path(cwd, "Step4_SHUD.R")) &&
      dir.exists(file.path(cwd, "Rfunction"))) {
    return(cwd)
  }
  env_root <- Sys.getenv("AUTOSHUD_REPO_ROOT", unset = "")
  if (nzchar(env_root) && dir.exists(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }
  cwd
}

autoshud_step4_source_candidates <- function(shud_source = NULL,
                                             xfg = NULL,
                                             repo_root = autoshud_step4_default_repo_root()) {
  config_source <- NULL
  if (!is.null(xfg$shud$source)) {
    config_source <- xfg$shud$source
  }
  c(
    explicit = shud_source,
    option = getOption("autoshud.shud_source"),
    env = Sys.getenv("AUTOSHUD_SHUD_SOURCE", unset = ""),
    env_dir = Sys.getenv("AUTOSHUD_SHUD_SOURCE_DIR", unset = ""),
    config = config_source,
    default = file.path(repo_root, "..", "SHUD")
  )
}

autoshud_step4_resolve_shud_source <- function(shud_source = NULL,
                                               xfg = NULL,
                                               repo_root = autoshud_step4_default_repo_root()) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = FALSE)
  candidates <- autoshud_step4_source_candidates(
    shud_source = shud_source, xfg = xfg, repo_root = repo_root
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  for (candidate in candidates) {
    candidate <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    if (dir.exists(candidate) &&
        file.exists(file.path(candidate, "Makefile")) &&
        dir.exists(file.path(candidate, "src"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  stop(
    "Local SHUD source directory is required for Step4. ",
    "Set shud.source in the project config, option('autoshud.shud_source'), ",
    "AUTOSHUD_SHUD_SOURCE, or place a SHUD checkout at ../SHUD relative to ",
    "the AutoSHUD repository root. No git clone or fallback solver is used.",
    call. = FALSE
  )
}

autoshud_step4_unrun_command <- function(command, args = character(), wd,
                                         stdout, stderr, timeout) {
  list(
    command = paste(c(command, args), collapse = " "),
    command_actual = paste(c(command, args), collapse = " "),
    executable = command,
    args = args,
    working_directory = normalizePath(wd, winslash = "/", mustWork = FALSE),
    stdout = normalizePath(stdout, winslash = "/", mustWork = FALSE),
    stderr = normalizePath(stderr, winslash = "/", mustWork = FALSE),
    timeout_seconds = timeout,
    exit_status = NA_integer_,
    error_message = NA_character_,
    started_at = NA_character_,
    finished_at = NA_character_,
    elapsed_seconds = NA_real_
  )
}

autoshud_step4_command_env <- function(extra = NULL, threads = 1L) {
  base <- c(
    OMP_NUM_THREADS = as.character(threads),
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1",
    NUMEXPR_NUM_THREADS = "1",
    MAKEFLAGS = "-j1"
  )
  if (!is.null(extra) && length(extra)) {
    extra_names <- names(extra)
    if (is.null(extra_names) || any(!nzchar(extra_names))) {
      stop("extra environment entries must be a named character vector.",
           call. = FALSE)
    }
    base[extra_names] <- as.character(extra)
  }
  paste0(names(base), "=", unname(base))
}

autoshud_step4_write_lines <- function(lines, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(lines), con = file, useBytes = TRUE)
  invisible(file)
}

autoshud_step4_run_logged_command <- function(command, args = character(),
                                              wd, stdout, stderr,
                                              timeout = 600,
                                              env = character()) {
  dir.create(dirname(stdout), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(stderr), recursive = TRUE, showWarnings = FALSE)
  start <- Sys.time()
  old_wd <- setwd(wd)
  on.exit(setwd(old_wd), add = TRUE)
  command_actual <- command
  args_actual <- args
  timeout_bin <- Sys.which("timeout")
  if (nzchar(timeout_bin) && is.finite(timeout) && timeout > 0) {
    command_actual <- unname(timeout_bin)
    args_actual <- c(paste0(as.integer(timeout), "s"), command, args)
  }
  status <- tryCatch(
    system2(command = command_actual, args = args_actual, stdout = stdout,
            stderr = stderr, wait = TRUE, env = env),
    error = function(e) structure(127L, error_message = conditionMessage(e))
  )
  end <- Sys.time()
  if (is.null(status)) status <- 0L
  list(
    command = paste(c(command, args), collapse = " "),
    command_actual = paste(c(command_actual, args_actual), collapse = " "),
    executable = command,
    args = args,
    working_directory = normalizePath(wd, winslash = "/", mustWork = FALSE),
    stdout = normalizePath(stdout, winslash = "/", mustWork = FALSE),
    stderr = normalizePath(stderr, winslash = "/", mustWork = FALSE),
    timeout_seconds = timeout,
    exit_status = as.integer(status),
    error_message = attr(status, "error_message") %||% NA_character_,
    started_at = format(start, "%Y-%m-%d %H:%M:%S %z"),
    finished_at = format(end, "%Y-%m-%d %H:%M:%S %z"),
    elapsed_seconds = as.numeric(difftime(end, start, units = "secs"))
  )
}

autoshud_step4_command_to_lines <- function(x, prefix) {
  field <- function(name) {
    value <- x[[name]]
    if (is.null(value) || length(value) == 0L) {
      return(NA_character_)
    }
    if (length(value) > 1L) {
      return(paste(as.character(value), collapse = " "))
    }
    as.character(value)
  }
  elapsed <- suppressWarnings(as.numeric(field("elapsed_seconds")))
  elapsed <- if (length(elapsed) && is.finite(elapsed)) {
    as.character(round(elapsed, 3))
  } else {
    NA_character_
  }
  c(
    paste0(prefix, ".command\t", field("command")),
    paste0(prefix, ".command_actual\t", field("command_actual") %||% field("command")),
    paste0(prefix, ".working_directory\t", field("working_directory")),
    paste0(prefix, ".stdout\t", field("stdout")),
    paste0(prefix, ".stderr\t", field("stderr")),
    paste0(prefix, ".timeout_seconds\t", field("timeout_seconds")),
    paste0(prefix, ".exit_status\t", field("exit_status")),
    paste0(prefix, ".started_at\t", field("started_at")),
    paste0(prefix, ".finished_at\t", field("finished_at")),
    paste0(prefix, ".elapsed_seconds\t", elapsed),
    paste0(prefix, ".error_message\t", field("error_message"))
  )
}

autoshud_step4_git_info <- function(source_dir) {
  run_git <- function(args) {
    out <- tryCatch(
      system2("git", c("-C", source_dir, args), stdout = TRUE, stderr = TRUE),
      warning = function(w) attr(w, "message") %||% conditionMessage(w),
      error = function(e) conditionMessage(e)
    )
    paste(out, collapse = "\n")
  }
  list(
    commit = run_git(c("rev-parse", "HEAD")),
    status = run_git(c("status", "--short"))
  )
}

autoshud_step4_required_input_files <- function(model_input_dir, prjname,
                                                suffixes = AUTOSHUD_STEP4_REQUIRED_INPUTS) {
  prjname <- autoshud_step4_validate_prjname(prjname)
  stats::setNames(file.path(model_input_dir, paste0(prjname, ".", suffixes)),
                  paste0(prjname, ".", suffixes))
}

autoshud_step4_check_required_inputs <- function(model_input_dir, prjname,
                                                 suffixes = AUTOSHUD_STEP4_REQUIRED_INPUTS) {
  files <- autoshud_step4_required_input_files(model_input_dir, prjname, suffixes)
  ok <- file.exists(files) & is.finite(file.info(files)$size) & file.info(files)$size > 0
  if (!all(ok)) {
    missing <- names(files)[!ok]
    stop("Step3 model input prerequisite(s) missing or empty before Step4: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  files
}

autoshud_step4_read_config <- function(file) {
  lines <- readLines(file, warn = FALSE)
  lines <- lines[!grepl("^\\s*(#|$)", lines)]
  x <- utils::read.table(text = lines, header = FALSE,
                         stringsAsFactors = FALSE)
  stats::setNames(as.character(x[[2]]), toupper(as.character(x[[1]])))
}

autoshud_step4_write_config <- function(x, file) {
  out <- data.frame(names(x), unname(x), stringsAsFactors = FALSE)
  utils::write.table(out, file = file, sep = "\t", quote = FALSE,
                     row.names = FALSE, col.names = FALSE)
  invisible(file)
}

autoshud_step4_configure_required_outputs <- function(model_input_dir, prjname,
                                                      end_day = 1,
                                                      output_interval = 1440,
                                                      max_solver_step = NULL) {
  prjname <- autoshud_step4_validate_prjname(prjname)
  model_input_dir <- normalizePath(model_input_dir, winslash = "/",
                                   mustWork = TRUE)
  cfg_file <- file.path(model_input_dir, paste0(prjname, ".cfg.para"))
  autoshud_step4_assert_path_inside(cfg_file, model_input_dir,
                                    "Step4 cfg.para path")
  cfg <- autoshud_step4_read_config(cfg_file)
  required <- AUTOSHUD_STEP4_REQUIRED_OUTPUT_CONFIG
  required[] <- as.character(output_interval)
  cfg[names(required)] <- unname(required)
  if (!is.null(end_day)) {
    cfg["END"] <- as.character(end_day)
  }
  if (!is.null(max_solver_step)) {
    cfg["MAX_SOLVER_STEP"] <- as.character(max_solver_step)
  }
  cfg["BINARY_OUTPUT"] <- "1"
  autoshud_step4_write_config(cfg, cfg_file)
  invisible(cfg)
}

autoshud_step4_required_output_files <- function(output_dir, prjname,
                                                 variables = AUTOSHUD_STEP4_REQUIRED_OUTPUTS) {
  prjname <- autoshud_step4_validate_prjname(prjname)
  stats::setNames(file.path(output_dir, paste0(prjname, ".", variables, ".dat")),
                  variables)
}

autoshud_step4_check_required_outputs <- function(output_dir, prjname,
                                                  variables = AUTOSHUD_STEP4_REQUIRED_OUTPUTS) {
  files <- autoshud_step4_required_output_files(output_dir, prjname, variables)
  ok <- file.exists(files) & is.finite(file.info(files)$size) & file.info(files)$size > 0
  if (!all(ok)) {
    missing <- names(files)[!ok]
    stop("Required real SHUD solver output(s) missing or empty after Step4: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  files
}

autoshud_step4_stage_inputs <- function(model_input_dir, run_dir, prjname) {
  prjname <- autoshud_step4_validate_prjname(prjname)
  run_dir <- normalizePath(run_dir, winslash = "/", mustWork = FALSE)
  expected <- normalizePath(file.path(run_dir, "input", prjname),
                            winslash = "/", mustWork = FALSE)
  autoshud_step4_assert_path_inside(expected, run_dir,
                                    "Step4 staged input directory")
  actual <- normalizePath(model_input_dir, winslash = "/", mustWork = FALSE)
  if (identical(actual, expected)) {
    return(actual)
  }
  dir.create(dirname(expected), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(expected)) {
    unlink(expected, recursive = TRUE, force = TRUE)
  }
  dir.create(expected, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(actual, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  ok <- vapply(files, function(path) {
    file.copy(path, expected, recursive = TRUE, copy.date = TRUE,
              overwrite = TRUE)
  }, logical(1))
  if (!all(ok)) {
    stop("Failed to stage Step3 model inputs into Step4 run directory: ",
         expected, call. = FALSE)
  }
  expected
}

autoshud_step4_executable_info <- function(path) {
  list(
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    exists = file.exists(path),
    size = if (file.exists(path)) file.info(path)$size else NA_real_,
    md5 = if (file.exists(path)) unname(tools::md5sum(path)) else NA_character_,
    version = NA_character_
  )
}

autoshud_step4_write_metadata <- function(result, file) {
  env_lines <- paste0("env.", names(result$environment), "\t",
                      unname(result$environment))
  git_status <- result$shud_source_git$status %||% NA_character_
  lines <- c(
    paste0("prjname\t", result$prjname),
    paste0("shud_source\t", result$shud_source),
    paste0("shud_source_commit\t", result$shud_source_git$commit),
    paste0("shud_source_status\t",
           if (!is.na(git_status) && nzchar(git_status)) {
             gsub("\n", "; ", git_status, fixed = TRUE)
           } else {
             "clean"
           }),
    paste0("run_dir\t", result$run_dir),
    paste0("model_input_dir\t", result$model_input_dir),
    paste0("output_dir\t", result$output_dir),
    paste0("logs_dir\t", result$logs_dir),
    paste0("staged_executable\t", result$executable$path),
    paste0("staged_executable_size\t", result$executable$size),
    paste0("staged_executable_md5\t", result$executable$md5),
    paste0("staged_executable_version\t", result$executable$version),
    env_lines,
    autoshud_step4_command_to_lines(result$make_clean, "make_clean"),
    autoshud_step4_command_to_lines(result$make_shud, "make_shud"),
    autoshud_step4_command_to_lines(result$solver, "solver"),
    paste0("required_outputs\t", paste(names(result$required_outputs), collapse = ","))
  )
  autoshud_step4_write_lines(lines, file)
}

autoshud_step4_run_case <- function(prjname,
                                    model_input_dir,
                                    run_dir,
                                    shud_source = NULL,
                                    xfg = NULL,
                                    repo_root = autoshud_step4_default_repo_root(),
                                    build_timeout = 600,
                                    run_timeout = 1800,
                                    threads = 1L,
                                    environment = NULL,
                                    required_outputs = AUTOSHUD_STEP4_REQUIRED_OUTPUTS,
                                    configure_step5_outputs = TRUE,
                                    step5_output_interval = 1440,
                                    step5_output_end_day = NULL,
                                    step5_max_solver_step = NULL,
                                    stage_inputs = FALSE,
                                    logs_dir = NULL) {
  prjname <- autoshud_step4_validate_prjname(prjname)
  source_dir <- autoshud_step4_resolve_shud_source(
    shud_source = shud_source, xfg = xfg, repo_root = repo_root
  )
  model_input_dir <- normalizePath(model_input_dir, winslash = "/",
                                   mustWork = TRUE)
  autoshud_step4_check_required_inputs(model_input_dir, prjname)

  run_dir <- normalizePath(run_dir, winslash = "/", mustWork = FALSE)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  expected_input <- normalizePath(file.path(run_dir, "input", prjname),
                                  winslash = "/", mustWork = FALSE)
  autoshud_step4_assert_path_inside(expected_input, run_dir,
                                    "Step4 model input directory")
  if (isTRUE(stage_inputs)) {
    model_input_dir <- autoshud_step4_stage_inputs(model_input_dir, run_dir, prjname)
  } else {
    if (!identical(normalizePath(model_input_dir, winslash = "/", mustWork = FALSE),
                   expected_input)) {
      stop("Step4 run_dir must contain the selected model input at input/",
           prjname, " or stage_inputs must be TRUE. run_dir: ", run_dir,
           "; model_input_dir: ", model_input_dir, call. = FALSE)
    }
  }
  if (isTRUE(configure_step5_outputs)) {
    autoshud_step4_configure_required_outputs(
      model_input_dir = model_input_dir,
      prjname = prjname,
      end_day = step5_output_end_day,
      output_interval = step5_output_interval,
      max_solver_step = step5_max_solver_step
    )
  }

  logs_dir <- logs_dir %||% file.path(run_dir, "autoshud-step4-logs", prjname)
  logs_dir <- normalizePath(logs_dir, winslash = "/", mustWork = FALSE)
  autoshud_step4_assert_path_inside(logs_dir, run_dir, "Step4 logs_dir")
  dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(file.path(run_dir, "output", paste0(prjname, ".out")),
                              winslash = "/", mustWork = FALSE)
  autoshud_step4_assert_path_inside(output_dir, run_dir, "Step4 output_dir")
  staged_exe <- normalizePath(file.path(run_dir, "shud"), winslash = "/",
                              mustWork = FALSE)
  autoshud_step4_assert_path_inside(staged_exe, run_dir,
                                    "Step4 staged executable")
  make_clean_stdout <- file.path(logs_dir, "make-clean.stdout.log")
  make_clean_stderr <- file.path(logs_dir, "make-clean.stderr.log")
  make_shud_stdout <- file.path(logs_dir, "make-shud.stdout.log")
  make_shud_stderr <- file.path(logs_dir, "make-shud.stderr.log")
  solver_stdout <- file.path(logs_dir, "solver.stdout.log")
  solver_stderr <- file.path(logs_dir, "solver.stderr.log")
  make_shud_unrun <- autoshud_step4_unrun_command(
    "make", "shud", wd = source_dir, stdout = make_shud_stdout,
    stderr = make_shud_stderr, timeout = build_timeout
  )
  solver_unrun <- autoshud_step4_unrun_command(
    "./shud", prjname, wd = run_dir, stdout = solver_stdout,
    stderr = solver_stderr, timeout = run_timeout
  )
  env <- autoshud_step4_command_env(extra = environment, threads = threads)
  env_named <- stats::setNames(sub("^[^=]+=", "", env), sub("=.*$", "", env))
  git_info <- autoshud_step4_git_info(source_dir)

  make_clean <- autoshud_step4_run_logged_command(
    "make", "clean", wd = source_dir,
    stdout = make_clean_stdout,
    stderr = make_clean_stderr,
    timeout = build_timeout, env = env
  )
  if (!identical(make_clean$exit_status, 0L)) {
    result <- list(
      prjname = prjname, shud_source = source_dir, shud_source_git = git_info,
      run_dir = run_dir, model_input_dir = model_input_dir,
      output_dir = output_dir,
      logs_dir = logs_dir, environment = env_named,
      make_clean = make_clean,
      make_shud = make_shud_unrun,
      solver = solver_unrun,
      executable = autoshud_step4_executable_info(staged_exe),
      required_outputs = character()
    )
    autoshud_step4_write_metadata(result, file.path(logs_dir, "step4-metadata.tsv"))
    stop("make clean failed for SHUD source. See log: ",
         make_clean$stderr, call. = FALSE)
  }

  make_shud <- autoshud_step4_run_logged_command(
    "make", "shud", wd = source_dir,
    stdout = make_shud_stdout,
    stderr = make_shud_stderr,
    timeout = build_timeout, env = env
  )
  if (!identical(make_shud$exit_status, 0L)) {
    result <- list(
      prjname = prjname, shud_source = source_dir, shud_source_git = git_info,
      run_dir = run_dir, model_input_dir = model_input_dir,
      output_dir = output_dir,
      logs_dir = logs_dir, environment = env_named,
      make_clean = make_clean, make_shud = make_shud,
      solver = solver_unrun,
      executable = autoshud_step4_executable_info(staged_exe),
      required_outputs = character()
    )
    autoshud_step4_write_metadata(result, file.path(logs_dir, "step4-metadata.tsv"))
    stop("make shud failed for SHUD source. See log: ",
         make_shud$stderr, call. = FALSE)
  }

  built_exe <- file.path(source_dir, "shud")
  if (!file.exists(built_exe) || file.info(built_exe)$size <= 0) {
    stop("make shud completed but did not produce a non-empty executable: ",
         built_exe, call. = FALSE)
  }
  if (!file.copy(built_exe, staged_exe, overwrite = TRUE, copy.date = TRUE)) {
    stop("Failed to stage freshly built shud executable into run directory: ",
         staged_exe, call. = FALSE)
  }
  Sys.chmod(staged_exe, mode = "0755", use_umask = FALSE)

  solver <- autoshud_step4_run_logged_command(
    "./shud", prjname, wd = run_dir,
    stdout = solver_stdout,
    stderr = solver_stderr,
    timeout = run_timeout, env = env
  )
  if (!identical(solver$exit_status, 0L)) {
    result <- list(
      prjname = prjname, shud_source = source_dir, shud_source_git = git_info,
      run_dir = run_dir, model_input_dir = model_input_dir,
      output_dir = output_dir, logs_dir = logs_dir, environment = env_named,
      make_clean = make_clean, make_shud = make_shud, solver = solver,
      executable = autoshud_step4_executable_info(staged_exe),
      required_outputs = character()
    )
    autoshud_step4_write_metadata(result, file.path(logs_dir, "step4-metadata.tsv"))
    stop("SHUD solver execution failed. See log: ",
         solver$stderr, call. = FALSE)
  }

  outputs <- autoshud_step4_check_required_outputs(output_dir, prjname,
                                                   required_outputs)
  result <- list(
    prjname = prjname,
    shud_source = source_dir,
    shud_source_git = git_info,
    run_dir = run_dir,
    model_input_dir = model_input_dir,
    output_dir = output_dir,
    logs_dir = logs_dir,
    environment = env_named,
    make_clean = make_clean,
    make_shud = make_shud,
    solver = solver,
    executable = autoshud_step4_executable_info(staged_exe),
    required_outputs = outputs
  )
  autoshud_step4_write_metadata(result, file.path(logs_dir, "step4-metadata.tsv"))
  result
}
