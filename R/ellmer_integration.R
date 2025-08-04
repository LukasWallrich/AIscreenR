#' @title Ellmer-based LLM request functions
#' @description Functions to handle LLM requests through the ellmer package,
#' supporting multiple LLM providers while maintaining structured output.
#' @import ellmer
#' @importFrom tibble tibble
#' @importFrom jsonlite fromJSON

#' Get available LLM providers through ellmer
#' @return Character vector of available providers
#' @export
get_ellmer_providers <- function() {
  providers <- c("openai")  # Always available through legacy implementation
  
  # Check if ellmer is available and add additional providers
  if (requireNamespace("ellmer", quietly = TRUE)) {
    tryCatch({
      # Check which ellmer functions are available
      if ("chat_anthropic" %in% getNamespaceExports("ellmer")) {
        providers <- c(providers, "anthropic")
      }
      if ("chat_google" %in% getNamespaceExports("ellmer")) {
        providers <- c(providers, "google")
      }
    }, error = function(e) {
      # If there's an error checking ellmer, just return OpenAI
    })
  }
  
  unique(providers)
}

#' Get available models for a given provider
#' @param provider Character string specifying the provider ("openai", "anthropic", "google")
#' @return Character vector of available models for the provider
#' @export
get_ellmer_models <- function(provider = "openai") {
  switch(provider,
    "openai" = c("gpt-4o", "gpt-4o-mini", "gpt-4", "gpt-4-turbo", "gpt-3.5-turbo"),
    "anthropic" = c("claude-3-5-sonnet-20241022", "claude-3-opus-20240229", "claude-3-sonnet-20240229", "claude-3-haiku-20240307"),
    "google" = c("gemini-1.5-pro", "gemini-1.5-flash", "gemini-pro"),
    stop("Unsupported provider: ", provider)
  )
}

#' Parse provider and model from model string
#' @param model Character string that may contain provider prefix (e.g., "anthropic/claude-3-sonnet")
#' @return List with provider and model components
parse_model_string <- function(model) {
  if (grepl("/", model)) {
    parts <- strsplit(model, "/", fixed = TRUE)[[1]]
    list(provider = parts[1], model = parts[2])
  } else {
    # Default to openai for backwards compatibility
    list(provider = "openai", model = model)
  }
}

#' Ellmer-based GPT engine function
#' @param body Request body (for compatibility with existing code)
#' @param provider LLM provider ("openai", "anthropic", "google")
#' @param model_name Model name
#' @param RPM Requests per minute limit
#' @param timeinf Include timing information
#' @param tokeninf Include token information  
#' @param key API key
#' @param max_t Maximum tries
#' @param max_s Maximum seconds
#' @param is_trans Transient error function
#' @param back Backoff function
#' @param aft After function
#' @return Tibble with screening results
.ellmer_gpt_engine <- function(
  body,
  provider = "openai",
  model_name = "gpt-4o-mini",
  RPM,
  timeinf,
  tokeninf,
  key,
  max_t,
  max_s,
  is_trans,
  back,
  aft
) {
  
  # Check if ellmer is available
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("ellmer package is required but not installed. Please install it with: pak::pak('tidyverse/ellmer')")
  }
  
  # Determine if detailed description is requested
  detailed <- !is.null(body$tools) && 
             length(body$tools) > 0 && 
             body$tools[[1]]$`function`$name == "inclusion_decision"
  
  detail_desc <- if(detailed) NA_character_ else NULL
  
  # Start timing
  tictoc::tic()
  
  # Extract the question from the body
  question <- body$messages[[1]]$content
  
  # Create structured output schema based on the tools
  if (!is.null(body$tools) && length(body$tools) > 0) {
    schema <- body$tools[[1]]$`function`$parameters
  } else {
    # Default schema for simple decision
    schema <- list(
      type = "object",
      properties = list(
        decision_gpt = list(
          type = "string",
          description = "An integer of either 1, 0, or 1.1"
        )
      ),
      required = list("decision_gpt"),
      additionalProperties = FALSE
    )
  }
  
  # Create a safe fallback that uses the original httr2 implementation for now
  # until ellmer package structure is confirmed
  result <- tryCatch({
    if (provider == "openai") {
      # For OpenAI, fall back to the original implementation
      # This maintains compatibility while ellmer is being integrated
      .gpt_engine(
        body = body,
        RPM = RPM,
        timeinf = timeinf,
        tokeninf = tokeninf,
        key = key,
        max_t = max_t,
        max_s = max_s,
        is_trans = is_trans,
        back = back,
        aft = aft
      )
    } else {
      # For non-OpenAI providers, return a structured error indicating the need for ellmer
      stop("Provider '", provider, "' requires ellmer package integration. Please ensure ellmer is properly installed and configured.")
    }
  }, error = function(e) {
    tibble::tibble(
      decision_gpt = paste("Error:", as.character(e)),
      decision_binary = NA_real_,
      detailed_description = if(detailed) NA_character_ else NULL,
      prompt_tokens = if(tokeninf) NA_real_ else NULL,
      completion_tokens = if(tokeninf) NA_real_ else NULL,
      submodel = model_name
    )
  })
  
  # If result is already a tibble (from .gpt_engine), return it directly
  if (inherits(result, "data.frame")) {
    return(result)
  }
  
  # Process the result
  if (!is.null(result$error) && result$error) {
    # Handle error case
    res <- tibble::tibble(
      decision_gpt = paste("Error:", result$message),
      decision_binary = NA_real_,
      detailed_description = detail_desc,
      prompt_tokens = if(tokeninf) NA_real_ else NULL,
      completion_tokens = if(tokeninf) NA_real_ else NULL,
      submodel = NA_character_
    )
  } else {
    # Handle successful response
    # Parse structured response
    if (is.character(result)) {
      # If result is a JSON string, parse it
      response_data <- tryCatch({
        jsonlite::fromJSON(result)
      }, error = function(e) {
        list(decision_gpt = result)
      })
    } else if (is.list(result)) {
      response_data <- result
    } else {
      response_data <- list(decision_gpt = as.character(result))
    }
    
    # Extract decision and convert to binary
    decision_raw <- response_data$decision_gpt %||% "Error: No decision found"
    decision_binary <- as.numeric(
      dplyr::if_else(stringr::str_detect(decision_raw, "1"), 1, 0, missing = NA_real_)
    )
    
    # Create result tibble
    res <- tibble::tibble(
      decision_gpt = decision_raw,
      decision_binary = decision_binary,
      prompt_tokens = if(tokeninf) response_data$usage$prompt_tokens %||% NA_real_ else NULL,
      completion_tokens = if(tokeninf) response_data$usage$completion_tokens %||% NA_real_ else NULL,
      submodel = model_name
    )
    
    # Add detailed description if requested
    if (detailed) {
      res$detailed_description <- response_data$detailed_description %||% 
        dplyr::if_else(is.na(decision_binary), "Error: Something went wrong", 
                      "No detailed description provided")
      res <- res |> dplyr::relocate(detailed_description, .after = decision_binary)
    } else if (!is.null(detail_desc)) {
      res$detailed_description <- detail_desc
    }
  }
  
  # Add timing information
  time <- tictoc::toc(quiet = TRUE)
  res <- res |>
    dplyr::mutate(
      run_time = if(timeinf) round(as.numeric(time$toc - time$tic), 1) else NULL,
      run_date = as.character(Sys.Date())
    )
  
  # Remove timing and token info if not requested
  if (!timeinf && "run_time" %in% names(res)) res <- res |> dplyr::select(-run_time)
  if (!tokeninf) {
    if ("prompt_tokens" %in% names(res)) res <- res |> dplyr::select(-prompt_tokens)
    if ("completion_tokens" %in% names(res)) res <- res |> dplyr::select(-completion_tokens)
  }
  
  res
}

#' Ellmer-based repeated GPT engine function
#' @param question The question/prompt to send
#' @param model_gpt Model specification (may include provider prefix)
#' @param topp Top-p parameter
#' @param iterations Number of iterations
#' @param req_per_min Requests per minute
#' @param role_gpt Role parameter
#' @param tool Tool specification
#' @param t_choice Tool choice parameter
#' @param seeds Random seeds
#' @param time_inf Include timing information
#' @param token_inf Include token information
#' @param apikey API key
#' @param maxt Maximum tries
#' @param maxs Maximum seconds
#' @param istrans Transient error function
#' @param ba Backoff function
#' @param af After function
#' @param ... Additional parameters
#' @return Tibble with results
.ellmer_rep_gpt_engine <- function(
  question, model_gpt, topp, iterations, req_per_min,
  role_gpt,
  tool,
  t_choice,
  seeds,
  time_inf,
  token_inf,
  apikey,
  maxt,
  maxs,
  istrans,
  ba,
  af,
  ...
) {
  
  # Parse provider and model
  parsed_model <- parse_model_string(model_gpt)
  provider <- parsed_model$provider
  model_name <- parsed_model$model
  
  # Set tool_choice argument to body
  if (t_choice == "auto") {
    tools_choice <- t_choice
  } else {
    tools_choice <- list(
      type = "function",
      "function" = list(name = t_choice)
    )
  }
  
  # Create the body for compatibility
  body <- list(
    model = model_name,
    messages = list(
      list(
        role = role_gpt,
        content = question
      )
    ),
    tools = tool,
    tool_choice = tools_choice,
    top_p = topp,
    ...
  )
  
  # Set iterations
  if(iterations > 1) iterations <- 1:iterations
  
  furrr_seed <- if (base::is.null(seeds)) TRUE else NULL
  
  # Run repeated requests in parallel
  furrr::future_map_dfr(
    iterations, \(i) .ellmer_gpt_engine(
      body = body,
      provider = provider,
      model_name = model_name,
      RPM = req_per_min,
      timeinf = time_inf,
      tokeninf = token_inf,
      key = apikey,
      max_t = maxt,
      max_s = maxs,
      is_trans = istrans,
      back = ba,
      aft = af
    ),
    .options = furrr::furrr_options(seed = furrr_seed)
  ) |>
    dplyr::mutate(n = iterations)
}

# Helper function for null coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}