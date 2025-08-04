#' @title Helper functions for model selection and configuration
#' @description Functions to help users discover and configure available models
#' across different LLM providers through ellmer.

#' List all available models across providers
#' @param provider Optional character string to filter by provider ("openai", "anthropic", "google").
#'   If NULL, returns models from all available providers.
#' @return Data frame with columns: provider, model, full_name (provider/model format)
#' @export
#' @examples
#' # List all available models
#' \donttest{
#' models <- list_available_models()
#' head(models)
#' }
#' 
#' # List only OpenAI models  
#' \donttest{
#' openai_models <- list_available_models("openai")
#' }
#' 
#' # Examples of expected structure (without API calls)
#' # The function returns a data frame with columns:
#' # - provider: "openai", "anthropic", "google"
#' # - model: model name like "gpt-4o-mini", "claude-3-sonnet"
#' # - full_name: "provider/model" format for cross-provider use
list_available_models <- function(provider = NULL) {
  
  # Get available providers
  available_providers <- if (requireNamespace("ellmer", quietly = TRUE)) {
    get_ellmer_providers()
  } else {
    "openai"  # Fallback to OpenAI only if ellmer not available
  }
  
  if (!is.null(provider)) {
    if (!provider %in% available_providers) {
      stop("Provider '", provider, "' is not available. Available providers: ", 
           paste(available_providers, collapse = ", "))
    }
    available_providers <- provider
  }
  
  # Build model list
  model_list <- list()
  
  for (prov in available_providers) {
    if (prov == "openai" && !requireNamespace("ellmer", quietly = TRUE)) {
      # Fallback to traditional models if ellmer not available
      # Safely access model_prizes with existence and structure checks
      models <- tryCatch({
        if (exists("model_prizes", envir = .GlobalEnv) && 
            is.data.frame(model_prizes) && 
            "model" %in% names(model_prizes)) {
          # Filter out o1 models that may not work with current setup
          available_models <- model_prizes$model
          available_models[!available_models %in% c("o1-preview", "o1-mini")]
        } else {
          # Fallback if model_prizes not available
          c("gpt-4o", "gpt-4o-mini", "gpt-4", "gpt-4-turbo", "gpt-3.5-turbo")
        }
      }, error = function(e) {
        # Ultimate fallback
        c("gpt-4o-mini", "gpt-4o")
      })
    } else {
      models <- tryCatch({
        get_ellmer_models(prov)
      }, error = function(e) {
        character(0)
      })
    }
    
    if (length(models) > 0) {
      model_list[[prov]] <- tibble::tibble(
        provider = prov,
        model = models,
        full_name = paste(prov, models, sep = "/")
      )
    }
  }
  
  # Combine all models
  if (length(model_list) > 0) {
    result <- dplyr::bind_rows(model_list)
    
    # Add backward compatibility entries for OpenAI models
    if ("openai" %in% result$provider) {
      openai_models <- result[result$provider == "openai", ]
      openai_models$full_name <- openai_models$model  # Allow bare model names
      result <- dplyr::bind_rows(result, openai_models)
    }
    
    result
  } else {
    tibble::tibble(
      provider = character(0),
      model = character(0),
      full_name = character(0)
    )
  }
}

#' Get recommended models for systematic review screening
#' @return Character vector of recommended model names
#' @export
#' @examples
#' # Get recommended models for systematic reviews
#' \donttest{
#' recommended <- get_recommended_models()
#' print(recommended)
#' }
#' 
#' # Example of typical output:
#' # [1] "gpt-4o-mini"                        "gpt-4o"                           
#' # [3] "anthropic/claude-3-5-sonnet-20241022" "anthropic/claude-3-haiku-20240307"
#' # [5] "google/gemini-1.5-flash"
get_recommended_models <- function() {
  recommended <- c(
    "gpt-4o-mini",  # Best value for money (OpenAI)
    "gpt-4o",       # High performance (OpenAI)
    "anthropic/claude-3-5-sonnet-20241022",  # High performance (Anthropic)
    "anthropic/claude-3-haiku-20240307",     # Cost-effective (Anthropic)
    "google/gemini-1.5-flash"                # Cost-effective (Google)
  )
  
  # Filter to only available models
  available <- list_available_models()
  if (nrow(available) > 0) {
    intersect(recommended, c(available$full_name, available$model))
  } else {
    c("gpt-4o-mini", "gpt-4o")  # Fallback
  }
}

#' Show model comparison table
#' @param include_pricing Logical indicating whether to include pricing information.
#'   Only works for OpenAI models when not using ellmer.
#' @return Data frame with model comparison information
#' @export
#' @examples
#' # Show comparison of available models
#' \donttest{
#' comparison <- show_model_comparison()
#' print(comparison)
#' }
#' 
#' # Show comparison with pricing (for OpenAI models)
#' \donttest{
#' comparison_with_pricing <- show_model_comparison(include_pricing = TRUE)
#' }
#' 
#' # Expected columns in output:
#' # - provider: LLM provider name
#' # - model: Model name
#' # - full_name: Full provider/model specification
#' # - recommended: Whether model is recommended for screening
#' # - use_case: Suggested use case for the model
show_model_comparison <- function(include_pricing = FALSE) {
  
  models <- list_available_models()
  
  if (nrow(models) == 0) {
    return(tibble::tibble())
  }
  
  # Add metadata about models
  models$recommended <- models$full_name %in% get_recommended_models() | 
                       models$model %in% get_recommended_models()
  
  models$use_case <- dplyr::case_when(
    stringr::str_detect(models$model, "gpt-4o-mini|claude-3-haiku|gemini-1.5-flash") ~ "Cost-effective screening",
    stringr::str_detect(models$model, "gpt-4o|claude-3-5-sonnet|gemini-1.5-pro") ~ "High-performance screening",
    stringr::str_detect(models$model, "gpt-4") ~ "Premium screening",
    stringr::str_detect(models$model, "gpt-3.5") ~ "Basic screening",
    TRUE ~ "General use"
  )
  
  # Add pricing if requested and available
  if (include_pricing && "openai" %in% models$provider) {
    pricing_data <- tryCatch({
      if (exists("model_prizes", envir = .GlobalEnv) && 
          is.data.frame(model_prizes) && 
          all(c("model", "input_price_1k_token", "output_price_1k_token") %in% names(model_prizes))) {
        model_prizes |>
          dplyr::select(model, input_price_1k_token, output_price_1k_token) |>
          dplyr::rename_with(~paste0("openai_", .), -model)
      } else {
        NULL
      }
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(pricing_data)) {
      models <- models |>
        dplyr::left_join(pricing_data, by = c("model" = "model"))
    }
  }
  
  # Remove duplicates and arrange
  models |>
    dplyr::distinct(provider, model, .keep_all = TRUE) |>
    dplyr::arrange(provider, dplyr::desc(recommended), model) |>
    dplyr::select(provider, model, full_name, recommended, use_case, dplyr::everything())
}