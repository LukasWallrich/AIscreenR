#' @title Helper functions for model selection and configuration
#' @description Functions to help users discover and configure available models
#' across different LLM providers through ellmer.

#' List all available models across providers
#' @param provider Optional character string to filter by provider ("openai", "anthropic", "google").
#'   If NULL, returns models from all available providers.
#' @return Data frame with columns: provider, model, full_name (provider/model format)
#' @export
#' @examples
#' \dontrun{
#' # List all available models
#' list_available_models()
#' 
#' # List only OpenAI models  
#' list_available_models("openai")
#' 
#' # List only Anthropic models
#' list_available_models("anthropic")
#' }
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
      models <- model_prizes$model[model_prizes$model != "o1-preview" & model_prizes$model != "o1-mini"]
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
#' \dontrun{
#' get_recommended_models()
#' }
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
#' \dontrun{
#' show_model_comparison()
#' }
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
    pricing_data <- model_prizes |>
      dplyr::select(model, input_price_1k_token, output_price_1k_token) |>
      dplyr::rename_with(~paste0("openai_", .), -model)
    
    models <- models |>
      dplyr::left_join(pricing_data, by = c("model" = "model"))
  }
  
  # Remove duplicates and arrange
  models |>
    dplyr::distinct(provider, model, .keep_all = TRUE) |>
    dplyr::arrange(provider, dplyr::desc(recommended), model) |>
    dplyr::select(provider, model, full_name, recommended, use_case, dplyr::everything())
}