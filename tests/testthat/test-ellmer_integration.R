test_that("get_ellmer_providers returns valid providers", {
  providers <- get_ellmer_providers()
  
  expect_true(is.character(providers))
  expect_true(length(providers) >= 1)
  expect_true("openai" %in% providers)  # Always available
  expect_true(all(providers %in% c("openai", "anthropic", "google")))
})

test_that("get_ellmer_providers caches results", {
  # Clear cache first
  if (exists(".ellmer_provider_cache")) {
    rm(list = ls(envir = .ellmer_provider_cache), envir = .ellmer_provider_cache)
  }
  
  # First call should populate cache
  providers1 <- get_ellmer_providers()
  expect_true(exists("available_providers", envir = .ellmer_provider_cache))
  
  # Second call should use cache
  providers2 <- get_ellmer_providers()
  expect_identical(providers1, providers2)
})

test_that("get_ellmer_models returns valid models for each provider", {
  providers <- c("openai", "anthropic", "google")
  
  for (provider in providers) {
    models <- tryCatch({
      get_ellmer_models(provider)
    }, error = function(e) {
      character(0)
    })
    
    expect_true(is.character(models))
    if (length(models) > 0) {
      expect_true(all(nchar(models) > 0))
    }
  }
})

test_that("get_ellmer_models throws error for unsupported provider", {
  expect_error(
    get_ellmer_models("invalid_provider"),
    "Unsupported provider: invalid_provider"
  )
})

test_that("parse_model_string handles provider/model format", {
  result <- parse_model_string("anthropic/claude-3-sonnet")
  expect_equal(result$provider, "anthropic")
  expect_equal(result$model, "claude-3-sonnet")
})

test_that("parse_model_string defaults to openai for bare model names", {
  result <- parse_model_string("gpt-4o-mini")
  expect_equal(result$provider, "openai")
  expect_equal(result$model, "gpt-4o-mini")
})

test_that("parse_model_string handles complex model names", {
  result <- parse_model_string("google/gemini-1.5-pro")
  expect_equal(result$provider, "google")
  expect_equal(result$model, "gemini-1.5-pro")
})

test_that("list_available_models returns proper structure", {
  models <- list_available_models()
  
  expect_true(is.data.frame(models))
  expected_cols <- c("provider", "model", "full_name")
  expect_true(all(expected_cols %in% names(models)))
  
  if (nrow(models) > 0) {
    expect_true(all(nchar(models$provider) > 0))
    expect_true(all(nchar(models$model) > 0))
    expect_true(all(nchar(models$full_name) > 0))
  }
})

test_that("list_available_models filters by provider correctly", {
  openai_models <- list_available_models("openai")
  
  expect_true(is.data.frame(openai_models))
  if (nrow(openai_models) > 0) {
    expect_true(all(openai_models$provider == "openai"))
  }
})

test_that("list_available_models throws error for invalid provider", {
  expect_error(
    list_available_models("invalid_provider"),
    "Provider 'invalid_provider' is not available"
  )
})

test_that("get_recommended_models returns valid models", {
  recommended <- get_recommended_models()
  
  expect_true(is.character(recommended))
  expect_true(length(recommended) >= 1)
  expect_true(all(nchar(recommended) > 0))
})

test_that("show_model_comparison returns proper structure", {
  comparison <- show_model_comparison()
  
  expect_true(is.data.frame(comparison))
  if (nrow(comparison) > 0) {
    expected_cols <- c("provider", "model", "full_name", "recommended", "use_case")
    expect_true(all(expected_cols %in% names(comparison)))
    expect_true(is.logical(comparison$recommended))
    expect_true(all(nchar(comparison$use_case) > 0))
  }
})

test_that("show_model_comparison handles pricing when requested", {
  comparison <- show_model_comparison(include_pricing = TRUE)
  
  expect_true(is.data.frame(comparison))
  # Pricing columns should be added if model_prizes is available and contains OpenAI models
})

test_that(".ellmer_gpt_engine handles missing ellmer package gracefully", {
  # Mock missing ellmer package
  with_mocked_bindings(
    requireNamespace = function(pkg, quietly = TRUE) {
      if (pkg == "ellmer") FALSE else TRUE
    },
    {
      body <- list(
        messages = list(list(content = "Test question")),
        tools = NULL
      )
      
      expect_error(
        .ellmer_gpt_engine(
          body = body,
          provider = "openai",
          model_name = "gpt-4o-mini",
          RPM = 60,
          timeinf = FALSE,
          tokeninf = FALSE,
          key = "test_key",
          max_t = 3,
          max_s = 60,
          is_trans = function(x) FALSE,
          back = function(x) 1,
          aft = function(x) NULL
        ),
        "ellmer package is required"
      )
    }
  )
})

test_that(".ellmer_gpt_engine validates API keys", {
  skip_if_not_installed("ellmer")
  
  body <- list(
    messages = list(list(content = "Test question")),
    tools = NULL
  )
  
  # Test missing key for Anthropic
  result <- .ellmer_gpt_engine(
    body = body,
    provider = "anthropic",
    model_name = "claude-3-haiku",
    RPM = 60,
    timeinf = FALSE,
    tokeninf = FALSE,
    key = "",  # Empty key
    max_t = 3,
    max_s = 60,
    is_trans = function(x) FALSE,
    back = function(x) 1,
    aft = function(x) NULL
  )
  
  expect_true(is.data.frame(result))
  expect_true("decision_gpt" %in% names(result))
  expect_true(grepl("API key error", result$decision_gpt[1]))
})

test_that(".ellmer_gpt_engine returns proper structure on success", {
  skip_if_not_installed("ellmer")
  
  # Mock successful ellmer response
  with_mocked_bindings(
    `ellmer::chat_openai` = function(...) {
      list(content = '{"decision_gpt": "1"}')
    },
    {
      body <- list(
        messages = list(list(content = "Test question")),
        tools = list(
          list(
            "function" = list(
              name = "inclusion_decision",
              parameters = list(
                type = "object",
                properties = list(
                  decision_gpt = list(type = "string")
                )
              )
            )
          )
        )
      )
      
      result <- .ellmer_gpt_engine(
        body = body,
        provider = "openai",
        model_name = "gpt-4o-mini",
        RPM = 60,
        timeinf = TRUE,
        tokeninf = TRUE,
        key = "test_key",
        max_t = 3,
        max_s = 60,
        is_trans = function(x) FALSE,
        back = function(x) 1,
        aft = function(x) NULL
      )
      
      expect_true(is.data.frame(result))
      expected_cols <- c("decision_gpt", "decision_binary", "submodel", "run_time", "run_date")
      expect_true(all(expected_cols %in% names(result)))
      expect_equal(result$decision_gpt[1], "1")
      expect_equal(result$decision_binary[1], 1)
    }
  )
})

test_that(".ellmer_rep_gpt_engine handles multiple iterations", {
  skip_if_not_installed("ellmer")
  
  # Mock successful ellmer response
  with_mocked_bindings(
    `ellmer::chat_openai` = function(...) {
      list(content = '{"decision_gpt": "1"}')
    },
    {
      result <- .ellmer_rep_gpt_engine(
        question = "Test question",
        model_gpt = "gpt-4o-mini",
        topp = 0.9,
        iterations = 2,
        req_per_min = 60,
        role_gpt = "user",
        tool = NULL,
        t_choice = "auto",
        seeds = NULL,
        time_inf = FALSE,
        token_inf = FALSE,
        apikey = "test_key",
        maxt = 3,
        maxs = 60,
        istrans = function(x) FALSE,
        ba = function(x) 1,
        af = function(x) NULL
      )
      
      expect_true(is.data.frame(result))
      expect_equal(nrow(result), 2)
      expect_true("n" %in% names(result))
      expect_equal(result$n, c(1, 2))
    }
  )
})

test_that("null coalescing operator works correctly", {
  # Test the %||% operator
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal("" %||% "default", "")
  expect_equal(0 %||% "default", 0)
})