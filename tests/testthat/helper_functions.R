

stype_tester <- function(v_type, canonical_type, generator, error_generators){
  
  # Generate two vectors
  # TODO: I'm not in love with this approach of passing a list (for types with
  # mulitple inputs or a function
  if(is.list(generator)){
    g1 <- g2 <- generator
  } else {
    g1 <- list(generator())
    g2 <- list(generator())
  }
  
  x1 <- do.call(v_type, args = g1)
  x2 <- do.call(v_type, args = g2)
  
  ctxt1 <- context(short_label = "test label", long_label = "test label")
  ctxt2 <- context(short_label = "diff label", long_label = "diff label")
  x1c <- do.call(v_type, args = append(g1, list(internal_name = "test", 
                                                context = ctxt1)))
  x2c <- do.call(v_type, args = append(g2, list(internal_name = "test", 
                                                context = ctxt2)))
  
  test_that(
    sprintf("%s inheritance is correct", v_type),
    {
      ### Check inheritance of v_type
      expect_true(inherits(x1, v_type))
      # after subsetting
      expect_true(inherits(x1[1:2], v_type))
      # after vec_c
      expect_true(inherits(vctrs::vec_c(x1, x2), v_type))
      # after c
      expect_true(inherits(c(x1, x2), v_type))
    })
  
  if (length(error_generators) > 0){
    test_that(
      sprintf("%s should give error on bad inputs", v_type),
      {
        ### Check that errors occur when given bad inputs
        purrr::walk(
          .x = error_generators,
          .f = function(f){
            dat <- f()
            expect_error(
              do.call(v_type, args = list(dat)),
              info = sprintf("%s did not give error with inputs: %s",
                             v_type,
                             paste(dat, collapse = ", ")))
          } 
        )
      })
  }
  
  test_that(
    sprintf("%s should retain attributes", v_type),
    {
      ### Check that all expected attributes are retained
      expected_attrs <- c("internal_name", "data_summary", "context")  
      expect_true(all(expected_attrs %in% names(attributes(x1))))
      expect_true(all(expected_attrs %in% names(attributes(x1[1:2]))))
      expect_true(all(expected_attrs %in% names(attributes(c(x1, x2)))))
    })
  
  test_that(
    sprintf("as_canonical(%s) should return %s", v_type, canonical_type),
    {  
      ### Check as_canonical returns the right type
      expect_is(as_canonical(x1), canonical_type)
    }
  )
  
  test_that(
    sprintf("contexts are retained in %s", v_type),
    {  
      # Check context is there
      ctxt <- get_context(x1c)
      expect_is(ctxt, "context")
      
      # Check context elements are as expected
      expect_equal(get_short_label(x1c), "test label")
      expect_equal(get_short_label(x1c[1:2]), "test label")
      
      # Check contexts are handled correctly when combining vectors
      expect_equal(get_short_label(c(x1c, x1c)), "test label")
      expect_error(c(x1c, x2c))
    }
  )
 
  test_that(
    sprintf("%s can be cast to a character", v_type),
    {  
      expect_is(as.character(x1), "character")
    }
  ) 
  
  test_that("get_internal_name should work",
    {  
      expect_equal(get_internal_name(x1c), "test")
    }
  ) 
  
}