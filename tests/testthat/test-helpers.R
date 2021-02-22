test_that("list of context inputs returns correctly", {
    
    expect_identical(context_inputs("study")[1],"study_id")
    
    expect_error(context_inputs(123))
    
    expect_error(context_inputs("banana"))
    
}
)

test_that("list of context outputs returns correctly", {
    
    expect_identical(context_outputs("study")[1],"summary")
    
    expect_error(context_outputs(123))
    
    expect_error(context_outputs("banana"))
    
}
)

test_that("input example returns correctly", {
    
    expect_identical(input_example("study_id")[1],"ST000001")
    
    expect_error(input_example(123))
    
    expect_error(input_example("banana"))
    
}
)