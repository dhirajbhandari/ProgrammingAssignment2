
assertVectorEquals <- function(actual, expected) {
  result = assertNumericEquals(length(actual), length(expected))
  if (result) {
    index = 1
    for (item in expected) {
      a <- actual[index]
      msg <- paste0("item at: ", index, " ", compareMessage(a, item))
      result = result & assertTrue(actual[index] == item, msg)
      index = index + 1
    }
  }
  result
}

assertNumericEquals <- function(actual, expected) {
  assertTrue(actual == expected, compareMessage(actual, expected))
}

assertLogicalEquals <- function(actual, expected) {
  assertTrue(actual == expected, compareMessage(actual, expected))  
}
assertTrue <- function(condition, msg = compareMessage(FALSE, TRUE)) {
  if (!condition) {
    failAssert(msg)
    return(FALSE)
  }
  TRUE
}

compareMessage <- function(actual, expected) {
  paste0("actual: ", actual, " expected:", expected)
}

failAssert <- function(msg) {
  print(paste0("FAILED:: ", msg))
  #error(msg)
  return(FALSE)
}

runTestSuite <- function(testSuite) {
  result = TRUE
  for(test in testSuite) {
    result <- result & test()
  }
  if (result) {
    print("TestSuite PASSED")
  } else {
    print("TestSuite FAILED")
  }
}
