add_executable(lexer_test lexer_test.c)

target_link_libraries(lexer_test PUBLIC check pthread lexer)

target_include_directories(lexer_test PUBLIC "${PROJECT_BINARY_DIR}")

add_test(NAME lexer_test COMMAND lexer_test WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Testing)
set_tests_properties(lexer_test PROPERTIES TIMEOUT 30)
