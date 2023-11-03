add_executable(vrlang_test vrlang_test.c)

target_link_libraries(vrlang_test PUBLIC check pthread)

target_include_directories(vrlang_test PUBLIC "${PROJECT_BINARY_DIR}")

add_test(NAME vrlang_test COMMAND vrlang_test WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Testing)
set_tests_properties(vrlang_test PROPERTIES TIMEOUT 30)