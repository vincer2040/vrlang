cmake_minimum_required(VERSION 3.10)

project(vrlang VERSION 0.1)

enable_testing()

add_subdirectory(tests)
set(INSTALL_PREFIX "/usr/local")

set(CMAKE_C_COMPILER "gcc")

set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -std=c99 -Wall -Werror -pedantic -fstack-clash-protection -fstack-protector-all -fstack-protector-strong -Werror=format-security -Werror=implicit-function-declaration -pipe -O2")

set(CMAKE_C_FLAGS_RELEASE "-std=c99 -Wall -Werror -pedantic -fstack-clash-protection -fstack-protector-all -fstack-protector-strong -Werror=format-security -Werror=implicit-function-declaration -pipe -O2 -s -DNDEBUG")

add_library(
    vstr
    src/vstr.c
)

add_library(
    lexer
    src/token.c
    src/lexer.c
)

add_library(
    ast
    src/ast.c
)

add_library(
    parser
    src/parser.c
)

target_link_libraries(
    lexer
    vstr
)

target_link_libraries(
    parser
    lexer
    ast
    vstr
)

add_executable(vrlang src/vrlang.c)

install(FILES build/vrlang DESTINATION "${INSTALL_PREFIX}/bin")
