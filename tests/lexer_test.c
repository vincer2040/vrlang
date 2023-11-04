#include "../src/lexer.h"
#include "../src/token.h"
#include <check.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define size(arr) ((sizeof arr) / (sizeof arr[0]))

#define assert_tok_eq(a, b)                                                    \
    do {                                                                       \
        ck_assert_uint_eq(a.type, b.type);                                     \
        if (a.value == NULL) {                                                 \
            ck_assert_ptr_null(b.value);                                       \
        } else {                                                               \
            ck_assert_ptr_nonnull(b.value);                                    \
            ck_assert_mem_eq(a.value, b.value, strlen(a.value));               \
        }                                                                      \
    } while (0)

START_TEST(test_next_token) {
    const char* input = "let five = 5;\n\
let ten = 10;\n\
let add = fn(x, y) {\n\
x + y;\n\
};\n\
let result = add(five, ten);\
!-/*5;\n\
5 < 10 > 5;\n\
if (5 < 10) {\n\
    return true;\n\
} else {\n\
    return false;\n\
}\n\
10 == 10;\n\
10 != 9;\
";
    size_t input_len = strlen(input);
    Lexer l = lexer_new(input, input_len);
    Token exp_toks[] = {
        {Let, NULL},       {Ident, "five"},   {Assign, NULL},
        {Int, "5"},        {Semicolon, NULL}, {Let, NULL},
        {Ident, "ten"},    {Assign, NULL},    {Int, "10"},
        {Semicolon, NULL}, {Let, NULL},       {Ident, "add"},
        {Assign, NULL},    {Function, NULL},  {LParen, NULL},
        {Ident, "x"},      {Comma, NULL},     {Ident, "y"},
        {RParen, NULL},    {LSquirly, NULL},  {Ident, "x"},
        {Plus, NULL},      {Ident, "y"},      {Semicolon, NULL},
        {RSquirly, NULL},  {Semicolon, NULL}, {Let, NULL},
        {Ident, "result"}, {Assign, NULL},    {Ident, "add"},
        {LParen, NULL},    {Ident, "five"},   {Comma, NULL},
        {Ident, "ten"},    {RParen, NULL},    {Semicolon, NULL},
        {Bang, NULL},      {Minus, NULL},     {Slash, NULL},
        {Asterisk, NULL},  {Int, "5"},        {Semicolon, NULL},
        {Int, "5"},        {Lt, NULL},        {Int, "10"},
        {Gt, NULL},        {Int, "5"},        {Semicolon, NULL},
        {If, NULL},        {LParen, NULL},    {Int, "5"},
        {Lt, NULL},        {Int, "10"},       {RParen, NULL},
        {LSquirly, NULL},  {Return, NULL},    {True, NULL},
        {Semicolon, NULL}, {RSquirly, NULL},  {Else, NULL},
        {LSquirly, NULL},  {Return, NULL},    {False, NULL},
        {Semicolon, NULL}, {RSquirly, NULL},  {Int, "10"},
        {Eq, NULL},        {Int, "10"},       {Semicolon, NULL},
        {Int, "10"},       {NotEq, NULL},     {Int, "9"},
        {Semicolon, NULL}, {Eoft, NULL},
    };

    size_t i, len = size(exp_toks);

    for (i = 0; i < len; ++i) {
        Token cur = lexer_next_token(&l);
        Token exp = exp_toks[i];
        assert_tok_eq(exp, cur);
        token_free(&cur);
    }
}
END_TEST

Suite* suite() {
    Suite* s;
    TCase* tc_core;
    s = suite_create("lexer test");
    tc_core = tcase_create("Core");
    tcase_add_test(tc_core, test_next_token);
    suite_add_tcase(s, tc_core);
    return s;
}

int main() {
    int number_failed;
    Suite* s;
    SRunner* sr;
    s = suite();
    sr = srunner_create(s);
    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
