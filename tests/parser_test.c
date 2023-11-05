#include "../src/ast.h"
#include "../src/lexer.h"
#include "../src/parser.h"
#include <check.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define size(arr) ((sizeof arr) / (sizeof arr[0]))

typedef enum {
    Bool,
    Integer,
} LiteralType;

typedef struct {
    const char* exp_ident;
    const char* exp_val_lit;
    int64_t exp_val;
} LetTest;

typedef struct {
    LiteralType type;
    const char* input;
    PrefixOperator oper;
    const char* lit;
    int64_t val;
} PrefixTest;

typedef struct {
    LiteralType type;
    const char* input;
    const char* lval_lit;
    int64_t lval;
    InfixOperator oper;
    const char* rval_lit;
    int64_t rval;
} InfixTest;

typedef struct {
    const char* input;
    const char* exp;
} PrecedenceTest;

typedef struct {
    const char* input;
    const char* exp_lit;
    bool exp;
} BoolTest;

typedef struct {
    const char* exp_lit;
    int64_t exp;
} IntTest;

typedef struct {
    const char* input;
    size_t param_len;
    const char* params[3];
} ParamTest;

void assert_let_statement(Statement* stmt, const char* exp_ident) {
    LetStatement ls;
    vstr ls_tok_lit;
    vstr ident_tok_lit;
    ck_assert_uint_eq(stmt->type, ST_Let);
    ls = stmt->data.let;

    ck_assert_str_eq(ls.name.value, exp_ident);

    ls_tok_lit = statement_token_literal(stmt);
    ck_assert_str_eq(ls_tok_lit, "let");
    vstr_delete(ls_tok_lit);

    ident_tok_lit = identifier_token_literal(&(ls.name));
    ck_assert_str_eq(ident_tok_lit, exp_ident);
    vstr_delete(ident_tok_lit);
}

void assert_ident(Expression* e, const char* exp_name) {
    ck_assert_uint_eq(e->type, ET_Identifier);
    ck_assert_str_eq(e->data.ident.value, exp_name);
}

void assert_integer_literal(Expression* e, const char* exp_lit, int64_t exp) {
    vstr lit;
    ck_assert_uint_eq(e->type, ET_Integer);
    lit = expression_token_literal(e);
    ck_assert_str_eq(lit, exp_lit);
    vstr_delete(lit);
    ck_assert_int_eq(e->data.integer.value, exp);
}

void assert_boolean_literal(Expression* e, const char* exp_lit, bool exp) {
    vstr lit;
    ck_assert_uint_eq(e->type, ET_Boolean);
    lit = expression_token_literal(e);
    ck_assert_str_eq(lit, exp_lit);
    vstr_delete(lit);
    ck_assert_int_eq(e->data.boolean.value, exp);
}

void assert_literal(Expression* e, const char* exp_lit, void* value,
                    LiteralType type) {
    switch (type) {
    case Bool:
        assert_boolean_literal(e, exp_lit, *((bool*)value));
        break;
    case Integer:
        assert_integer_literal(e, exp_lit, *((int64_t*)value));
        break;
    }
}

void check_errors(Parser* p) {
    size_t i, len = p->errors.len;
    if (len) {
        for (i = 0; i < len; ++i) {
            vstr e = p->errors.errors[i];
            printf("%s\n", e);
        }
        ck_assert(0 && "parser had errors");
    }
}

START_TEST(test_let_statement) {
    const char* input = "\
let x = 5;\n\
let y = 10;\n\
let foobar = 838383;\n\
";
    Lexer l = lexer_new(input, strlen(input));
    Parser p = parser_new(&l);
    Program* program = parse(&p);
    LetTest exp_idents[] = {
        {"x", "5", 5},
        {"y", "10", 10},
        {"foobar", "838383", 838383},
    };
    size_t i, len = size(exp_idents);

    check_errors(&p);

    ck_assert_ptr_nonnull(program);
    ck_assert_uint_eq(program->len, 3);

    for (i = 0; i < len; ++i) {
        Statement stmt = program->statements[i];
        Expression e;
        const char* exp = exp_idents[i].exp_ident;
        assert_let_statement(&stmt, exp);

        e = stmt.data.let.value;
        assert_integer_literal(&e, exp_idents[i].exp_val_lit,
                               exp_idents[i].exp_val);
    }

    program_free(program);
}
END_TEST

START_TEST(test_return_statement) {
    const char* input = "\
return 5;\n\
return 10;\n\
return 993322;\n\
";
    Lexer l = lexer_new(input, strlen(input));
    Parser p = parser_new(&l);
    Program* program = parse(&p);
    size_t i, len = 3;
    IntTest tests[] = {
        { "5", 5 },
        { "10", 10 },
        { "993322", 993322 },
    };

    ck_assert_uint_eq(program->len, 3);

    for (i = 0; i < len; ++i) {
        Statement stmt = program->statements[i];
        Expression e;
        vstr ret_lit;
        IntTest test = tests[i];
        ck_assert_uint_eq(stmt.type, ST_Return);

        ret_lit = statement_token_literal(&stmt);
        ck_assert_str_eq(ret_lit, "return");
        vstr_delete(ret_lit);
        e = stmt.data.ret.value;
        assert_integer_literal(&e, test.exp_lit, test.exp);
    }

    program_free(program);
}
END_TEST

START_TEST(test_identifier_expression) {
    const char* input = "foobar;";
    Lexer l = lexer_new(input, strlen(input));
    Parser p = parser_new(&l);
    Program* program = parse(&p);
    Statement stmt;
    Expression ident;
    vstr exp_tok_lit;
    check_errors(&p);

    ck_assert_uint_eq(program->len, 1);

    stmt = program->statements[0];

    ck_assert_uint_eq(stmt.type, ST_Expression);

    ident = stmt.data.es.expression;
    ck_assert_uint_eq(ident.type, ET_Identifier);
    ck_assert_str_eq(ident.data.ident.value, "foobar");
    exp_tok_lit = expression_token_literal(&ident);
    ck_assert_str_eq(exp_tok_lit, "foobar");
    vstr_delete(exp_tok_lit);
    program_free(program);
}
END_TEST

START_TEST(test_integer_expression) {
    const char* input = "5;";
    Lexer l = lexer_new(input, strlen(input));
    Parser p = parser_new(&l);
    Program* program = parse(&p);
    Statement stmt;
    Expression e;

    check_errors(&p);
    ck_assert_uint_eq(program->len, 1);

    stmt = program->statements[0];
    ck_assert_uint_eq(stmt.type, ST_Expression);

    e = stmt.data.es.expression;
    assert_integer_literal(&e, "5", 5);

    program_free(program);
}
END_TEST

START_TEST(test_boolean_expression) {
    BoolTest tests[] = {
        {"true;", "true", true},
        {"false;", "false", false},
    };

    size_t i, len = size(tests);
    for (i = 0; i < len; ++i) {
        BoolTest test = tests[i];
        const char* input = test.input;
        Lexer l = lexer_new(input, strlen(input));
        Parser p = parser_new(&l);
        Program* program = parse(&p);
        Statement stmt;
        Expression e;

        check_errors(&p);
        ck_assert_uint_eq(program->len, 1);
        stmt = program->statements[0];
        ck_assert_uint_eq(stmt.type, ST_Expression);
        e = stmt.data.es.expression;
        assert_boolean_literal(&e, test.exp_lit, test.exp);

        program_free(program);
    }
}
END_TEST

START_TEST(test_prefix_expression) {
    PrefixTest tests[] = {
        {Integer, "!5", PO_Bang, "5", 5},
        {Integer, "-15", PO_Minus, "15", 15},
        {Bool, "!true", PO_Bang, "true", true},
        {Bool, "!false", PO_Bang, "false", false},
    };

    size_t i, len = size(tests);

    for (i = 0; i < len; ++i) {
        PrefixTest test = tests[i];
        const char* input = test.input;
        Lexer l = lexer_new(input, strlen(input));
        Parser p = parser_new(&l);
        Program* program = parse(&p);
        Statement stmt;
        Expression e;

        check_errors(&p);

        ck_assert_uint_eq(program->len, 1);
        stmt = program->statements[0];
        ck_assert_uint_eq(stmt.type, ST_Expression);
        e = stmt.data.es.expression;
        ck_assert_uint_eq(e.type, ET_Prefix);
        ck_assert_uint_eq(e.data.prefix.oper, test.oper);
        assert_literal(e.data.prefix.right, test.lit, &(test.val), test.type);

        program_free(program);
    }
}
END_TEST

START_TEST(test_infix_expression) {
    InfixTest tests[] = {
        {Integer, "5 + 5", "5", 5, IO_Plus, "5", 5},
        {Integer, "5 - 5", "5", 5, IO_Minus, "5", 5},
        {Integer, "5 * 5", "5", 5, IO_Asterisk, "5", 5},
        {Integer, "5 / 5", "5", 5, IO_Slash, "5", 5},
        {Integer, "5 > 5", "5", 5, IO_Gt, "5", 5},
        {Integer, "5 < 5", "5", 5, IO_Lt, "5", 5},
        {Integer, "5 == 5", "5", 5, IO_Eq, "5", 5},
        {Integer, "5 != 5", "5", 5, IO_NotEq, "5", 5},
        {Bool, "true == true", "true", true, IO_Eq, "true", true},
        {Bool, "true != false", "true", true, IO_NotEq, "false", false},
        {Bool, "false == false", "false", false, IO_Eq, "false", false}};

    size_t i, len = size(tests);

    for (i = 0; i < len; ++i) {
        InfixTest test = tests[i];
        const char* input = test.input;
        Lexer l = lexer_new(input, strlen(input));
        Parser p = parser_new(&l);
        Program* program = parse(&p);
        Statement stmt;
        Expression e;
        check_errors(&p);

        ck_assert_uint_eq(program->len, 1);
        stmt = program->statements[0];
        ck_assert_uint_eq(stmt.type, ST_Expression);

        e = stmt.data.es.expression;
        ck_assert_uint_eq(e.type, ET_Infix);
        ck_assert_uint_eq(e.data.infix.oper, test.oper);
        assert_literal(e.data.infix.left, test.lval_lit, &(test.lval),
                       test.type);
        assert_literal(e.data.infix.right, test.rval_lit, &(test.rval),
                       test.type);
        program_free(program);
    }
}
END_TEST

START_TEST(test_operator_precedence) {
    PrecedenceTest tests[] = {
        {"-a * b", "((-a) * b)"},
        {"!-a", "(!(-a))"},
        {"a + b + c", "((a + b) + c)"},
        {"a + b - c", "((a + b) - c)"},
        {"a * b * c", "((a * b) * c)"},
        {"a * b / c", "((a * b) / c)"},
        {"a + b / c", "(a + (b / c))"},
        {"a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"},
        {"3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"},
        {"5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"},
        {"5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"},
        {"3 + 4 * 5 == 3 * 1 + 4 * 5",
         "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
        {"3 + 4 * 5 == 3 * 1 + 4 * 5",
         "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"},
        {"true", "true"},
        {"false", "false"},
        {"3 > 5 == false", "((3 > 5) == false)"},
        {"3 < 5 == true", "((3 < 5) == true)"},
        {"1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"},
        {"(5 + 5) * 2", "((5 + 5) * 2)"},
        {"2 / (5 + 5)", "(2 / (5 + 5))"},
        {"-(5 + 5)", "(-(5 + 5))"},
        {"!(true == true)", "(!(true == true))"},
        {"a + add(b * c) + d", "((a + add((b * c))) + d)"},
        {"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
         "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"},
        {"add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"},
    };

    size_t i, len = size(tests);
    for (i = 0; i < len; ++i) {
        PrecedenceTest test = tests[i];
        const char* input = test.input;
        const char* exp = test.exp;
        Lexer l = lexer_new(input, strlen(input));
        Parser p = parser_new(&l);
        Program* program = parse(&p);
        vstr s;
        check_errors(&p);

        s = program_string(program);
        ck_assert_str_eq(s, exp);
        vstr_delete(s);
    }
}
END_TEST

START_TEST(test_if_expression) {
    const char* input = "if (x < y) { x }";
    Lexer l = lexer_new(input, strlen(input));
    Parser p = parser_new(&l);
    Program* program = parse(&p);
    Statement stmt, conseq_stmt;
    Expression e, conseq_exp;

    check_errors(&p);
    ck_assert_uint_eq(program->len, 1);
    stmt = program->statements[0];
    ck_assert_uint_eq(stmt.type, ST_Expression);
    e = stmt.data.es.expression;
    ck_assert_uint_eq(e.type, ET_If);

    ck_assert_uint_eq(e.data.if_exp.condition->type, ET_Infix);
    assert_ident(e.data.if_exp.condition->data.infix.left, "x");
    ck_assert_uint_eq(e.data.if_exp.condition->data.infix.oper, IO_Lt);
    assert_ident(e.data.if_exp.condition->data.infix.right, "y");

    ck_assert_ptr_nonnull(e.data.if_exp.consequence.statements);
    ck_assert_uint_eq(e.data.if_exp.consequence.statements->len, 1);
    conseq_stmt = e.data.if_exp.consequence.statements->statements[0];
    ck_assert_uint_eq(conseq_stmt.type, ST_Expression);
    conseq_exp = conseq_stmt.data.es.expression;
    assert_ident(&conseq_exp, "x");
    ck_assert_ptr_null(e.data.if_exp.alternative.statements);

    program_free(program);
}
END_TEST

START_TEST(test_if_else) {
    const char* input = "if (x < y) { x } else { y }";
    Lexer l = lexer_new(input, strlen(input));
    Parser p = parser_new(&l);
    Program* program = parse(&p);
    Statement stmt, conseq_stmt, alt_stmt;
    Expression e, conseq_exp, alt_exp;

    check_errors(&p);
    ck_assert_uint_eq(program->len, 1);
    stmt = program->statements[0];
    ck_assert_uint_eq(stmt.type, ST_Expression);
    e = stmt.data.es.expression;
    ck_assert_uint_eq(e.type, ET_If);

    ck_assert_uint_eq(e.data.if_exp.condition->type, ET_Infix);
    assert_ident(e.data.if_exp.condition->data.infix.left, "x");
    ck_assert_uint_eq(e.data.if_exp.condition->data.infix.oper, IO_Lt);
    assert_ident(e.data.if_exp.condition->data.infix.right, "y");

    ck_assert_ptr_nonnull(e.data.if_exp.consequence.statements);
    ck_assert_uint_eq(e.data.if_exp.consequence.statements->len, 1);
    conseq_stmt = e.data.if_exp.consequence.statements->statements[0];
    ck_assert_uint_eq(conseq_stmt.type, ST_Expression);
    conseq_exp = conseq_stmt.data.es.expression;
    assert_ident(&conseq_exp, "x");

    ck_assert_ptr_nonnull(e.data.if_exp.alternative.statements);
    alt_stmt = e.data.if_exp.alternative.statements->statements[0];
    ck_assert_uint_eq(alt_stmt.type, ST_Expression);
    alt_exp = alt_stmt.data.es.expression;
    assert_ident(&alt_exp, "y");

    program_free(program);
}
END_TEST

START_TEST(test_function_expressions) {
    const char* input = "fn(x, y) { x + y; }";
    Lexer l = lexer_new(input, strlen(input));
    Parser p = parser_new(&l);
    Program* program = parse(&p);
    Statement stmt, body_stmt;
    Expression e, body_exp;
    FunctionLiteral fn;
    check_errors(&p);

    ck_assert_uint_eq(program->len, 1);
    stmt = program->statements[0];
    ck_assert_uint_eq(stmt.type, ST_Expression);
    e = stmt.data.es.expression;
    ck_assert_uint_eq(e.type, ET_Fn);
    fn = e.data.fn;

    ck_assert_ptr_nonnull(fn.parameters);
    ck_assert_uint_eq(fn.parameters->len, 2);
    ck_assert_str_eq(fn.parameters->idents[0].value, "x");
    ck_assert_str_eq(fn.parameters->idents[1].value, "y");

    ck_assert_uint_eq(fn.body.statements->len, 1);

    body_stmt = fn.body.statements->statements[0];
    ck_assert_uint_eq(body_stmt.type, ST_Expression);
    body_exp = body_stmt.data.es.expression;
    ck_assert_uint_eq(body_exp.type, ET_Infix);

    assert_ident(body_exp.data.infix.left, "x");
    ck_assert_uint_eq(body_exp.data.infix.oper, IO_Plus);
    assert_ident(body_exp.data.infix.right, "y");

    program_free(program);
}
END_TEST

START_TEST(test_function_params) {
    ParamTest tests[] = {
        {"fn() {}", 0},
        {"fn(x) {}", 1, {"x"}},
        {"fn(x, y, z) {}", 3, {"x", "y", "z"}},
    };
    size_t i, len = size(tests);
    for (i = 0; i < len; ++i) {
        ParamTest test = tests[i];
        const char* input = test.input;
        Lexer l = lexer_new(input, strlen(input));
        Parser p = parser_new(&l);
        Program* program = parse(&p);
        Statement stmt;
        Expression e;
        FunctionLiteral fn;
        check_errors(&p);

        ck_assert_uint_eq(program->len, 1);
        stmt = program->statements[0];
        ck_assert_uint_eq(stmt.type, ST_Expression);
        e = stmt.data.es.expression;
        ck_assert_uint_eq(e.type, ET_Fn);
        fn = e.data.fn;

        if (test.param_len == 0) {
            ck_assert_ptr_null(fn.parameters);
        } else {
            ck_assert_uint_eq(fn.parameters->len, test.param_len);
            size_t k, p_len = test.param_len;
            for (k = 0; k < p_len; ++k) {
                const char* exp = test.params[k];
                Identifier ident = fn.parameters->idents[k];
                ck_assert_str_eq(exp, ident.value);
            }
        }
        program_free(program);
    }
}
END_TEST

START_TEST(test_call_expression) {
    const char* input = "add(1, 2 * 3, 4 + 5);";
    Lexer l = lexer_new(input, strlen(input));
    Parser p = parser_new(&l);
    Program* program = parse(&p);
    Statement stmt;
    Expression e;
    CallExpression call;

    check_errors(&p);

    ck_assert_uint_eq(program->len, 1);
    stmt = program->statements[0];
    ck_assert_uint_eq(stmt.type, ST_Expression);
    e = stmt.data.es.expression;
    ck_assert_uint_eq(e.type, ET_Call);
    call = e.data.call;
    assert_ident(call.function, "add");
    ck_assert_ptr_nonnull(call.arguments);
    ck_assert_uint_eq(call.arguments->len, 3);

    program_free(program);
}
END_TEST

Suite* suite() {
    Suite* s;
    TCase* tc_core;
    s = suite_create("parser test");
    tc_core = tcase_create("Core");
    tcase_add_test(tc_core, test_let_statement);
    tcase_add_test(tc_core, test_return_statement);
    tcase_add_test(tc_core, test_identifier_expression);
    tcase_add_test(tc_core, test_integer_expression);
    tcase_add_test(tc_core, test_boolean_expression);
    tcase_add_test(tc_core, test_prefix_expression);
    tcase_add_test(tc_core, test_infix_expression);
    tcase_add_test(tc_core, test_operator_precedence);
    tcase_add_test(tc_core, test_if_expression);
    tcase_add_test(tc_core, test_if_else);
    tcase_add_test(tc_core, test_function_expressions);
    tcase_add_test(tc_core, test_function_params);
    tcase_add_test(tc_core, test_call_expression);
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
