#include "parser.h"
#include "ast.h"
#include "token.h"
#include "util.h"
#include "vstr.h"
#include <assert.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum {
    Lowest = 0,
    Equals = 1,
    LessGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
} Precedence;

static void parser_next_token(Parser* p);
static inline int cur_tok_is(Parser* p, TokenT type);
static inline int peek_tok_is(Parser* p, TokenT type);
static int expect_peek(Parser* p, TokenT type);
static Precedence peek_precedence(Parser* p);
static Precedence cur_precedence(Parser* p);
static int peek_error(Parser* p, TokenT type);
static int no_prefix_parse_method(Parser* p, TokenT type);

static Statement parse_statement(Parser* p);
static Statement parse_let_statement(Parser* p);
static Statement parse_return_statement(Parser* p);
static Statement parse_expression_statement(Parser* p);

static Expression parse_expression(Parser* p, Precedence precedence);
static Expression parse_identifier(Parser* p);
static Expression parse_integer(Parser* p);
static Expression parse_boolean(Parser* p);
static Expression parse_prefix_expression(Parser* p);
static Expression parse_infix_expression(Parser* p, Expression* left);
static Expression parse_grouped_expression(Parser* p);
static Expression parse_if_expression(Parser* p);
static Expression parse_function(Parser* p);
static Expression parse_call_expression(Parser* p, Expression* fn);

static BlockStatement parse_block_statement(Parser* p);
static IdentifierVec* parse_function_params(Parser* p);
static ExpressionVec* parse_call_args(Parser* p);

static int append_err(Parser* p, vstr err);
static void parser_errors_free(ParserErrors* errs);

Parser parser_new(Lexer* l) {
    Parser p = {0};

    p.l = *l;

    parser_next_token(&p);
    parser_next_token(&p);

    return p;
}

static void parser_next_token(Parser* p) {
    p->cur = p->peek;
    p->peek = lexer_next_token(&(p->l));
}

Program* parse(Parser* p) {
    Program* program = stmt_vec_init();
    while (p->cur.type != Eoft) {
        Statement stmt = parse_statement(p);
        if (stmt.type != ST_Invalid) {
            int append_res = stmt_vec_append(&program, &stmt);
            assert(append_res != -1);
        }
        parser_next_token(p);
    }
    return program;
}

static Statement parse_statement(Parser* p) {
    Statement stmt = {0};
    switch (p->cur.type) {
    case Let:
        stmt = parse_let_statement(p);
        break;
    case Return:
        stmt = parse_return_statement(p);
        break;
    default:
        stmt = parse_expression_statement(p);
        break;
    }
    return stmt;
}

static Statement parse_let_statement(Parser* p) {
    Statement stmt = {0};
    Identifier name = {0};

    stmt.data.let.tok = p->cur;

    if (!expect_peek(p, Ident)) {
        return stmt;
    }

    name.tok = p->cur;
    name.value = p->cur.value;

    if (!expect_peek(p, Assign)) {
        return stmt;
    }

    parser_next_token(p);

    stmt.data.let.value = parse_expression(p, Lowest);

    if (peek_tok_is(p, Semicolon)) {
        parser_next_token(p);
    }

    stmt.type = ST_Let;
    stmt.data.let.name = name;
    return stmt;
}

static Statement parse_return_statement(Parser* p) {
    Statement stmt = {0};
    stmt.data.ret.tok = p->cur;
    stmt.type = ST_Return;

    parser_next_token(p);

    stmt.data.ret.value = parse_expression(p, Lowest);

    if (peek_tok_is(p, Semicolon)) {
        parser_next_token(p);
    }

    return stmt;
}

static Statement parse_expression_statement(Parser* p) {
    Statement stmt = {0};

    stmt.type = ST_Expression;
    stmt.data.es.tok = p->cur;
    stmt.data.es.expression = parse_expression(p, Lowest);

    while (peek_tok_is(p, Semicolon)) {
        parser_next_token(p);
    }

    return stmt;
}

static Expression parse_expression(Parser* p, Precedence precedence) {
    Expression e = {0};

    switch (p->cur.type) {
    case Ident:
        e = parse_identifier(p);
        break;
    case Int:
        e = parse_integer(p);
        break;
    case Bang:
        e = parse_prefix_expression(p);
        break;
    case Minus:
        e = parse_prefix_expression(p);
        break;
    case True:
        e = parse_boolean(p);
        break;
    case False:
        e = parse_boolean(p);
        break;
    case LParen:
        e = parse_grouped_expression(p);
        break;
    case If:
        e = parse_if_expression(p);
        break;
    case Function:
        e = parse_function(p);
        break;
    default:
        no_prefix_parse_method(p, p->cur.type);
        return e;
    }

    while ((!peek_tok_is(p, Semicolon)) && (precedence < peek_precedence(p))) {
        switch (p->peek.type) {
        case Plus:
        case Minus:
        case Slash:
        case Asterisk:
        case Eq:
        case NotEq:
        case Lt:
        case Gt:
            parser_next_token(p);
            e = parse_infix_expression(p, &e);
            break;
        case LParen:
            parser_next_token(p);
            e = parse_call_expression(p, &e);
            break;
        default:
            return e;
        }
    }
    return e;
}

static Expression parse_identifier(Parser* p) {
    Expression e = {0};
    e.type = ET_Identifier;
    e.data.ident.tok = p->cur;
    e.data.ident.value = p->cur.value;
    return e;
}

static Expression parse_integer(Parser* p) {
    Expression e = {0};
    vstr val = p->cur.value;
    size_t i, len = vstr_len(val);
    int64_t res = 0;

    for (i = 0; i < len; ++i) {
        char at = val[i];

        res = (res * 10) + (at - '0');
    }

    e.type = ET_Integer;
    e.data.integer.tok = p->cur;
    e.data.integer.value = res;
    return e;
}

static Expression parse_boolean(Parser* p) {
    Expression e = {0};
    e.type = ET_Boolean;
    e.data.boolean.tok = p->cur;
    switch (p->cur.type) {
    case True:
        e.data.boolean.value = true;
        break;
    case False:
        e.data.boolean.value = false;
        break;
    default:
        unreachable;
    }
    return e;
}

static Expression parse_prefix_expression(Parser* p) {
    Expression e = {0};
    Expression* right;
    e.type = ET_Prefix;
    e.data.prefix.tok = p->cur;
    switch (p->cur.type) {
    case Bang:
        e.data.prefix.oper = PO_Bang;
        break;
    case Minus:
        e.data.prefix.oper = PO_Minus;
        break;
    default:
        unreachable;
    }
    right = calloc(1, sizeof *right);
    assert(right != NULL);
    parser_next_token(p);
    *right = parse_expression(p, Prefix);
    e.data.prefix.right = right;
    return e;
}

static Expression parse_infix_expression(Parser* p, Expression* left) {
    Expression e = {0};
    Expression* right;
    Precedence prec;
    e.type = ET_Infix;
    e.data.infix.left = calloc(1, sizeof(Expression));
    assert(e.data.infix.left != NULL);
    memcpy(e.data.infix.left, left, sizeof(Expression));
    switch (p->cur.type) {
    case Plus:
        e.data.infix.oper = IO_Plus;
        break;
    case Minus:
        e.data.infix.oper = IO_Minus;
        break;
    case Asterisk:
        e.data.infix.oper = IO_Asterisk;
        break;
    case Slash:
        e.data.infix.oper = IO_Slash;
        break;
    case Lt:
        e.data.infix.oper = IO_Lt;
        break;
    case Gt:
        e.data.infix.oper = IO_Gt;
        break;
    case Eq:
        e.data.infix.oper = IO_Eq;
        break;
    case NotEq:
        e.data.infix.oper = IO_NotEq;
        break;
    default:
        unreachable;
    }
    right = calloc(1, sizeof *right);
    assert(right != NULL);
    prec = cur_precedence(p);
    parser_next_token(p);
    *right = parse_expression(p, prec);
    e.data.infix.right = right;
    return e;
}

static Expression parse_grouped_expression(Parser* p) {
    Expression e = {0};
    parser_next_token(p);
    e = parse_expression(p, Lowest);
    if (!expect_peek(p, RParen)) {
        expression_free(&e);
        memset(&e, 0, sizeof e);
        return e;
    }
    return e;
}

static Expression parse_if_expression(Parser* p) {
    Expression e = {0};
    Expression* condition;
    e.type = ET_If;
    e.data.if_exp.tok = p->cur;

    if (!expect_peek(p, LParen)) {
        memset(&e, 0, sizeof e);
        return e;
    }

    condition = calloc(1, sizeof *condition);
    assert(condition != NULL);

    parser_next_token(p);

    *condition = parse_expression(p, Lowest);

    if (!expect_peek(p, RParen)) {
        memset(&e, 0, sizeof e);
        expression_free(condition);
        free(condition);
        return e;
    }

    if (!expect_peek(p, LSquirly)) {
        memset(&e, 0, sizeof e);
        expression_free(condition);
        free(condition);
        return e;
    }

    e.data.if_exp.condition = condition;
    e.data.if_exp.consequence = parse_block_statement(p);

    if (peek_tok_is(p, Else)) {
        parser_next_token(p);

        if (!expect_peek(p, LSquirly)) {
            memset(&e, 0, sizeof e);
            block_statement_free(&(e.data.if_exp.consequence));
            expression_free(condition);
            free(condition);
            return e;
        }

        e.data.if_exp.alternative = parse_block_statement(p);
    }

    return e;
}

static Expression parse_function(Parser* p) {
    Expression e = {0};
    e.type = ET_Fn;
    e.data.fn.tok = p->cur;
    if (!expect_peek(p, LParen)) {
        memset(&e, 0, sizeof e);
        return e;
    }
    e.data.fn.parameters = parse_function_params(p);
    if (!expect_peek(p, LSquirly)) {
        function_params_free(e.data.fn.parameters);
        memset(&e, 0, sizeof e);
        return e;
    }
    e.data.fn.body = parse_block_statement(p);
    return e;
}

static Expression parse_call_expression(Parser* p, Expression* fn) {
    Expression e = {0};
    e.type = ET_Call;
    e.data.call.tok = p->cur;
    e.data.call.function = calloc(1, sizeof(Expression));
    assert(e.data.call.function != NULL);
    memcpy(e.data.call.function, fn, sizeof *fn);
    e.data.call.arguments = parse_call_args(p);
    return e;
}

static ExpressionVec* parse_call_args(Parser* p) {
    ExpressionVec* vec = NULL;
    Expression e = {0};
    int append_res;
    if (peek_tok_is(p, RParen)) {
        parser_next_token(p);
        return vec;
    }
    vec = exp_vec_init();
    assert(vec != NULL);
    parser_next_token(p);

    e = parse_expression(p, Lowest);
    append_res = exp_vec_append(&vec, &e);
    assert(append_res != -1);

    while (peek_tok_is(p, Comma)) {
        parser_next_token(p);
        parser_next_token(p);
        e = parse_expression(p, Lowest);
        append_res = exp_vec_append(&vec, &e);
        assert(append_res != -1);
    }

    if (!expect_peek(p, RParen)) {
        expression_vec_free(vec);
        return NULL;
    }
    return vec;
}

static IdentifierVec* parse_function_params(Parser* p) {
    IdentifierVec* vec = NULL;
    Identifier ident = {0};
    int append_res;
    if (peek_tok_is(p, RParen)) {
        parser_next_token(p);
        return vec;
    }
    parser_next_token(p);
    vec = ident_vec_init();

    ident.tok = p->cur;
    ident.value = p->cur.value;
    append_res = ident_vec_append(&vec, &ident);
    assert(append_res != -1);

    while (peek_tok_is(p, Comma)) {
        parser_next_token(p);
        parser_next_token(p);
        ident.tok = p->cur;
        ident.value = p->cur.value;
        append_res = ident_vec_append(&vec, &ident);
        assert(append_res != -1);
    }

    if (!expect_peek(p, RParen)) {
        function_params_free(vec);
        return NULL;
    }
    return vec;
}

static BlockStatement parse_block_statement(Parser* p) {
    BlockStatement bs = {0};
    bs.tok = p->cur;
    StatementVec* stmts = stmt_vec_init();
    parser_next_token(p);

    while ((!cur_tok_is(p, RSquirly)) && (!cur_tok_is(p, Eoft))) {
        Statement stmt = parse_statement(p);
        if (stmt.type != ST_Invalid) {
            int append_res = stmt_vec_append(&stmts, &stmt);
            assert(append_res != -1);
        }
        parser_next_token(p);
    }
    bs.statements = stmts;
    return bs;
}

static inline int cur_tok_is(Parser* p, TokenT type) {
    return p->cur.type == type;
}

static inline int peek_tok_is(Parser* p, TokenT type) {
    return p->peek.type == type;
}

static int expect_peek(Parser* p, TokenT type) {
    if (peek_tok_is(p, type)) {
        parser_next_token(p);
        return 1;
    } else {
        int peek_res = peek_error(p, type);
        assert(peek_res != -1);
        return 0;
    }
}

static inline Precedence precedence(TokenT type) {
    switch (type) {
    case Eq:
        return Equals;
    case NotEq:
        return Equals;
    case Lt:
        return LessGreater;
    case Gt:
        return LessGreater;
    case Plus:
        return Sum;
    case Minus:
        return Sum;
    case Asterisk:
        return Product;
    case Slash:
        return Product;
    case LParen:
        return Call;
    default:
        return Lowest;
    }
}

static Precedence peek_precedence(Parser* p) {
    return precedence(p->peek.type);
}

static Precedence cur_precedence(Parser* p) { return precedence(p->cur.type); }

static int peek_error(Parser* p, TokenT type) {
    vstr e = vstr_format("expected next token to be %s, got %s instead",
                         token_str(type), token_str(p->peek.type));
    return append_err(p, e);
}

static int no_prefix_parse_method(Parser* p, TokenT type) {
    vstr e =
        vstr_format("no prefix parse function for %s found", token_str(type));
    return append_err(p, e);
}

static ParserErrors parser_errors_init(void) {
    ParserErrors errs = {0};
    errs.errors = calloc(PARSER_ERRORS_INITIAL_CAP, sizeof(vstr));
    assert(errs.errors != NULL);
    errs.cap = PARSER_ERRORS_INITIAL_CAP;
    return errs;
}

static int append_err(Parser* p, vstr err) {
    size_t len = p->errors.len, cap = p->errors.cap;
    if (cap == 0) {
        p->errors = parser_errors_init();
        cap = p->errors.cap;
    }

    if (len == cap) {
        void* tmp;
        cap <<= 1;
        tmp = realloc(p->errors.errors, cap * sizeof(vstr));
        if (tmp == NULL) {
            return -1;
        }

        p->errors.errors = tmp;
        memset(p->errors.errors + len, 0, (len - cap) * sizeof(vstr));
    }

    p->errors.errors[len] = err;
    p->errors.len++;
    return 0;
}

void parser_free(Parser* p) {
    if (p->errors.cap) {
        ParserErrors errs = p->errors;
        parser_errors_free(&errs);
    }
}

static void parser_errors_free(ParserErrors* errs) {
    size_t i, len = errs->len;
    for (i = 0; i < len; ++i) {
        vstr cur = errs->errors[i];
        vstr_delete(cur);
    }

    free(errs->errors);
}
