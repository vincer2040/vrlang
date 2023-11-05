#ifndef __AST_H__

#define __AST_H__

#include "token.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct Expression;
struct StatementVec;
struct ExpressionVec;

typedef struct {
    Token tok; /* the Ident token */
    vstr value;
} Identifier;

typedef struct {
    Token tok;
    int64_t value;
} IntegerLiteral;

typedef struct {
    Token tok;
    bool value;
} BooleanLiteral;

typedef enum {
    PO_Bang,
    PO_Minus,
} PrefixOperator;

typedef struct {
    Token tok; /* the prefix token, e.g. ! */
    PrefixOperator oper;
    struct Expression* right;
} PrefixExpression;

typedef enum {
    IO_Plus,
    IO_Minus,
    IO_Asterisk,
    IO_Slash,
    IO_Lt,
    IO_Gt,
    IO_Eq,
    IO_NotEq,
} InfixOperator;

typedef struct {
    Token tok; /* the infix token, e.g. + */
    struct Expression* left;
    InfixOperator oper;
    struct Expression* right;
} InfixExpression;

typedef struct {
    Token tok; /* the LSquirly token */
    struct StatementVec* statements;
} BlockStatement;

typedef struct {
    Token tok; /* the If token */
    struct Expression* condition;
    BlockStatement consequence;
    BlockStatement alternative;
} IfExpression;

typedef struct {
    size_t len;
    size_t cap;
    Identifier idents[];
} IdentifierVec;

typedef struct {
    Token tok; /* the Function token */
    IdentifierVec* parameters;
    BlockStatement body;
} FunctionLiteral;

typedef struct {
    Token tok; /* the LParen token */
    struct Expression* function;
    struct ExpressionVec* arguments;
} CallExpression;

typedef struct Expression {
    enum {
        ET_Invalid,
        ET_Identifier,
        ET_Integer,
        ET_Boolean,
        ET_Prefix,
        ET_Infix,
        ET_If,
        ET_Fn,
        ET_Call,
    } type;
    union {
        Identifier ident;
        IntegerLiteral integer;
        BooleanLiteral boolean;
        PrefixExpression prefix;
        InfixExpression infix;
        IfExpression if_exp;
        FunctionLiteral fn;
        CallExpression call;
    } data;
} Expression;

typedef struct ExpressionVec {
    size_t len;
    size_t cap;
    Expression expressions[];
} ExpressionVec;

typedef struct {
    Token tok; /* the Let token */
    Identifier name;
    Expression value;
} LetStatement;

typedef struct {
    Token tok; /* the Return token */
    Expression value;
} ReturnStatement;

typedef struct {
    Token tok;
    Expression expression;
} ExpressionStatement;

typedef struct {
    enum {
        ST_Invalid,
        ST_Let,
        ST_Return,
        ST_Expression,
    } type;
    union {
        LetStatement let;
        ReturnStatement ret;
        ExpressionStatement es;
    } data;
} Statement;

typedef struct StatementVec {
    size_t len;
    size_t cap;
    Statement statements[];
} StatementVec;

typedef StatementVec Program;

StatementVec* stmt_vec_init(void);
IdentifierVec* ident_vec_init(void);
ExpressionVec* exp_vec_init(void);

int stmt_vec_append(StatementVec** stmt_vec, Statement* stmt);
int ident_vec_append(IdentifierVec** ident_vec, Identifier* ident);
int exp_vec_append(ExpressionVec** exp_vec, Expression* e);

void expression_free(Expression* e);
void function_params_free(IdentifierVec* vec);
void block_statement_free(BlockStatement* bs);
void expression_vec_free(ExpressionVec* vec);
void program_free(Program* program);

vstr statement_token_literal(Statement* stmt);
vstr let_statement_token_literal(LetStatement* let);
vstr return_statement_token_literal(ReturnStatement* ret);
vstr boolean_token_literal(BooleanLiteral* boolean);
vstr integer_token_literal(IntegerLiteral* integer);
vstr identifier_token_literal(Identifier* ident);
vstr expression_statement_token_literal(ExpressionStatement* es);
vstr expression_token_literal(Expression* e);
vstr block_statement_token_literal(BlockStatement* bs);

vstr program_string(Program* program);

#endif /* __AST_H__ */
