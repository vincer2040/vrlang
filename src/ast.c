#include "ast.h"
#include "util.h"
#include "vstr.h"
#include <assert.h>
#include <memory.h>
#include <stdlib.h>

vstr expression_string(Expression* e);
vstr statement_string(Statement* stmt);
void statement_free(Statement* stmt);

StatementVec* stmt_vec_init(void) {
    StatementVec* p;
    size_t needed = (sizeof *p) + (sizeof(Statement) * INITIAL_CAP);
    p = malloc(needed);
    assert(p != NULL);
    memset(p, 0, needed);
    p->len = 0;
    p->cap = INITIAL_CAP;
    return p;
}

IdentifierVec* ident_vec_init(void) {
    IdentifierVec* vec;
    size_t needed = (sizeof *vec) + (sizeof(Identifier) * INITIAL_CAP);
    vec = malloc(needed);
    assert(vec != NULL);
    memset(vec, 0, needed);
    vec->len = 0;
    vec->cap = INITIAL_CAP;
    return vec;
}

ExpressionVec* exp_vec_init(void) {
    ExpressionVec* vec;
    size_t needed = (sizeof *vec) + (sizeof(Expression) * INITIAL_CAP);
    vec = malloc(needed);
    assert(vec != NULL);
    memset(vec, 0, needed);
    vec->len = 0;
    vec->cap = INITIAL_CAP;
    return vec;
}

const char* prefix_operator_str(PrefixOperator oper) {
    switch (oper) {
    case PO_Bang:
        return "!";
    case PO_Minus:
        return "-";
    }

    unreachable;
}

const char* infix_operator_str(InfixOperator oper) {
    switch (oper) {
    case IO_Plus:
        return "+";
    case IO_Minus:
        return "-";
    case IO_Asterisk:
        return "*";
    case IO_Slash:
        return "/";
    case IO_Lt:
        return "<";
    case IO_Gt:
        return ">";
    case IO_Eq:
        return "==";
    case IO_NotEq:
        return "!=";
    }

    unreachable;
}

vstr let_statement_token_literal(LetStatement* let) {
    UNUSED(let);
    return vstr_from("let");
}

vstr return_statement_token_literal(ReturnStatement* ret) {
    UNUSED(ret);
    return vstr_from("return");
}

vstr identifier_token_literal(Identifier* ident) {
    return vstr_dup(ident->value);
}

vstr expression_statement_token_literal(ExpressionStatement* es) {
    return vstr_from(token_str(es->tok.type));
}

vstr integer_token_literal(IntegerLiteral* integer) {
    return vstr_dup(integer->tok.value);
}

vstr boolean_token_literal(BooleanLiteral* boolean) {
    return vstr_from(token_str(boolean->tok.type));
}

vstr prefix_expression_token_literal(PrefixExpression* pe) {
    return vstr_from(token_str(pe->tok.type));
}

vstr infix_expression_token_literal(InfixExpression* ie) {
    return vstr_from(token_str(ie->tok.type));
}

vstr if_expression_token_literal(IfExpression* if_exp) {
    return vstr_from(token_str(if_exp->tok.type));
}

vstr block_statement_token_literal(BlockStatement* bs) {
    return vstr_from(token_str(bs->tok.type));
}

vstr function_token_literal(FunctionLiteral* fl) {
    return vstr_from(token_str(fl->tok.type));
}

vstr call_expression_token_literal(CallExpression* call) {
    return vstr_from(token_str(call->tok.type));
}

vstr expression_token_literal(Expression* e) {
    switch (e->type) {
    case ET_Identifier:
        return identifier_token_literal(&(e->data.ident));
    case ET_Integer:
        return integer_token_literal(&(e->data.integer));
    case ET_Boolean:
        return boolean_token_literal(&(e->data.boolean));
    case ET_Prefix:
        return prefix_expression_token_literal(&(e->data.prefix));
    case ET_Infix:
        return infix_expression_token_literal(&(e->data.infix));
    case ET_If:
        return if_expression_token_literal(&(e->data.if_exp));
    case ET_Fn:
        return function_token_literal(&(e->data.fn));
    case ET_Call:
        return call_expression_token_literal(&(e->data.call));
    default:
        break;
    }
    unreachable;
}

vstr statement_token_literal(Statement* stmt) {
    switch (stmt->type) {
    case ST_Let:
        return let_statement_token_literal(&(stmt->data.let));
    case ST_Return:
        return return_statement_token_literal(&(stmt->data.ret));
    case ST_Expression:
        return expression_statement_token_literal(&(stmt->data.es));
    default:
        break;
    }
    unreachable;
}

vstr identifier_string(Identifier* ident) {
    return identifier_token_literal(ident);
}

vstr let_statement_string(LetStatement* let) {
    vstr s = vstr_new();
    vstr let_lit = let_statement_token_literal(let);
    vstr name_lit = identifier_token_literal(&(let->name));
    vstr exp_str = expression_string(&(let->value));
    s = vstr_push_string(s, let_lit);
    vstr_delete(let_lit);
    s = vstr_push_char(s, ' ');
    s = vstr_push_string(s, name_lit);
    vstr_delete(name_lit);
    s = vstr_push_string(s, " = ");
    s = vstr_push_string(s, exp_str);
    vstr_delete(exp_str);
    s = vstr_push_char(s, ';');
    return s;
}

vstr return_statement_string(ReturnStatement* ret) {
    vstr s = vstr_new();
    vstr ret_lit = return_statement_token_literal(ret);
    vstr exp_str = expression_string(&(ret->value));
    s = vstr_push_string(s, ret_lit);
    vstr_delete(ret_lit);
    s = vstr_push_char(s, ' ');
    s = vstr_push_string(s, exp_str);
    vstr_delete(s);
    s = vstr_push_char(s, ';');
    return s;
}

vstr integer_literal_string(IntegerLiteral* integer) {
    return integer_token_literal(integer);
}

vstr boolean_literal_string(BooleanLiteral* boolean) {
    return boolean_token_literal(boolean);
}

vstr prefix_expression_string(PrefixExpression* prefix) {
    vstr s = vstr_new();
    vstr exp_str = expression_string(prefix->right);
    s = vstr_push_char(s, '(');
    s = vstr_push_string(s, prefix_operator_str(prefix->oper));
    s = vstr_push_string(s, exp_str);
    vstr_delete(exp_str);
    s = vstr_push_char(s, ')');
    return s;
}

vstr infix_expression_string(InfixExpression* infix) {
    vstr s = vstr_new();
    vstr left_exp = expression_string(infix->left);
    vstr right_exp = expression_string(infix->right);
    s = vstr_push_char(s, '(');
    s = vstr_push_string(s, left_exp);
    vstr_delete(left_exp);
    s = vstr_push_char(s, ' ');
    s = vstr_push_string(s, infix_operator_str(infix->oper));
    s = vstr_push_char(s, ' ');
    s = vstr_push_string(s, right_exp);
    vstr_delete(right_exp);
    s = vstr_push_char(s, ')');
    return s;
}

vstr block_statement_string(BlockStatement* bs) {
    vstr s = vstr_new();
    size_t i, len = bs->statements->len;
    for (i = 0; i < len; ++i) {
        Statement cur = bs->statements->statements[i];
        vstr stmt_str = statement_string(&cur);
        s = vstr_push_string(s, stmt_str);
        vstr_delete(stmt_str);
    }
    return s;
}

vstr if_expression_string(IfExpression* if_exp) {
    vstr s = vstr_new();
    vstr cond_str = expression_string(if_exp->condition);
    vstr conseq_str = block_statement_string(&(if_exp->consequence));
    vstr alt_str;
    s = vstr_push_string(s, "if");
    s = vstr_push_string(s, cond_str);
    vstr_delete(cond_str);
    s = vstr_push_char(s, ' ');
    s = vstr_push_string(s, conseq_str);
    vstr_delete(conseq_str);
    if (if_exp->alternative.statements) {
        alt_str = block_statement_string(&(if_exp->alternative));
        s = vstr_push_string(s, "else ");
        s = vstr_push_string(s, alt_str);
        vstr_delete(alt_str);
    }
    return s;
}

vstr function_literal_string(FunctionLiteral* fn) {
    vstr s = vstr_new();
    vstr fn_tok_literal = function_token_literal(fn);
    vstr body_str = block_statement_string(&(fn->body));
    s = vstr_push_string(s, fn_tok_literal);
    vstr_delete(fn_tok_literal);
    s = vstr_push_char(s, '(');
    if (fn->parameters) {
        size_t i, len = fn->parameters->len;
        for (i = 0; i < len; ++i) {
            Identifier ident = fn->parameters->idents[i];
            vstr ident_str = identifier_string(&ident);
            s = vstr_push_string(s, ident_str);
            vstr_delete(ident_str);
            if (i != (len - 1)) {
                s = vstr_push_string(s, ", ");
            }
        }
    }
    s = vstr_push_string(s, ") ");
    s = vstr_push_string(s, body_str);
    vstr_delete(body_str);
    return s;
}

vstr call_expression_string(CallExpression* call) {
    vstr s = vstr_new();
    vstr fn_str = expression_string(call->function);
    s = vstr_push_string(s, fn_str);
    vstr_delete(fn_str);
    s = vstr_push_char(s, '(');
    if (call->arguments) {
        size_t i, len = call->arguments->len;
        for (i = 0; i < len; ++i) {
            Expression arg = call->arguments->expressions[i];
            vstr arg_str = expression_string(&arg);
            s = vstr_push_string(s, arg_str);
            vstr_delete(arg_str);

            if (i != (len - 1)) {
                s = vstr_push_string(s, ", ");
            }
        }
    }
    s = vstr_push_char(s, ')');
    return s;
}

vstr expression_string(Expression* e) {
    switch (e->type) {
    case ET_Identifier:
        return identifier_string(&(e->data.ident));
    case ET_Integer:
        return integer_literal_string(&(e->data.integer));
    case ET_Boolean:
        return boolean_literal_string(&(e->data.boolean));
    case ET_Prefix:
        return prefix_expression_string(&(e->data.prefix));
    case ET_Infix:
        return infix_expression_string(&(e->data.infix));
    case ET_Fn:
        return function_literal_string(&(e->data.fn));
    case ET_Call:
        return call_expression_string(&(e->data.call));
    default:
        unreachable;
    }
}

vstr statement_string(Statement* stmt) {
    switch (stmt->type) {
    case ST_Let:
        return let_statement_string(&(stmt->data.let));
    case ST_Return:
        return return_statement_string(&(stmt->data.ret));
    case ST_Expression:
        return expression_string(&(stmt->data.es.expression));
    default:
        break;
    }
    unreachable;
}

vstr program_string(Program* program) {
    vstr s = vstr_new();
    size_t i, len = program->len;
    for (i = 0; i < len; ++i) {
        Statement cur = program->statements[i];
        vstr stmt_str = statement_string(&cur);
        s = vstr_push_string(s, stmt_str);
        vstr_delete(stmt_str);
    }
    return s;
}

void identifier_free(Identifier* ident) { vstr_delete(ident->value); }

void return_statement_free(ReturnStatement* ret) {
    expression_free(&(ret->value));
}

void let_statement_free(LetStatement* let) {
    vstr_delete(let->name.value);
    expression_free(&(let->value));
}

void integer_literal_free(IntegerLiteral* integer) {
    vstr_delete(integer->tok.value);
}

void prefix_expression_free(PrefixExpression* prefix) {
    expression_free(prefix->right);
    free(prefix->right);
}

void infix_expression_free(InfixExpression* infix) {
    expression_free(infix->left);
    expression_free(infix->right);

    free(infix->left);
    free(infix->right);
}

void block_statement_free(BlockStatement* bs) {
    if (bs->statements) {
        size_t i, len = bs->statements->len;
        for (i = 0; i < len; ++i) {
            Statement cur = bs->statements->statements[i];
            statement_free(&cur);
        }
        free(bs->statements);
    }
}

void if_expression_free(IfExpression* if_exp) {
    expression_free(if_exp->condition);
    block_statement_free(&(if_exp->consequence));
    block_statement_free(&(if_exp->alternative));
    free(if_exp);
}

void function_params_free(IdentifierVec* vec) {
    if (vec) {
        size_t i, len = vec->len;
        for (i = 0; i < len; ++i) {
            Identifier ident = vec->idents[i];
            identifier_free(&ident);
        }
        free(vec);
    }
}

void function_expression_free(FunctionLiteral* fn) {
    function_params_free(fn->parameters);
    block_statement_free(&(fn->body));
}

void expression_vec_free(ExpressionVec* vec) {
    if (vec) {
        size_t i, len = vec->len;
        for (i = 0; i < len; ++i) {
            Expression e = vec->expressions[i];
            expression_free(&e);
        }
        free(vec);
    }
}

void call_expression_free(CallExpression* call) {
    expression_vec_free(call->arguments);
    expression_free(call->function);
    free(call->function);
}

void expression_free(Expression* e) {
    switch (e->type) {
    case ET_Identifier:
        identifier_free(&(e->data.ident));
        break;
    case ET_Integer:
        integer_literal_free(&(e->data.integer));
        break;
    case ET_Prefix:
        prefix_expression_free(&(e->data.prefix));
        break;
    case ET_Infix:
        infix_expression_free(&(e->data.infix));
        break;
    case ET_Fn:
        function_expression_free(&(e->data.fn));
        break;
    case ET_Call:
        call_expression_free(&(e->data.call));
        break;
    default:
        break;
    }
}

void expression_statement_free(ExpressionStatement* es) {
    expression_free(&(es->expression));
}

void statement_free(Statement* stmt) {
    switch (stmt->type) {
    case ST_Let:
        let_statement_free(&(stmt->data.let));
        break;
    case ST_Return:
        return_statement_free(&(stmt->data.ret));
        break;
    case ST_Expression:
        expression_statement_free(&(stmt->data.es));
        break;
    default:
        break;
    }
}

void program_free(Program* program) {
    size_t i, len = program->len;
    for (i = 0; i < len; ++i) {
        Statement stmt = program->statements[i];
        statement_free(&stmt);
    }
    free(program);
}

int stmt_vec_append(StatementVec** stmt_vec, Statement* stmt) {
    size_t len = (*stmt_vec)->len, cap = (*stmt_vec)->cap;
    if (len == cap) {
        void* tmp;
        cap <<= 1;
        tmp = realloc(*stmt_vec, (cap * sizeof(Statement)));
        if (tmp == NULL) {
            return -1;
        }
        *stmt_vec = tmp;
        memset(&((*stmt_vec)->statements[len]), 0,
               (cap - len) * sizeof(Statement));
        (*stmt_vec)->cap = cap;
    }
    memcpy(&((*stmt_vec)->statements[len]), stmt, sizeof *stmt);
    (*stmt_vec)->len++;
    return 0;
}

int ident_vec_append(IdentifierVec** ident_vec, Identifier* ident) {
    size_t len = (*ident_vec)->len, cap = (*ident_vec)->cap;
    if (len == cap) {
        void* tmp;
        cap <<= 1;
        tmp = realloc(*ident_vec, (cap * sizeof(Identifier)));
        if (tmp == NULL) {
            return -1;
        }
        *ident_vec = tmp;
        memset(&((*ident_vec)->idents[len]), 0,
               (cap - len) * sizeof(Identifier));
        (*ident_vec)->cap = cap;
    }
    memcpy(&((*ident_vec)->idents[len]), ident, sizeof *ident);
    (*ident_vec)->len++;
    return 0;
}

int exp_vec_append(ExpressionVec** exp_vec, Expression* e) {
    size_t len = (*exp_vec)->len, cap = (*exp_vec)->cap;
    if (len == cap) {
        void* tmp;
        cap <<= 1;
        tmp = realloc(*exp_vec, (cap * sizeof(Expression)));
        if (tmp == NULL) {
            return -1;
        }
        *exp_vec = tmp;
        memset(&((*exp_vec)->expressions[len]), 0,
               (cap - len) * sizeof(Identifier));
        (*exp_vec)->cap = cap;
    }
    memcpy(&((*exp_vec)->expressions[len]), e, sizeof *e);
    (*exp_vec)->len++;
    return 0;
}
