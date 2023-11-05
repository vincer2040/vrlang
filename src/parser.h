
#ifndef __PARSER_H__

#define __PARSER_H__

#include "ast.h"
#include "lexer.h"
#include "token.h"

typedef struct {
    size_t len;
    size_t cap;
    vstr* errors;
} ParserErrors;

typedef struct {
    Lexer l;
    Token cur;
    Token peek;
    ParserErrors errors;
} Parser;

Parser parser_new(Lexer* l);
Program* parse(Parser* p);
void parser_free(Parser* p);
ParserErrors parser_errors(Parser* p);

#endif /* __PARSER_H__ */
