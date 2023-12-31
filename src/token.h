#ifndef __TOKEN_H__

#define __TOKEN_H__

#include "vstr.h"

typedef enum {
    Illegal,
    Eoft,
    Ident,
    Int,
    Assign,
    Plus,
    Minus,
    Slash,
    Asterisk,
    Bang,
    Lt,
    Gt,
    Eq,
    NotEq,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
} TokenT;

typedef struct {
    TokenT type;
    vstr value;
} Token;

TokenT lookup(const char* value, size_t value_len);
void token_free(Token* tok);
const char* token_str(TokenT t);

#endif /* __TOKEN_H__ */
