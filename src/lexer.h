
#ifndef __LEXER_H__

#define __LEXER_H__

#include "token.h"
#include <stddef.h>

typedef struct {
    const char* input;
    size_t input_len;
    size_t pos;
    char ch;
} Lexer;

Lexer lexer_new(const char* input, size_t input_len);
Token lexer_next_token(Lexer* l);

#endif /* __LEXER_H__ */
