#include "token.h"
#include "util.h"
#include "vstr.h"
#include <assert.h>
#include <memory.h>
#include <stddef.h>

typedef struct {
    const char* key_word;
    size_t key_word_len;
    TokenT type;
} KeyWord;

const KeyWord key_words[] = {
    {"fn", 2, Function},   {"let", 3, Let},   {"if", 2, If},
    {"else", 4, Else},     {"true", 4, True}, {"false", 5, False},
    {"return", 6, Return},
};

size_t key_words_len = sizeof key_words / sizeof key_words[0];

TokenT lookup(const char* value, size_t value_len) {
    size_t i;
    for (i = 0; i < key_words_len; ++i) {
        const KeyWord key_word = key_words[i];
        const char* word = key_word.key_word;
        size_t len = key_word.key_word_len;
        if ((len == value_len) && (memcmp(word, value, len) == 0)) {
            return key_word.type;
        }
    }

    return Ident;
}

void token_free(Token* tok) {
    switch (tok->type) {
    case Ident:
        vstr_delete(tok->value);
        break;
    case Int:
        vstr_delete(tok->value);
        break;
    default:
        break;
    }
}

const char* token_str(TokenT t) {
    switch (t) {
    case Illegal:
        return "Illegal";
    case Eoft:
        return "Eoft";
    case Ident:
        return "Ident";
    case Int:
        return "Int";
    case Assign:
        return "Assign";
    case Plus:
        return "Plus";
    case Minus:
        return "Minus";
    case Slash:
        return "Slash";
    case Asterisk:
        return "Asterisk";
    case Bang:
        return "Bang";
    case Lt:
        return "Lt";
    case Gt:
        return "Gt";
    case Eq:
        return "Eq";
    case NotEq:
        return "NotEq";
    case Comma:
        return "Comma";
    case Semicolon:
        return "Semicolon";
    case LParen:
        return "LParen";
    case RParen:
        return "RParen";
    case LSquirly:
        return "LSquirly";
    case RSquirly:
        return "RSquirly";
    case Function:
        return "Function";
    case Let:
        return "let";
    case If:
        return "if";
    case Else:
        return "else";
    case Return:
        return "return";
    case True:
        return "true";
    case False:
        return "false";
    }

    unreachable;
}
