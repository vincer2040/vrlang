#include "token.h"
#include "vstr.h"
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
