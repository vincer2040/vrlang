#include "lexer.h"
#include "token.h"

static void lexer_read_char(Lexer* l);
static inline int is_char(char ch);
static inline int is_digit(char ch);
static inline void skip_white_space(Lexer* l);
static void lexer_read_ident(Lexer* l);
static void lexer_read_int(Lexer* l);
static char peek_char(Lexer* l);

Lexer lexer_new(const char* input, size_t input_len) {
    Lexer l = {0};
    l.input = input;
    l.input_len = input_len;
    lexer_read_char(&l);
    return l;
}

static void lexer_read_char(Lexer* l) {
    if (l->pos >= l->input_len) {
        l->ch = 0;
    } else {
        l->ch = l->input[l->pos];
    }
    l->pos++;
}

Token lexer_next_token(Lexer* l) {
    Token tok = {0};
    skip_white_space(l);
    switch (l->ch) {
    case '=':
        if (peek_char(l) == '=') {
            lexer_read_char(l);
            tok.type = Eq;
        } else {
            tok.type = Assign;
        }
        break;
    case '+':
        tok.type = Plus;
        break;
    case '-':
        tok.type = Minus;
        break;
    case '*':
        tok.type = Asterisk;
        break;
    case '/':
        tok.type = Slash;
        break;
    case '!':
        if (peek_char(l) == '=') {
            lexer_read_char(l);
            tok.type = NotEq;
        } else {
            tok.type = Bang;
        }
        break;
    case '<':
        tok.type = Lt;
        break;
    case '>':
        tok.type = Gt;
        break;
    case '(':
        tok.type = LParen;
        break;
    case ')':
        tok.type = RParen;
        break;
    case '{':
        tok.type = LSquirly;
        break;
    case '}':
        tok.type = RSquirly;
        break;
    case ',':
        tok.type = Comma;
        break;
    case ';':
        tok.type = Semicolon;
        break;
    case 0:
        tok.type = Eoft;
        break;
    default:
        if (is_char(l->ch)) {
            size_t end_pos, start_pos = l->pos - 1;
            const char* val = l->input + (l->pos - 1);
            TokenT type = lookup(val);
            lexer_read_ident(l);
            end_pos = l->pos;
            if (type == Ident) {
                size_t size = end_pos - start_pos;
                vstr s = vstr_new_len(size);
                s = vstr_push_string_len(s, val, size);
                tok.value = s;
            }
            tok.type = type;
            return tok;
        } else if (is_digit(l->ch)) {
            size_t size, end_pos, start_pos = l->pos - 1;
            const char* val = l->input + start_pos;
            vstr s;
            lexer_read_int(l);
            end_pos = l->pos;
            size = end_pos - start_pos;
            s = vstr_new_len(size);
            s = vstr_push_string_len(s, val, size);
            tok.type = Int;
            tok.value = s;
            return tok;
        } else {
            tok.type = Illegal;
        }
        break;
    }
    lexer_read_char(l);
    return tok;
}

static inline int is_char(char ch) {
    return (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_');
}

static inline int is_digit(char ch) { return '0' <= ch && ch <= '9'; }

static inline void skip_white_space(Lexer* l) {
    while (l->ch == ' ' || l->ch == '\r' || l->ch == '\n' || l->ch == '\t') {
        lexer_read_char(l);
    }
}

static void lexer_read_ident(Lexer* l) {
    while (is_char(l->ch)) {
        lexer_read_char(l);
    }
}

static void lexer_read_int(Lexer* l) {
    while (is_digit(l->ch)) {
        lexer_read_char(l);
    }
}

static char peek_char(Lexer* l) {
    if (l->pos >= l->input_len) {
        return 0;
    } else {
        return l->input[l->pos];
    }
}
