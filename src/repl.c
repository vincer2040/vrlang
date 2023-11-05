#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "util.h"
#include "vstr.h"
#include <stdio.h>
#include <string.h>

const char* prompt = ">>> ";

static int print_errors(Parser* p);

int repl(void) {

    for (;;) {
        vstr line;
        size_t line_len;
        Lexer l;
        Parser p;
        Program* program;
        vstr program_str;
        int errors;

        printf("%s", prompt);
        line = read_line();
        line_len = vstr_len(line);
        if ((line_len == 4) && (strncmp(line, "exit", 4) == 0)) {
            vstr_delete(line);
            goto done;
        }

        l = lexer_new(line, line_len);
        p = parser_new(&l);
        program = parse(&p);

        errors = print_errors(&p);
        if (errors == -1) {
            parser_free(&p);
            program_free(program);
            vstr_delete(line);
            continue;
        }
        program_str = program_string(program);
        printf("%s\n", program_str);

        program_free(program);
        vstr_delete(program_str);
        vstr_delete(line);
    }

done:

    return 0;
}

static int print_errors(Parser* p) {
    size_t i, len = p->errors.len;
    if (len) {
        for (i = 0; i < len; ++i) {
            vstr e = p->errors.errors[i];
            printf("%s\n", e);
        }
        return -1;
    }
    return 0;
}
