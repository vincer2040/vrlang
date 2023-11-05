#include "vstr.h"
#include <stdio.h>

vstr read_line(void) {
    vstr s = vstr_new();
    int c;
    while ((c = getchar()) != '\n') {
        s = vstr_push_char(s, c);
    }
    return s;
}
