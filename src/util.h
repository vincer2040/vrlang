#ifndef __UTIL_H__

#define __UTIL_H__

#define INITIAL_CAP 32

#define UNUSED(v) ((void)v)

#define unreachable                                                            \
    do {                                                                       \
        assert(0 && "unreachable");                                            \
    } while (0)


#endif /* __UTIL_H__ */
