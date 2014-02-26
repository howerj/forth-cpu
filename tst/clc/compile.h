#include <stdint.h>
typedef enum { typeCon, typeId, typeLabel, typeOpr } nodeEnum;

/* constants */
typedef struct {
    int value;                  /* value of constant */
} conNodeType;

/* identifiers */
typedef struct {
    int i;                      /* identifiers index */
    char *s;                    /* identifier name */
} idNodeType;

typedef struct {
  char *s;                      /* goto label name */
} labelNodeType;

/* operators */
typedef struct {
    int oper;                   /* operator */
    int nops;                   /* number of operands */
    struct nodeTypeTag **op;	  /* operands */
} oprNodeType;

typedef struct nodeTypeTag {
    nodeEnum type;              /* type of node */

    union {
        conNodeType con;        /* constants */
        idNodeType id;          /* identifiers */
        labelNodeType label;    /* labels */
        oprNodeType opr;        /* operators */
    };
} nodeType;

#define MAX_SYMBOL_SIZE (64u)
#define MAX_MEMORY      (8192u)
