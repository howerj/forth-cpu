#include <stdio.h>
#include "compile.h"
#include "y.tab.h"

static int lbl;

int ex(nodeType *p) {
    int lbl1, lbl2;

    if (!p) return 0;
    switch(p->type) {
    case typeCon:       
        printf("\t%d\n", p->con.value); 
        break;
    case typeId:        
        printf("\tpush %s\n", p->id.s);
        break;
    case typeLabel:
        printf("%s\n", p->id.s);
        break;
    case typeOpr:
        switch(p->opr.oper) {
        case GOTO:
            printf("\tjump\t");
            ex(p->opr.op[0]);
            break;
        case VARDECL:
            printf("alloc\t");
            ex(p->opr.op[0]);
            break;
        case RETURN:
            printf("\texit\n");
            break;
        case WHILE:
            printf("L%03d:\n", lbl1 = lbl++);
            ex(p->opr.op[0]);
            printf("\tjumpc\tL%03d\n", lbl2 = lbl++);
            ex(p->opr.op[1]);
            printf("\tjump\tL%03d\n", lbl1);
            printf("L%03d:\n", lbl2);
            break;
        case IF:
            ex(p->opr.op[0]);
            if (p->opr.nops > 2) {
                /* if else */
                printf("\tjumpc\tL%03d\n", lbl1 = lbl++);
                ex(p->opr.op[1]);
                printf("\tjump\tL%03d\n", lbl2 = lbl++);
                printf("L%03d:\n", lbl1);
                ex(p->opr.op[2]);
                printf("L%03d:\n", lbl2);
            } else {
                /* if */
                printf("\tjumpc\tL%03d\n", lbl1 = lbl++);
                ex(p->opr.op[1]);
                printf("L%03d:\n", lbl1);
            }
            break;
        case PRINT:     
            ex(p->opr.op[0]);
            printf("\tprint\n");
            break;
        case '=':       
            ex(p->opr.op[1]);
            printf("\tpop\t%s\n", p->opr.op[0]->id.s);
            break;
        case UINVERT:    
            ex(p->opr.op[0]);
            printf("\tinvert\n");
            break;
        default:
            ex(p->opr.op[0]);
            ex(p->opr.op[1]);
            switch(p->opr.oper) {
            case '+':   printf("\tadd\n"); break;
            case '-':   printf("\tsub\n"); break; 
            case '*':   printf("\tmul\n"); break;
            case '/':   printf("\tdiv\n"); break;
            case '&':   printf("\tand\n"); break;
            case '|':   printf("\tor\n"); break;
            case '^':   printf("\txor\n"); break;
            case LS:    printf("\tlshift\n"); break;
            case RS:    printf("\trshift\n"); break;
            case '<':   printf("\tcompLT\n"); break;
            case '>':   printf("\tcompGT\n"); break;
            case GE:    printf("\tcompGE\n"); break;
            case LE:    printf("\tcompLE\n"); break;
            case NE:    printf("\tcompNE\n"); break;
            case EQ:    printf("\tcompEQ\n"); break;
            }
        }
    }
    return 0;
}
