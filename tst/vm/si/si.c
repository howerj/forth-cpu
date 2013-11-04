/** @file si.c 
 *
 * @brief simple interpreter for eventual porting to
 * the H2 CPU writte in VHDL
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 *
 * */

#include <stdio.h>
#include <stdint.h>

#define DICMAX  (8192u)
#define BUFLEN  (512u)
#define PREVWRD (dic[4])
#define DICPTR  (dic[5])
/** djb2*/
uint16_t hash (unsigned char *str)
{
  uint16_t hash = 5381u, c;
  while ((c = *str++))
    hash = ((hash << 5u) + hash) + c;	/* hash * 33 + c */
  return hash;
}

uint16_t getword(void){
  unsigned char s[BUFLEN]={0};
  scanf("%s",s);
  return hash(s);
}

uint16_t findword(uint16_t dic[], uint16_t head, uint16_t hash){
  for(;head;head = dic[head]){
    if(dic[head + 1] == hash)
      return head;
  }
  return 0;
}

#define XMACRO_PRIMITIVE\
  X(nop,      "nop"),\
  X(call,     "call"),\
  X(retn,     "retn"),\
  X(read,     "read"),\
  X(define,   ":"),\
  X(pushlit,  "dolit"),\
  X(getlit,   "lit"),\
  X(add,      "+"),\
  X(sub,      "-"),\
  X(mul,      "*"),\
  X(div,      "/"),\
  X(jmpc,     "jmpc"),\
  X(jmp,      "jmp"),\
  X(load,     "@"),\
  X(store,    "!"),\
  X(equal,    "="),\
  X(less,     "<"),\
  X(key,      "key"),\
  X(emit,     "emit"),\
  X(dup,      "dup"),\
  X(drop,     "drop"),\
  X(swap,     "swap"),\
  X(tor,      ">r"),\
  X(fromr,    "r>"),\
  X(halt,     "halt"),\
  X(LASTP,    "LASTP")

#define X(a, b) a
typedef enum{
  XMACRO_PRIMITIVE  
} primitives_e;
#undef X

#define X(a, b) b
char *primitive_s[] = {
  XMACRO_PRIMITIVE  
};
#undef X


void addword(uint16_t primitive, uint16_t hash, uint16_t dic[]){
  dic[DICPTR]    = PREVWRD;
  dic[DICPTR+1]  = hash; 
  dic[DICPTR+2]  = primitive; 

  PREVWRD = DICPTR;
  DICPTR+=3; /** dicp */
}

int main(void){
  uint16_t dic[DICMAX] = {0}, pc = 512;
  uint16_t var[32], ret[32], tos = 0, varp = 0, retp = 0;
  uint16_t *prevwrd = &dic[4], *dicp = &dic[5];
  uint16_t tmp;

  dic[5]=16;
  dic[4]=0;
  uint16_t i,j;
  for(i = 0; i< LASTP; i++){
    addword(i,hash((unsigned char *)primitive_s[i]),dic);
  }
  for(i = 0; i< 128; i++){
    if(i==0)
      printf("(%d)\t%05hu\t",dic[i],i);
    else if(i%4==0)
      printf("%05hu\n(%d)\t",dic[i],i);
    else
      printf("%05hu\t",dic[i]);
  }
  putchar('\n');

  dic[pc+1] = read;
  dic[pc+2] = jmp;
  dic[pc+3] = pc;
  while(pc++<DICMAX){
    tmp = dic[pc];
EXECUTE:
    switch(tmp){
    case nop: break;
    case call: ret[retp++] = pc + 1; pc = dic[pc]; break;
    case retn: pc = ret[--retp]; break;
    case read: /* getword(), findword(), execute if found*/ 
               tmp = findword(dic,dic[4],getword());
               tmp = dic[tmp+2];
               goto EXECUTE;
    case define: break;
    case pushlit: var[varp++] = tos; tos = dic[pc++]; break;
    case getlit: var[varp++] = tos; scanf("%hu", &tos); break;
    case add: tos = tos + var[--varp]; break;
    case sub: tos = tos - var[--varp]; break;
    case mul: tos = tos * var[--varp]; break;
    case div: if(var[varp]) tos = tos / var[varp++]; break;
    case jmpc: if(!tos) pc = dic[pc+1]; else pc++; break;
    case jmp: pc = dic[pc+1]; break;
    case load: tos = dic[tos]; break;
    case store: dic[tos] = var[--varp]; tos = var[--varp]; break;
    case equal: tos = tos == var[--varp]; break;
    case less: tos = tos < var[--varp]; break;
    case key:  var[++varp] = tos; tos = getchar(); break;
    case emit: putchar(tos); tos = var[--varp]; break;
    case dup: var[++varp] = tos; break;
    case drop: tos = var[varp--]; break;
    case swap: break;
    case tor: break;
    case fromr: break;
    case halt: break;
    default:
        fprintf(stderr,"not a primitive.\n");
        return 1;
    }
  }

  return 0;
}
