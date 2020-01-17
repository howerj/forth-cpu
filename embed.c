/* H2 Forth Virtual Machine, Richard James Howe, 2017-2019, MIT License */
#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STK         (64)
#define USE_HEX_IN  (0)
#define USE_HEX_OUT (1)

typedef uint16_t m_t;
typedef  int16_t s_t;
typedef uint32_t d_t;
typedef struct forth_t { m_t m[32768], vs[STK], rs[STK], pc, t, rp, sp, cpu; } forth_t;
typedef int (*cb)(forth_t *h, void *param); 

static inline size_t cells(forth_t const * const h) { 
	assert(h); 
	return sizeof(h->m)/sizeof(h->m[0]); 
}

static void die(const char *fmt, ...) {
	assert(fmt);
	va_list arg;
	va_start(arg, fmt);
	vfprintf(stderr, fmt, arg);
	va_end(arg);
	fputc('\n', stderr);
	exit(EXIT_FAILURE);
}

static FILE *fopen_or_die(const char *file, const char *mode) {
	assert(file && mode);
	FILE *h = NULL;
	errno = 0;
	if (!(h = fopen(file, mode)))
		die("file open %s (mode %s) failed: %s", file, mode, strerror(errno));
	return h;
}

static int save(forth_t *h, const char *name, const size_t start, const size_t length) {
	assert(h);
	if (!name || !(((length - start) <= length) && ((start + length) <= cells(h))))
		return -69; /* open-file IOR */
	FILE *out = fopen(name, "wb");
	if (!out)
		return -69; /* open-file IOR */
	int r = 0;
	for (size_t i = start; i < length; i++) {
		if (USE_HEX_OUT) {
			fprintf(out, "%02x%02x\n", ((unsigned)(h->m[i] >> 8) & 255u), (unsigned)(h->m[i] >> 0) & 255u);
		} else {
			if (fputc(h->m[i]&255, out) < 0 || fputc(h->m[i]>>8, out) < 0)
				r = -76; /* write-file IOR */
		}
	}
	return fclose(out) < 0 ? -62 /* close-file IOR */ : r;
}

static int load(forth_t *h, const char *name) {
	assert(h && name);
	FILE *input = fopen_or_die(name, "rb");
	long r = 0;
	for (size_t i = 0; i < cells(h); i++, r = i) {
		if (USE_HEX_IN) {
			unsigned d = 0;
			if (fscanf(input, "%x", &d) != 1) {
				r = -1;
				break;
			}
			h->m[i] = d;
		} else {
			int c1 = 0, c2 = 0;
			if ((c1 = fgetc(input)) < 0 || (c2 = fgetc(input)) < 0)
				break;
			h->m[i] = ((c1 & 0xffu)) | ((c2 & 0xffu) << 8u);
		}
	}
	fclose(input);
	return r < 64 ? -70 /* read-file IOR */ : 0; /* minimum size checks, 128 bytes */
}

static inline void trace(FILE *out, m_t opt, m_t *m, m_t pc, m_t instruction, m_t t, m_t rp, m_t sp) {
	(void)m;
	if (!(opt & 1))
		return;
	fprintf(out, "[ %4x %4x %4x %2x %2x ]\n", pc-1, instruction, t, rp, sp);
}

static int run(forth_t *h, m_t opt, FILE *in, FILE *out, const char *block, cb func, void *param) {
	assert(h && in && out);
	static const m_t delta[] = { 0, 1, -2, -1 };
	const m_t l = cells(h);
	m_t *const m = h->m, *const vs = h->vs, *const rs = h->rs;
	m_t pc = h->pc, t = h->t, rp = h->rp, sp = h->sp, cpu = h->cpu, r = 0;
	for (;;) {
		const m_t instruction = m[pc++];
		trace(out, opt, m, pc, instruction, t, rp, sp);
		if ((r = -!(sp < STK && rp < STK && pc < l))) /* critical error */
			goto finished;
		if (0x8000 & instruction) { /* literal */
			vs[++sp % STK] = t;
			t = instruction & 0x7FFF;
		} else if ((0xE000 & instruction) == 0x6000) { /* ALU */
			m_t n = vs[sp % STK], T = t;
			pc = (instruction & 0x10) ? rs[rp % STK] >> 1 : pc;
			switch ((instruction >> 8u) & 0x1f) {
			case  0:                           break;
			case  1: T = n;                    break;
			case  2: T += n;                   break;
			case  3: T &= n;                   break;
			case  4: T |= n;                   break;
			case  5: T ^= n;                   break;
			case  6: T = ~t;                   break;
			case  7: T = -(t == n);            break;
			case  8: T = -((s_t)n < (s_t)t);   break;
			case  9: T = n >> t;               break;
			case 10: T--;                      break;
			case 11: T = rs[rp % STK];         break;
			case 12: T = m[(t>>1)%l];          break;
			case 13: T = n << t;               break;
			case 14: T = sp;                   break;
			case 15: T = -(n < t);             break;
			case 16: cpu = t; T  = n;          break;
			case 17: T = cpu;                  break;
			case 18: T = rp;                   break;
			case 19: T = -(t == 0);            break;
			case 20: T = 0xD1ED; /* CPU-ID */  break;
			case 21: T = instruction & 0x7FFF; break; /* lit: internal use only */
			/* 22: UNUSED */
			/* 23: UNUSED */
			/* 24: UNUSED */
			/* Hosted instructions only */
			case 25: if (opt & 2) { T = save(h, block, n>>1, ((d_t)T+1)>>1); } break;
			case 26: if (opt & 2) { T = fputc(t, out); }       break;
			case 27: if (opt & 2) { T = fgetc(in); }           break;
			case 28: if (opt & 2) { if (rs[rp % STK]) { rs[rp % STK] = 0; sp--; r = t; t = n; goto finished; }; T = t; } break;
			case 29: if (opt & 2) { T = opt; opt = T; } break;
			case 30: if (opt & 2) { if (func) { T = func(h, param); } else { pc=4; T=21; } } break;
			/* 31: UNUSED */
			default: if (opt & 2) { pc=4; T=21; } break;
			}
			sp += delta[ instruction       & 0x3];
			rp += delta[(instruction >> 2) & 0x3];
			if (instruction & 0x80)
				vs[sp % STK] = t;
			if (instruction & 0x40)
				rs[rp % STK] = t;
			if (instruction & 0x20)
				m[(t >> 1) % l] = n;
			t = T;
		} else if (0x4000 & instruction) { /* call */
			rs[++rp % STK] = pc << 1;
			pc      = instruction & 0x1FFF;
		} else if (0x2000 & instruction) { /* 0branch */
			pc = !t ? instruction & 0x1FFF : pc;
			t  = vs[sp-- % STK];
		} else { /* branch */
			pc = instruction & 0x1FFF;
		}
	}
finished: h->pc = pc, h->t = t, h->rp = rp, h->sp = sp, h->cpu = cpu;
	return (s_t)r;
}

int main(int argc, char **argv) {
	static forth_t h = { .m = { 0 }, .vs = { 0 }, .rs = { 0 } };
	if (argc > 4)
		die("usage: %s [in.blk] [out.blk] [file.fth]", argv[0]);
	if (load(&h, argc < 2 ? "embed.blk" : argv[1]) < 0)
		die("embed: load failed");
	FILE *in = argc <= 3 ? stdin : fopen_or_die(argv[3], "rb");
	if (run(&h, 2, in, stdout, argc < 3 ? NULL : argv[2], NULL, NULL))
		die("embed: run failed");
	return 0; /* exiting takes care of closing files, freeing memory */
}

