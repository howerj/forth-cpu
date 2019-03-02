/* Embed Forth Virtual Machine, Richard James Howe, 2017-2019, MIT License */
#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uint16_t m_t;
typedef  int16_t s_t;
typedef uint32_t d_t;
typedef struct forth_t { m_t m[32768]; } forth_t;

static inline size_t embed_cells(forth_t const * const h) { assert(h); return h->m[5]; } /* count in cells, not bytes */
static inline size_t max_size_t(size_t a, size_t b)       { return a > b ? a : b; }

static void embed_die(const char *fmt, ...) {
	assert(fmt);
	va_list arg;
	va_start(arg, fmt);
	vfprintf(stderr, fmt, arg);
	va_end(arg);
	fputc('\n', stderr);
	exit(EXIT_FAILURE);
}

static FILE *embed_fopen_or_die(const char *file, const char *mode) {
	assert(file && mode);
	FILE *h = NULL;
	errno = 0;
	if (!(h = fopen(file, mode)))
		embed_die("file open %s (mode %s) failed: %s", file, mode, strerror(errno));
	return h;
}

static forth_t *embed_new(void) {
	forth_t *h = calloc(1, sizeof(*h));
	if (!h)
		embed_die("allocation (of %u) failed", (unsigned)sizeof(*h));
	return h;
}

static int save(forth_t *h, const char *name, const size_t start, const size_t length) {
	assert(h);
	if (!name || !(((length - start) <= length) && ((start + length) <= embed_cells(h))))
		return -69; /* open-file IOR */
	FILE *out = fopen(name, "wb");
	if (!out)
		return -69; /* open-file IOR */
	int r = 0;
	for (size_t i = start; i < length; i++)
		if (fputc(h->m[i]&255, out) < 0 || fputc(h->m[i]>>8, out) < 0)
			r = -76; /* write-file IOR */
	return fclose(out) < 0 ? -62 /* close-file IOR */ : r;
}


static int embed_load(forth_t *h, const char *name) {
	assert(h && name);
	FILE *input = embed_fopen_or_die(name, "rb");
	long r = 0;
	for (size_t i = 0; i < max_size_t(64, embed_cells(h)); i++, r = i) {
		int c1 = 0, c2 = 0;
		assert(embed_cells(h) <= 0x8000);
		if ((c1 = fgetc(input)) < 0 || (c2 = fgetc(input)) < 0)
			break;
		h->m[i] = ((c1 & 0xffu)) | ((c2 & 0xffu) << 8u);
	}
	fclose(input);
	return r < 64 ? -70 /* read-file IOR */ : 0; /* minimum size checks, 128 bytes */
}

static inline void trace(FILE *out, m_t opt, m_t *m, m_t pc, m_t instruction, m_t t, m_t rp, m_t sp) {
	if (!(opt & 1))
		return;
	fprintf(out, "[ %4x %4x %4x %2x %2x ]\n", pc-1, instruction, t, m[2]-rp, sp-m[3]);
}

static int embed_forth(forth_t *h, FILE *in, FILE *out, const char *block) {
	assert(h && in && out);
	static const m_t delta[] = { 0, 1, -2, -1 };
	const m_t l = embed_cells(h);
	m_t * const m = h->m;
	m_t pc = m[0], t = m[1], rp = m[2], sp = m[3], r = 0, opt = 0;
	for (d_t d;;) {
		const m_t instruction = m[pc++];
		trace(out, opt, m, pc, instruction, t, rp, sp);
		if ((r = -!(sp < l && rp < l && pc < l))) /* critical error */
			goto finished;
		if (0x8000 & instruction) { /* literal */
			m[++sp] = t;
			t       = instruction & 0x7FFF;
		} else if ((0xE000 & instruction) == 0x6000) { /* ALU */
			m_t n = m[sp], T = t;
			pc = (instruction & 0x10) ? m[rp] >> 1 : pc;
			switch((instruction >> 8u) & 0x1f) {
			case  0:                           break;
			case  1: T = n;                    break;
			case  2: T = m[rp];                break;
			case  3: T = m[(t>>1)%l];          break;
			case  4: m[(t>>1)%l] = n; T = m[--sp]; break;
			case  5: d = (d_t)t + n; T = d >> 16; m[sp] = d; n = d; break;
			case  6: d = (d_t)t * n; T = d >> 16; m[sp] = d; n = d; break;
			case  7: T &= n;                   break;
			case  8: T |= n;                   break;
			case  9: T ^= n;                   break;
			case 10: T = ~t;                   break;
			case 11: T--;                      break;
			case 12: T = -(t == 0);            break;
			case 13: T = -(t == n);            break;
			case 14: T = -(n < t);             break;
			case 15: T = -((s_t)n < (s_t)t);   break;
			case 16: T = n >> t;               break;
			case 17: T = n << t;               break;
			case 18: T = sp << 1;              break;
			case 19: T = rp << 1;              break;
			case 20: sp = t >> 1;              break;
			case 21: rp = t >> 1; T = n;       break;
			case 22: T = save(h, block, n>>1, ((d_t)T+1)>>1); break;
			case 23: T = fputc(t, out);        break;
			case 24: m[++sp] = t; T = fgetc(in); t = T; n = 0;    break; /* n = blocking status */
			case 25: if (t) { d = m[--sp]|((d_t)n<<16); T=d/t; t=d%t; n=t; } else { pc=4; T=10; } break;
			case 26: if (t) { T=(s_t)n/t; t=(s_t)n%t; n=t; } else { pc=4; T=10; } break;
			case 27: if (m[rp]) { m[rp] = 0; sp--; r = t; t = n; goto finished; }; T = t; break;
			/* 28 is virtual machine callback mechanism, not implemented here */
			case 29: T = opt; opt = t; break;
			default: pc=4; T=21; break;
			}
			sp += delta[ instruction       & 0x3];
			rp -= delta[(instruction >> 2) & 0x3];
			if (instruction & 0x80)
				m[sp] = t;
			if (instruction & 0x40)
				m[rp] = t;
			t = (instruction & 0x20) ? n : T;
		} else if (0x4000 & instruction) { /* call */
			m[--rp] = pc << 1;
			pc      = instruction & 0x1FFF;
		} else if (0x2000 & instruction) { /* 0branch */
			pc = !t ? instruction & 0x1FFF : pc;
			t  = m[sp--];
		} else { /* branch */
			pc = instruction & 0x1FFF;
		}
	}
finished: m[0] = pc, m[1] = t, m[2] = rp, m[3] = sp;
	return (s_t)r;
}

int main(int argc, char **argv) {
	forth_t *h = embed_new();
	if (argc > 4)
		embed_die("usage: %s [in.blk] [out.blk] [file.fth]", argv[0]);
	if (embed_load(h, argc < 2 ? "embed.blk" : argv[1]) < 0)
		embed_die("embed: load failed");
	FILE *in = argc <= 3 ? stdin : embed_fopen_or_die(argv[3], "rb");
	if (embed_forth(h, in, stdout, argc < 3 ? NULL : argv[2]))
		embed_die("embed: run failed");
	return 0; /* exiting takes care of closing files, freeing memory */
}

