#ifndef H2_H
#define H2_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

/**@note STK_SIZE is fixed to 64, but h2.vhd allows for the instantiation of
 * CPUs with different stack sizes, within reasonable limits, so long as they
 * are a power of 2. */

#define MAX_CORE             (8192u)
#define STK_SIZE             (64u)
#define START_ADDR           (0u)

#ifndef H2_CPU_ID_SIMULATION
#ifdef NO_MAIN
#define H2_CPU_ID_SIMULATION (0xD1ED)
#else
#define H2_CPU_ID_SIMULATION (0xDEADu)
#endif
#endif

#define H2_CPU_ID_VHDL       (0xCAFEu)

#define VGA_INIT_FILE        ("text.hex")  /**< default name for VGA screen */
#define FLASH_INIT_FILE      ("nvram.blk") /**< default file for flash initialization */

typedef struct {
	size_t length;
	uint16_t *points;
} break_point_t;

typedef struct {
	uint16_t core[MAX_CORE]; /**< main memory */
	uint16_t rstk[STK_SIZE]; /**< return stack */
	uint16_t dstk[STK_SIZE]; /**< variable stack */
	uint16_t pc;  /**< program counter */
	uint16_t tos; /**< top of stack */
	uint16_t rp;  /**< return stack pointer */
	uint16_t sp;  /**< variable stack pointer */
	bool     ie;  /**< interrupt enable */
	unsigned time; /**< cycles run for */

	break_point_t bp; /**< list of break points */
	uint16_t rpm; /**< maximum value of rp ever encountered */
	uint16_t spm; /**< maximum value of sp ever encountered */
} h2_t; /**< state of the H2 CPU */

typedef enum {
	SYMBOL_TYPE_LABEL,
	SYMBOL_TYPE_CALL,
	SYMBOL_TYPE_CONSTANT,
	SYMBOL_TYPE_VARIABLE,
} symbol_type_e;

typedef struct {
	symbol_type_e type;
	char *id;
	uint16_t value;
	bool hidden;
} symbol_t;

typedef struct {
	size_t length;
	symbol_t **symbols;
} symbol_table_t;

#define CLOCK_SPEED_HZ             (100000000ULL)

#define VGA_BUFFER_LENGTH          (1 << 13)
#define VGA_WIDTH                  (80)
#define VGA_HEIGHT                 (40)
#define VGA_AREA                   (VGA_WIDTH * VGA_HEIGHT)

#define VGA_CTL_B_BIT              (0)
#define VGA_CTL_G_BIT              (1)
#define VGA_CTL_R_BIT              (2)
#define VGA_CUR_MODE_BIT           (3)
#define VGA_CUR_BLINK_BIT          (4)
#define VGA_CUR_EN_BIT             (5)
#define VGA_EN_BIT                 (6)
#define VGA_SCREEN_SELECT_BIT      (7)

#define VGA_CTL_B                  (1 << VGA_CTL_B_BIT)
#define VGA_CTL_G                  (1 << VGA_CTL_G_BIT)
#define VGA_CTL_R                  (1 << VGA_CTL_R_BIT)
#define VGA_CUR_MODE               (1 << VGA_CUR_MODE_BIT)
#define VGA_CUR_BLINK              (1 << VGA_CUR_BLINK_BIT)
#define VGA_CUR_EN                 (1 << VGA_CUR_EN_BIT)
#define VGA_EN                     (1 << VGA_EN_BIT)
#define VGA_SCREEN_SELECT          (1 << VGA_SCREEN_SELECT_BIT)

#define TIMER_ENABLE_BIT           (15)
#define TIMER_RESET_BIT            (14)
#define TIMER_INTERRUPT_ENABLE_BIT (13)
#define TIMER_ENABLE               (1 << TIMER_ENABLE_BIT)
#define TIMER_RESET                (1 << TIMER_RESET_BIT)
#define TIMER_INTERRUPT_ENABLE     (1 << TIMER_INTERRUPT_ENABLE_BIT)

#define UART_FIFO_DEPTH            (8)
#define UART_BAUD_RATE             (115200)

#define UART_RX_FIFO_EMPTY_BIT     (8)
#define UART_RX_FIFO_FULL_BIT      (9)
#define UART_RX_RE_BIT             (10)
#define UART_TX_FIFO_EMPTY_BIT     (11)
#define UART_TX_FIFO_FULL_BIT      (12)
#define UART_TX_WE_BIT             (13)

#define UART_RX_FIFO_EMPTY         (1 << UART_RX_FIFO_EMPTY_BIT)
#define UART_RX_FIFO_FULL          (1 << UART_RX_FIFO_FULL_BIT)
#define UART_RX_RE                 (1 << UART_RX_RE_BIT)
#define UART_TX_FIFO_EMPTY         (1 << UART_TX_FIFO_EMPTY_BIT)
#define UART_TX_FIFO_FULL          (1 << UART_TX_FIFO_FULL_BIT)
#define UART_TX_WE                 (1 << UART_TX_WE_BIT)

#define PS2_NEW_CHAR_BIT           (8)
#define PS2_NEW_CHAR               (1 << PS2_NEW_CHAR_BIT)

#define CHIP_MEMORY_SIZE           (8*1024*1024) /*NB. size in WORDs not bytes! */
#define FLASH_MASK_ADDR_UPPER_MASK (0x1ff)

#define FLASH_CHIP_SELECT_BIT      (10)
#define SRAM_CHIP_SELECT_BIT       (11)
#define FLASH_MEMORY_WAIT_BIT      (12)
#define FLASH_MEMORY_RESET_BIT     (13)
#define FLASH_MEMORY_OE_BIT        (14)
#define FLASH_MEMORY_WE_BIT        (15)

#define FLASH_CHIP_SELECT          (1 << FLASH_CHIP_SELECT_BIT)
#define SRAM_CHIP_SELECT           (1 << SRAM_CHIP_SELECT_BIT)
#define FLASH_MEMORY_WAIT          (1 << FLASH_MEMORY_WAIT_BIT)
#define FLASH_MEMORY_RESET         (1 << FLASH_MEMORY_RESET_BIT)
#define FLASH_MEMORY_OE            (1 << FLASH_MEMORY_OE_BIT)
#define FLASH_MEMORY_WE            (1 << FLASH_MEMORY_WE_BIT)

#define FLASH_BLOCK_MAX            (130)

typedef enum {
	FLASH_UNLOCKED,
	FLASH_LOCKED,
	FLASH_LOCKED_DOWN,
} flash_lock_t;

typedef struct {
	unsigned cycle;
	unsigned mode;
	unsigned we;
	unsigned cs;
	uint8_t  status;
	uint32_t arg1_address, arg2_address;
	uint16_t data;
	uint16_t nvram[CHIP_MEMORY_SIZE];
	uint8_t  locks[FLASH_BLOCK_MAX];
} flash_t;

typedef enum { /**@warning do not change the order or insert elements */
	BLACK,
	RED,
	GREEN,
	YELLOW,
	BLUE,
	MAGENTA,
	CYAN,
	WHITE,
} color_t;

typedef enum {
	TERMINAL_NORMAL_MODE,
	TERMINAL_CSI,
	TERMINAL_COMMAND,
	TERMINAL_NUMBER_1,
	TERMINAL_NUMBER_2,
	TERMINAL_DECTCEM,
	TERMINAL_STATE_END,
} terminal_state_t;

typedef struct {
	unsigned bold:          1;
	unsigned under_score:   1;
	unsigned blink:         1;
	unsigned reverse_video: 1;
	unsigned conceal:       1;
	unsigned foreground_color: 3;
	unsigned background_color: 3;
} vt100_attribute_t;

#define VT100_MAX_SIZE (8192)

typedef struct {
	size_t cursor;
	size_t cursor_saved;
	unsigned n1, n2;
	unsigned height;
	unsigned width;
	unsigned size;
	terminal_state_t state;
	bool blinks;
	bool cursor_on;
	vt100_attribute_t attribute;
	vt100_attribute_t attributes[VT100_MAX_SIZE];
	uint8_t m[VT100_MAX_SIZE];
	uint8_t command_index;
} vt100_t;

typedef struct {
	uint8_t leds;
	vt100_t vt100;

	uint16_t timer_control;
	uint16_t timer;

	uint16_t irc_mask;

	uint8_t uart_getchar_register, ps2_getchar_register;

	uint16_t led_7_segments;

	uint16_t switches;
	uint16_t switches_previous;

	uint16_t vram[CHIP_MEMORY_SIZE];
	uint16_t mem_control;
	uint16_t mem_addr_low;
	uint16_t mem_dout;
	flash_t flash;

	bool wait;
	bool interrupt;
	uint8_t interrupt_selector;
} h2_soc_state_t;

typedef uint16_t (*h2_io_get)(h2_soc_state_t *soc, uint16_t addr, bool *debug_on);
typedef void     (*h2_io_set)(h2_soc_state_t *soc, uint16_t addr, uint16_t value, bool *debug_on);
typedef void     (*h2_io_update)(h2_soc_state_t *soc);

typedef struct {
	h2_io_get in;
	h2_io_set out;
	h2_io_update update;
	h2_soc_state_t *soc;
} h2_io_t;

typedef enum {
	iUart         = 0x4000,
	iVT100        = 0x4002,
	iTimerDin     = 0x4004,
	iSwitches     = 0x4006,
	iMemDin       = 0x4008,
} h2_input_addr_t;

typedef enum {
	oUart         = 0x4000,
	oVT100        = 0x4002,
	oTimerCtrl    = 0x4004,
	oLeds         = 0x4006,
	oMemDout      = 0x4008,
	oMemControl   = 0x400A,
	oMemAddrLow   = 0x400C,
	o7SegLED      = 0x400E,
	oIrcMask      = 0x4010,
} h2_output_addr_t;

typedef enum {
	isrEntry,
	isrRxFifoNotEmpty,
	isrRxFifoFull,
	isrTxFifoNotEmpty,
	isrTxFifoFull,
	isrKbdNew,
	isrTimer,
	isrDPadButton,
} h2_interrupt_address_t;

void *allocate_or_die(size_t length);
FILE *fopen_or_die(const char *file, const char *mode);

h2_t *h2_new(uint16_t start_address);
void h2_free(h2_t *h);
int h2_load(h2_t *h, FILE *hexfile);
int h2_save(h2_t *h, FILE *output, bool full);
/**@todo the interface to h2_run needs simplifying and rethinking */
int h2_run(h2_t *h, h2_io_t *io, FILE *output, unsigned steps, symbol_table_t *symbols, bool run_debugger, FILE *trace);

uint16_t h2_io_memory_read_operation(h2_soc_state_t *soc);
void soc_print(FILE *out, h2_soc_state_t *soc);
h2_soc_state_t *h2_soc_state_new(void);
void h2_soc_state_free(h2_soc_state_t *soc);
h2_io_t *h2_io_new(void);
void h2_io_free(h2_io_t *io);

int binary_memory_save(FILE *output, uint16_t *p, size_t length);
int binary_memory_load(FILE *input, uint16_t *p, size_t length);
int nvram_save(h2_io_t *io, const char *name);
int nvram_load_and_transfer(h2_io_t *io, const char *name, bool transfer_to_sram);

typedef uint8_t fifo_data_t;

typedef struct {
	size_t head;
	size_t tail;
	size_t size;
	fifo_data_t *buffer;
} fifo_t;

fifo_t *fifo_new(size_t size);
void fifo_free(fifo_t *fifo);
bool fifo_is_full(fifo_t * fifo);
bool fifo_is_empty(fifo_t * fifo);
size_t fifo_count(fifo_t * fifo);
size_t fifo_push(fifo_t * fifo, fifo_data_t data);
size_t fifo_pop(fifo_t * fifo, fifo_data_t * data);

/** @warning LOG_FATAL level kills the program */
#define X_MACRO_LOGGING\
	X(LOG_MESSAGE_OFF,  "")\
	X(LOG_FATAL,        "fatal")\
	X(LOG_ERROR,        "error")\
	X(LOG_WARNING,      "warning")\
	X(LOG_NOTE,         "note")\
	X(LOG_DEBUG,        "debug")\
	X(LOG_ALL_MESSAGES, "any")

typedef enum {
#define X(ENUM, NAME) ENUM,
	X_MACRO_LOGGING
#undef X
} log_level_e;

extern log_level_e log_level;

int logger(log_level_e level, const char *func,
		const unsigned line, const char *fmt, ...);

#define fatal(FMT, ...)   logger(LOG_FATAL,   __func__, __LINE__, FMT, ##__VA_ARGS__)
#define error(FMT, ...)   logger(LOG_ERROR,   __func__, __LINE__, FMT, ##__VA_ARGS__)
#define warning(FMT, ...) logger(LOG_WARNING, __func__, __LINE__, FMT, ##__VA_ARGS__)
#define note(FMT, ...)    logger(LOG_NOTE,    __func__, __LINE__, FMT, ##__VA_ARGS__)
#define debug(FMT, ...)   logger(LOG_DEBUG,   __func__, __LINE__, FMT, ##__VA_ARGS__)

int memory_load(FILE *input, uint16_t *p, size_t length);
int memory_save(FILE *output, uint16_t *p, size_t length);

#define BACKSPACE (8)
#define ESCAPE    (27)
#define DELETE    (127)  /* ASCII delete */

void vt100_update(vt100_t *t, uint8_t c);

typedef struct {
	size_t size;
	size_t next;
	void *data;
} buffer_t;

buffer_t *buffer_new(void);
void buffer_reserve(buffer_t *b, size_t bytes);
buffer_t *buffer_copy(buffer_t *b);
buffer_t *buffer_load(FILE *input);
int buffer_save(buffer_t *b, FILE *output);
void serialize_uint64_t(buffer_t *b, uint64_t data);

#endif
