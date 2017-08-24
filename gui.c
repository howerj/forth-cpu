/**@file      h2.c
 * @brief     Simulate the H2 SoC peripherals visually
 * @copyright Richard James Howe (2017)
 * @license   MIT
 *
 * @todo Allow the setting of the foreground and background color
 * of a text string.
 * @todo A terminal emulator as a separate program could be hacked together
 * from the components in this module, this would be a separate program.
 */

#include "h2.h"
#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <GL/glut.h>
#include <GL/freeglut_ext.h> /* for glutStrokeHeight */
#include <stdarg.h>

/* ====================================== Utility Functions ==================================== */

#define PI               (3.1415926535897932384626433832795)
#define MAX(X, Y)        ((X) > (Y) ? (X) : (Y))
#define MIN(X, Y)        ((X) < (Y) ? (X) : (Y))
#define ESCAPE           (27)
#define BACKSPACE        (8)
#define UNUSED(X)        ((void)(X))
#define X_MAX            (100.0)
#define X_MIN            (0.0)
#define Y_MAX            (100.0)
#define Y_MIN            (0.0)
#define LINE_WIDTH       (0.5)
#define CYCLE_MODE_FIXED (false)
#define CYCLE_INITIAL    (100000)
#define CYCLE_INCREMENT  (10000)
#define CYCLE_DECREMENT  (500)
#define CYCLE_MINIMUM    (10000)
#define CYCLE_HYSTERESIS (2.0)
#define TARGET_FPS       (30.0)

typedef struct {
	double window_height;
	double window_width;
	double window_x_starting_position;
	double window_y_starting_position;
	double window_scale_x;
	double window_scale_y;
	volatile unsigned tick;
	volatile bool     halt_simulation;
	unsigned arena_tick_ms;
	bool use_uart_input;
	bool debug_extra;
	bool step;
	bool debug_mode;
	uint64_t cycle_count;
	uint64_t cycles;
	void *font_scaled;
} world_t;

static world_t world = {
	.window_height               = 800.0,
	.window_width                = 800.0,
	.window_x_starting_position  = 60.0,
	.window_y_starting_position  = 20.0,
	.window_scale_x              = 1.0,
	.window_scale_y              = 1.0,
	.tick                        = 0,
	.halt_simulation             = false,
	.arena_tick_ms               = 30, /**@todo This should be automatically adjusted based on frame rate */
	.use_uart_input              = true,
	.debug_extra                 = false,
	.step                        = false,
	.debug_mode                  = false,
	.cycle_count                 = 0,
	.cycles                      = CYCLE_INITIAL,
	.font_scaled                 = GLUT_STROKE_MONO_ROMAN
};

typedef enum { /**@warning do not change the order or insert elements */
	BLACK,
	RED,
	GREEN,
	YELLOW,
	BLUE,
	MAGENTA,
	CYAN,
	WHITE,
} colors_e;

typedef colors_e color_t;

typedef enum {
	TRIANGLE,
	SQUARE,
	PENTAGON,
	HEXAGON,
	SEPTAGON,
	OCTAGON,
	DECAGON,
	CIRCLE,
	INVALID_SHAPE
} shape_e;

typedef shape_e shape_t;

typedef struct {
	double x;
	double y;
} scale_t;

typedef struct {
	double x, y;
	bool draw_border;
	color_t color_text, color_box;
	double width, height;
} textbox_t;

typedef struct { /**@note it might be worth translating some functions to use points*/
	double x, y;
} point_t;

static const char *nvram_file = FLASH_INIT_FILE;

/**@bug not quite correct, arena_tick_ms is what we request, not want the arena
 * tick actually is */
static double seconds_to_ticks(const world_t *world, double s)
{
	assert(world);
	return s * (1000. / (double)world->arena_tick_ms);
}

static double rad2deg(double rad)
{
	return (rad / (2.0 * PI)) * 360.0;
}

static void set_color(color_t color)
{
	switch(color) {      /* RED  GRN  BLU */
	case WHITE:   glColor3f(0.8, 0.8, 0.8);   break;
	case RED:     glColor3f(0.8, 0.0, 0.0);   break;
	case YELLOW:  glColor3f(0.8, 0.8, 0.0);   break;
	case GREEN:   glColor3f(0.0, 0.8, 0.0);   break;
	case CYAN:    glColor3f(0.0, 0.8, 0.8);   break;
	case BLUE:    glColor3f(0.0, 0.0, 0.8);   break;
	case MAGENTA: glColor3f(0.8, 0.0, 0.8);   break;
	case BLACK:   glColor3f(0.0, 0.0, 0.0);   break;
	default:      fatal("invalid color '%d'", color);
	}
}

/* see: https://www.opengl.org/discussion_boards/showthread.php/160784-Drawing-Circles-in-OpenGL */
static void _draw_regular_polygon(
		double x, double y,
		double orientation,
		double radius, double sides,
		bool lines, double thickness,
		color_t color)
{
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
		glLoadIdentity();
		glTranslatef(x, y, 0.0);
		glRotated(rad2deg(orientation), 0, 0, 1);
		set_color(color);
		if(lines) {
			glLineWidth(thickness);
			glBegin(GL_LINE_LOOP);
		} else {
			glBegin(GL_POLYGON);
		}
			for(double i = 0; i < 2.0 * PI; i += PI / sides)
				glVertex3d(cos(i) * radius, sin(i) * radius, 0.0);
		glEnd();
	glPopMatrix();
}

static void _draw_rectangle(double x, double y, double width, double height, bool lines, double thickness, color_t color)
{
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
		glLoadIdentity();
		glRasterPos2d(x, y);
		set_color(color);
		if(lines) {
			glLineWidth(thickness);
			glBegin(GL_LINE_LOOP);
		} else {
			glBegin(GL_POLYGON);
		}
		glVertex3d(x,       y,        0);
		glVertex3d(x+width, y,        0);
		glVertex3d(x+width, y+height, 0);
		glVertex3d(x,       y+height, 0);
		glEnd();
	glPopMatrix();
}

static void draw_rectangle_filled(double x, double y, double width, double height, color_t color)
{
	return _draw_rectangle(x, y, width, height, false, 0, color);
}

static void draw_rectangle_line(double x, double y, double width, double height, double thickness, color_t color)
{
	return _draw_rectangle(x, y, width, height, true, thickness, color);
}

static double shape_to_sides(shape_t shape)
{
	static const double sides[] =
	{
		[TRIANGLE] = 1.5,
		[SQUARE]   = 2,
		[PENTAGON] = 2.5,
		[HEXAGON]  = 3,
		[SEPTAGON] = 3.5,
		[OCTAGON]  = 4,
		[DECAGON]  = 5,
		[CIRCLE]   = 24
	};
	if(shape >= INVALID_SHAPE)
		fatal("invalid shape '%d'", shape);
	return sides[shape % INVALID_SHAPE];
}

static void draw_regular_polygon_filled(double x, double y, double orientation, double radius, shape_t shape, color_t color)
{
	double sides = shape_to_sides(shape);
	_draw_regular_polygon(x, y, orientation, radius, sides, false, 0, color);
}

static void draw_regular_polygon_line(double x, double y, double orientation, double radius, shape_t shape, double thickness, color_t color)
{
	double sides = shape_to_sides(shape);
	_draw_regular_polygon(x, y, orientation, radius, sides, true, thickness, color);
}

static void draw_char(uint8_t c)
{
	c = c >= 32 && c <= 127 ? c : '?';
	glutStrokeCharacter(world.font_scaled, c);
}

/* see: https://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_Text_Rendering_01
 *      https://stackoverflow.com/questions/538661/how-do-i-draw-text-with-glut-opengl-in-c
 *      https://stackoverflow.com/questions/20866508/using-glut-to-simply-print-text */
static int draw_block(const uint8_t *msg, size_t len)
{
	assert(msg);
	for(size_t i = 0; i < len; i++)
		draw_char(msg[i]);
	return len;
}

static int draw_string(const char *msg)
{
	assert(msg);
	return draw_block((uint8_t*)msg, strlen(msg));
}

static int draw_block_scaled(double x, double y, double scale_x, double scale_y, double orientation, const uint8_t *msg, size_t len, color_t color)
{
	assert(msg);
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
		glLoadIdentity();
		glTranslatef(x, y, 0.0);
		glScaled(scale_x, scale_y, 1.0);
		glRotated(rad2deg(orientation), 0, 0, 1);
		set_color(color);
		for(size_t i = 0; i < len; i++) {
			uint8_t c = msg[i];
			c = c >= 32 && c <= 127 ? c : '?';
			glutStrokeCharacter(world.font_scaled, c);
		}
		glEnd();
	glPopMatrix();
	return len;
}

static int draw_string_scaled(double x, double y, double scale_x, double scale_y, double orientation, const char *msg, color_t color)
{
	assert(msg);
	return draw_block_scaled(x, y, scale_x, scale_y, orientation, (uint8_t*)msg, strlen(msg), color);
}

static int vdraw_text(color_t color, double x, double y, const char *fmt, va_list ap)
{
	char f;
	int r = 0;
	assert(fmt);
	static const double scale_x = 0.011;
	static const double scale_y = 0.011;

	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	set_color(color);
	glTranslatef(x, y, 0);
	glScaled(scale_x, scale_y, 1.0);
	while(*fmt) {
		if('%' != (f = *fmt++)) {
			glutStrokeCharacter(world.font_scaled, f);
			r++;
			continue;
		}
		switch(f = *fmt++) {
		case 'c':
		{
			char x[2] = {0, 0};
			x[0] = va_arg(ap, int);
			r += draw_string(x);
			break;
		}
		case 's':
		{
			char *s = va_arg(ap, char*);
			r += draw_string(s);
			break;
		}
		case 'x':
		{
			unsigned d = va_arg(ap, unsigned);
			char s[64] = {0};
			sprintf(s, "%04x", d);
			r += draw_string(s);
			break;
		}
		case 'u':
		case 'd':
		{
			int d = va_arg(ap, int);
			char s[64] = {0};
			sprintf(s, f == 'u' ? "%u": "%d", d);
			r += draw_string(s);
			break;
		}
		case 'f':
		{
			double f = va_arg(ap, double);
			char s[512] = {0};
			sprintf(s, "%.2f", f);
			r += draw_string(s);
			break;
		}
		case 0:
		default:
			fatal("invalid format specifier '%c'", f);
		}

	}
	glPopMatrix();
	return r;
}

static scale_t font_attributes(void)
{
	scale_t scale = { 0., 0.};
	scale.y = glutStrokeHeight(world.font_scaled);
	scale.x = glutStrokeWidth(world.font_scaled, 'M');
	return scale;
}

static void fill_textbox(textbox_t *t, const char *fmt, ...)
{
	double r;
	va_list ap;
	assert(t);
	assert(fmt);

	scale_t scale = font_attributes();
	double char_width = scale.x / X_MAX;
	double char_height = scale.y / Y_MAX;
	assert(t && fmt);
	va_start(ap, fmt);
	r = vdraw_text(t->color_text, t->x, t->y - t->height, fmt, ap);
	r *= char_width * 1.11;
	r += 1;
	va_end(ap);
	t->width = MAX(t->width, r);
	t->height += (char_height); /*correct?*/
}

static void draw_textbox(textbox_t *t)
{
	assert(t);
	scale_t scale = font_attributes();
	double char_height = scale.y / Y_MAX;
	if(!(t->draw_border))
		return;
	draw_rectangle_line(t->x - LINE_WIDTH, t->y - t->height + char_height - 1, t->width, t->height + 1, LINE_WIDTH, t->color_box);
}

static bool detect_circle_circle_collision(
		double ax, double ay, double aradius,
		double bx, double by, double bradius)
{
	double dx = ax - bx;
	double dy = ay - by;
	double distance = hypot(dx, dy);
	return (distance < (aradius + bradius));
}

/* ====================================== Utility Functions ==================================== */

/* ====================================== Simulator Objects ==================================== */

typedef struct {
	double x;
	double y;
	double angle;
	double radius;
	bool on;
} led_t;

static void draw_led(led_t *l)
{
	assert(l);
	double msz = l->radius * 0.75;
	double off = (l->radius - msz) / 2.0;
	draw_rectangle_filled(l->x+off, l->y+off, msz, msz, l->on ? GREEN : RED);
	draw_rectangle_filled(l->x, l->y, l->radius, l->radius, BLUE);
}

typedef struct {
	double x;
	double y;
	double angle;
	double radius;
	bool on;
} switch_t;

static void draw_switch(switch_t *s)
{
	assert(s);
	double msz = s->radius * 0.6;
	double off = (s->radius - msz) / 2.0;
	draw_rectangle_filled(s->x+off, s->on ? (s->y + s->radius) - off : s->y+off, msz, msz, s->on ? GREEN : RED);
	draw_rectangle_filled(s->x+off, s->y + off, msz, msz*2., BLACK);
	draw_rectangle_filled(s->x, s->y, s->radius, s->radius * 2, BLUE);
}

typedef enum {
	LED_SEGMENT_A,
	LED_SEGMENT_B,
	LED_SEGMENT_C,
	LED_SEGMENT_D,
	LED_SEGMENT_E,
	LED_SEGMENT_F,
	LED_SEGMENT_G,
	LED_SEGMENT_DP,
} led_segment_e;

typedef struct {
	double x;
	double y;
	/*double angle;*/
	double width;
	double height;
	uint8_t segment;
} led_8_segment_t;

static uint8_t convert_to_segments(uint8_t segment) {
	static const uint8_t c2s[16] = { 
		0x3F, 0x06, 0x5B, 0x4F, 0x66, 0x6D, 0x7D, 0x07, 
		0x7F, 0x6F, 0x77, 0x7C, 0x39, 0x5E, 0x79, 0x71 
	};
	return c2s[segment & 0xf];
}

#define SEG_CLR(SG,BIT) (((SG) & (1 << BIT)) ? RED : BLACK)

static void draw_led_8_segment(led_8_segment_t *l)
{
	assert(l);
	uint8_t sgs = convert_to_segments(l->segment);

	draw_rectangle_filled(l->x + l->width * 0.20, l->y + l->height * 0.45, l->width * 0.5, l->height * 0.1, SEG_CLR(sgs, LED_SEGMENT_G)); /* Center */
	draw_rectangle_filled(l->x + l->width * 0.20, l->y + l->height * 0.1,  l->width * 0.5, l->height * 0.1, SEG_CLR(sgs, LED_SEGMENT_D)); /* Bottom */
	draw_rectangle_filled(l->x + l->width * 0.20, l->y + l->height * 0.8,  l->width * 0.5, l->height * 0.1, SEG_CLR(sgs, LED_SEGMENT_A)); /* Top */

	draw_rectangle_filled(l->x + l->width * 0.05, l->y + l->height * 0.15, l->width * 0.1, l->height * 0.3, SEG_CLR(sgs, LED_SEGMENT_E)); /* Bottom Left */
	draw_rectangle_filled(l->x + l->width * 0.75, l->y + l->height * 0.15, l->width * 0.1, l->height * 0.3, SEG_CLR(sgs, LED_SEGMENT_C)); /* Bottom Right */

	draw_rectangle_filled(l->x + l->width * 0.05, l->y + l->height * 0.50, l->width * 0.1, l->height * 0.3, SEG_CLR(sgs, LED_SEGMENT_F)); /* Top Left */
	draw_rectangle_filled(l->x + l->width * 0.75, l->y + l->height * 0.50, l->width * 0.1, l->height * 0.3, SEG_CLR(sgs, LED_SEGMENT_B)); /* Top Right */

	draw_regular_polygon_filled(l->x + l->width * 0.9, l->y + l->height * 0.07, 0.0, sqrt(l->width*l->height)*.06, CIRCLE, SEG_CLR(sgs, LED_SEGMENT_DP));

	draw_rectangle_filled(l->x, l->y, l->width, l->height, WHITE);
}

typedef struct {
	double x;
	double y;
	double angle;

	uint8_t cursor_x;
	uint8_t cursor_y;

	uint16_t control;

	uint64_t blink_count;
	bool blink_on;

	/**@warning The actual VGA memory is 16-bit, only the lower 8-bits are used */
	uint8_t m[VGA_BUFFER_LENGTH];
} vga_t;


static void draw_vga(const world_t *world, vga_t *v)
{
	assert(world);
	assert(v);
	if(!(v->control & VGA_EN))
		return;
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	color_t color = (v->control & 0x7);

	static const double scale_x = 0.011;
	static const double scale_y = 0.011;

	scale_t scale = font_attributes();

	double char_width  = scale.x / X_MAX;
       	double char_height = scale.y / Y_MAX;
	uint8_t *m = v->m + (v->control & VGA_SCREEN_SELECT ? VGA_BUFFER_LENGTH/2 : 0);

	for(size_t i = 0; i < VGA_HEIGHT; i++)
		draw_block_scaled(v->x, v->y - ((double)i * char_height), scale_x, scale_y, 0, m + (i*VGA_WIDTH), VGA_WIDTH, color);
	draw_string_scaled(v->x, v->y - (VGA_HEIGHT * char_height), scale_x, scale_y, 0, "VGA", color);

	/* fudge factor = 1/((1/scale_x)/X_MAX) ??? */

	if(v->control & VGA_CUR_EN) {
		double now = world->tick - v->blink_count;
		if(now > seconds_to_ticks(world, 1.0)) {
			v->blink_on = !(v->blink_on);
			v->blink_count = world->tick;
		}
		if(v->blink_on) /* fudge factor of 1.10? */
			draw_rectangle_filled(v->x + (char_width * 1.10 * (v->cursor_x-1.0)) , v->y - (char_height * v->cursor_y), char_width, char_height, WHITE);
	}

	glPopMatrix();

	draw_rectangle_line(v->x, v->y - (char_height * (VGA_HEIGHT-1.0)), char_width * VGA_WIDTH * 1.10, char_height * VGA_HEIGHT, LINE_WIDTH, color);
}

typedef struct {
	double x;
	double y;
	double angle;
	double radius;

	bool up;
	bool down;
	bool left;
	bool right;
	bool center;
} dpad_t;

static void draw_dpad(dpad_t *d)
{
	draw_regular_polygon_filled(d->x + (d->radius*2.0), d->y,                   d->angle,            d->radius, TRIANGLE, d->right  ? GREEN : RED);
	draw_regular_polygon_filled(d->x - (d->radius*2.0), d->y,                   d->angle + (PI/3.0), d->radius, TRIANGLE, d->left   ? GREEN : RED);
	draw_regular_polygon_filled(d->x,                   d->y - (d->radius*2.0), d->angle - (PI/2.0), d->radius, TRIANGLE, d->down   ? GREEN : RED);
	draw_regular_polygon_filled(d->x,                   d->y + (d->radius*2.0), d->angle + (PI/2.0), d->radius, TRIANGLE, d->up     ? GREEN : RED);
	draw_regular_polygon_filled(d->x,                   d->y,                   d->angle,            d->radius, CIRCLE,   d->center ? GREEN : RED);

	draw_regular_polygon_line(d->x, d->y, d->angle, d->radius * 3.1, CIRCLE, LINE_WIDTH, WHITE);
}

typedef enum {
	DPAN_COL_NONE,
	DPAN_COL_RIGHT,
	DPAN_COL_LEFT,
	DPAN_COL_DOWN,
	DPAN_COL_UP,
	DPAN_COL_CENTER
} dpad_collision_e;

static dpad_collision_e dpad_collision(dpad_t *d, double x, double y, double radius)
{
	if(detect_circle_circle_collision(x, y, radius, d->x + (d->radius*2.0), d->y,                   d->radius))
		return DPAN_COL_RIGHT;
	if(detect_circle_circle_collision(x, y, radius, d->x - (d->radius*2.0), d->y,                   d->radius))
		return DPAN_COL_LEFT;
	if(detect_circle_circle_collision(x, y, radius, d->x,                   d->y + (d->radius*2.0), d->radius))
		return DPAN_COL_UP;
	if(detect_circle_circle_collision(x, y, radius, d->x,                   d->y - (d->radius*2.0), d->radius))
		return DPAN_COL_DOWN;
	if(detect_circle_circle_collision(x, y, radius, d->x,                   d->y,                   d->radius))
		return DPAN_COL_CENTER;
	return DPAN_COL_NONE;
}

#define TERMINAL_WIDTH       (80)
#define TERMINAL_HEIGHT      (10)
#define TERMINAL_SIZE        (TERMINAL_WIDTH*TERMINAL_HEIGHT)
#define TERMINAL_COMMAND_LEN (256+1)

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
} terminal_attribute_t;

typedef struct {
	uint64_t blink_count;
	double x;
	double y;
	size_t cursor;
	size_t cursor_saved;
	terminal_state_t state;
	unsigned n1, n2;
	bool blink_on;
	bool blinks;
	bool cursor_on;
	terminal_attribute_t attributes[TERMINAL_SIZE];
	terminal_attribute_t attribute;
	uint8_t m[TERMINAL_SIZE];
	uint8_t command_index;
} terminal_t;

void draw_terminal(const world_t *world, terminal_t *t)
{
	assert(world);
	assert(t);
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();

	color_t color = BLUE;
	static const double scale_x = 0.011;
	static const double scale_y = 0.011;
	double now = world->tick - t->blink_count;
	scale_t scale = font_attributes();
	double char_width  = scale.x / X_MAX;
       	double char_height = scale.y / Y_MAX;
	size_t cursor_x = t->cursor % TERMINAL_WIDTH;
	size_t cursor_y = t->cursor / TERMINAL_WIDTH;
	char *source = world->use_uart_input ? "UART RX / TX" : "PS/2 KBD RX / UART TX";

	for(size_t i = 0; i < TERMINAL_HEIGHT; i++)
		draw_block_scaled(t->x, t->y - ((double)i * char_height), scale_x, scale_y, 0, t->m + (i*TERMINAL_WIDTH), TERMINAL_WIDTH, color);
	draw_string_scaled(t->x, t->y - (TERMINAL_HEIGHT * char_height), scale_x, scale_y, 0, source, color);

	/* fudge factor = 1/((1/scale_x)/X_MAX) ??? */

	if(now > seconds_to_ticks(world, 1.0)) {
		t->blink_on = !(t->blink_on);
		t->blink_count = world->tick;
	}

	/**@note the cursor is deliberately in a different position compared to draw_vga(), due to how the VGA cursor behaves in hardware */
	if((!(t->blinks) || t->blink_on) && t->cursor_on) /* fudge factor of 1.10? */
		draw_rectangle_filled(t->x + (char_width * 1.10 * (cursor_x)) , t->y - (char_height * cursor_y), char_width, char_height, WHITE);

	glPopMatrix();

	draw_rectangle_line(t->x, t->y - (char_height * (TERMINAL_HEIGHT-1.0)), char_width * TERMINAL_WIDTH * 1.10, char_height * TERMINAL_HEIGHT, LINE_WIDTH, color);
}

void terminal_default_command_sequence(terminal_t *t)
{
	assert(t);
	t->n1 = 1;
	t->n2 = 1;
	t->command_index = 0;
}

static void terminal_at_xy(terminal_t *t, unsigned x, unsigned y, bool limit_not_wrap)
{
	assert(t);
	if(limit_not_wrap) {
		x = MAX(x, 0);
		y = MAX(y, 0);
		x = MIN(x, TERMINAL_WIDTH - 1);
		y = MIN(y, TERMINAL_HEIGHT - 1);
	} else {
		x %= TERMINAL_WIDTH;
		y %= TERMINAL_HEIGHT;
	}
	t->cursor = (y * TERMINAL_WIDTH) + x;
}

static int terminal_x_current(terminal_t *t)
{
	assert(t);
	return t->cursor % TERMINAL_WIDTH;
}

static int terminal_y_current(terminal_t *t)
{
	assert(t);
	return t->cursor / TERMINAL_WIDTH;
}

static void terminal_at_xy_relative(terminal_t *t, int x, int y, bool limit_not_wrap)
{
	assert(t);
	int x_current = terminal_x_current(t);
	int y_current = terminal_y_current(t);
	terminal_at_xy(t, x_current + x, y_current + y, limit_not_wrap);
}

static void terminal_parse_attribute(terminal_attribute_t *a, unsigned v)
{
	switch(v) {
	case 0:
		memset(a, 0, sizeof(*a));
		a->foreground_color = WHITE;
		a->background_color = BLACK;
		return;
	case 1: a->bold          = true; return;
	case 4: a->under_score   = true; return;
	case 5: a->blink         = true; return;
	case 7: a->reverse_video = true; return;
	case 8: a->conceal       = true; return;
	default:
		if(v >= 30 && v <= 37)
			a->foreground_color = v - 30;
		if(v >= 40 && v <= 47)
			a->background_color = v - 40;
	}
}

/**@todo move this to "h2.c", it can be used to simulate the new terminal
 * interface being built into the hardware */
int terminal_escape_sequences(terminal_t *t, uint8_t c)
{
	assert(t);
	assert(t->state != TERMINAL_NORMAL_MODE);
	switch(t->state) {
	case TERMINAL_CSI:
		if(c == '[')
			t->state = TERMINAL_COMMAND;
		else
			goto fail;
		break;
	case TERMINAL_COMMAND:
		switch(c) {
		case 's': 
			t->cursor_saved = t->cursor; 
			goto success;
		case 'n': 
			t->cursor = t->cursor_saved;
			goto success;
		case '?': 
			terminal_default_command_sequence(t);
			t->state = TERMINAL_DECTCEM; 
			break;
		case ';':
			terminal_default_command_sequence(t);
			t->state = TERMINAL_NUMBER_2;
			break;
		default:
			if(isdigit(c)) {
				terminal_default_command_sequence(t);
				t->command_index++;
				t->n1 = c - '0';
				t->state = TERMINAL_NUMBER_1;
			} else {
				goto fail;
			}
		}
		break;
	case TERMINAL_NUMBER_1:
		if(isdigit(c)) {
			if(t->command_index > 3)
				goto fail;
			t->n1 = (t->n1 * (t->command_index ? 10 : 0)) + (c - '0');
			t->command_index++;
			break;
		}

		switch(c) {
		case 'A': terminal_at_xy_relative(t,  0,     -t->n1, true); goto success;/* relative cursor up */
		case 'B': terminal_at_xy_relative(t,  0,      t->n1, true); goto success;/* relative cursor down */
		case 'C': terminal_at_xy_relative(t,  t->n1,  0,     true); goto success;/* relative cursor forward */
		case 'D': terminal_at_xy_relative(t, -t->n1,  0,     true); goto success;/* relative cursor back */
		case 'E': terminal_at_xy(t, 0,  t->n1, false); goto success; /* relative cursor down, beginning of line */
		case 'F': terminal_at_xy(t, 0, -t->n1, false); goto success; /* relative cursor up, beginning of line */
		case 'G': terminal_at_xy(t, t->n1, terminal_y_current(t), true); goto success; /* move the cursor to column n */
		case 'm': /* set attribute, CSI number m */
			terminal_parse_attribute(&t->attribute, t->n1);
			t->attributes[t->cursor] = t->attribute;	
			goto success;
		case 'i': /* AUX Port On == 5, AUX Port Off == 4 */
			if(t->n1 == 5 || t->n1 == 4)
				goto success;
			goto fail;
		case 'n': /* Device Status Report */
			/** @note This should transmit to the H2 system the
			 * following "ESC[n;mR", where n is the row and m is the column,
			 * we're not going to do this, although fifo_push() on
			 * uart_rx_fifo could be called to do this */
			if(t->n1 == 6)
				goto success;
			goto fail;
		case 'J': /* reset */
			switch(t->n1) {
			case 3:
			case 2: t->cursor = 0; /* with cursor */
			case 1: 
				if(t->command_index) {
					memset(t->m, ' ', TERMINAL_SIZE);
					for(size_t i = 0; i < TERMINAL_SIZE; i++)
						memcpy(&t->attributes[i], &t->attribute, sizeof(t->attribute));
					goto success;
				} /* fall through if number not supplied */
			case 0:
				memset(t->m, ' ', t->cursor);
				for(size_t i = 0; i < t->cursor; i++)
					memcpy(&t->attributes[i], &t->attribute, sizeof(t->attribute));
				goto success;
			}
			goto fail;
		case ';':
			t->command_index = 0;
			t->state = TERMINAL_NUMBER_2;
			break;
		default:
			goto fail;
		}
		break;
	case TERMINAL_NUMBER_2:
		if(isdigit(c)) {
			if(t->command_index > 3)
				goto fail;
			t->n2 = (t->n2 * (t->command_index ? 10 : 0)) + (c - '0');
			t->command_index++;
		} else {
			switch(c) {
			case 'm':
				terminal_parse_attribute(&t->attribute, t->n1);
				terminal_parse_attribute(&t->attribute, t->n2);
				t->attributes[t->cursor] = t->attribute;	
				goto success;
			case 'H':
			case 'f':
				terminal_at_xy(t, t->n2, t->n1, true);
				goto success;
			}
			goto fail;
		}
		break;
	case TERMINAL_DECTCEM:
		if(isdigit(c)) {
			if(t->command_index > 1)
				goto fail;
			t->n1 = (t->n1 * (t->command_index ? 10 : 0)) + (c - '0');
			t->command_index++;
			break;
		}

		if(t->n1 != 25)
			goto fail;
		switch(c) {
		case 'l': t->cursor_on = false; goto success;
		case 'h': t->cursor_on = true;  goto success;
		default:
			goto fail;
		}
	case TERMINAL_STATE_END:
		t->state = TERMINAL_NORMAL_MODE;
		break;
	default:
		fatal("invalid terminal state: %u", (unsigned)t->state);
	}

	return 0;
success:
	t->state = TERMINAL_NORMAL_MODE;
	return 0;
fail:
	t->state = TERMINAL_NORMAL_MODE;
	return -1;
}

/**@bug Terminal is no longer clearing the display due to the way '\n' is now
 * handled */
void update_terminal(terminal_t *t, fifo_t *f)
{
	assert(t);
	assert(f);
	for(;!fifo_is_empty(f);) {
		uint8_t c = 0;
		bool r = fifo_pop(f, &c);
		if(r && t->state != TERMINAL_NORMAL_MODE) {
			if(terminal_escape_sequences(t, c)) {
				t->state = TERMINAL_NORMAL_MODE;
				/*warning("invalid ANSI command sequence");*/
			}
		} else if(r) {
			switch(c) {
			case ESCAPE:
				t->state = TERMINAL_CSI;
				break;
			case '\t':
				t->cursor += 8;
				t->cursor &= ~0x7;
				break;
			case '\n':
 				t->cursor += TERMINAL_WIDTH;
				t->cursor = (t->cursor / TERMINAL_WIDTH) * TERMINAL_WIDTH;
				break;
			case '\r':
				break;
			case BACKSPACE:
				terminal_at_xy_relative(t, -1, 0, true);
				break;
			default:
				assert(t->cursor < TERMINAL_SIZE);
				t->m[t->cursor] = c;
				memcpy(&t->attributes[t->cursor], &t->attribute, sizeof(t->attribute));
				t->cursor++;
			}
			if(t->cursor >= TERMINAL_SIZE) {
				for(size_t i = 0; i < TERMINAL_SIZE; i++)
					memcpy(&t->attributes[i], &t->attribute, sizeof(t->attribute));
				memset(t->m, ' ', TERMINAL_SIZE);
			}
			t->cursor %= TERMINAL_SIZE;
		}
	}
}

/* ====================================== Simulator Objects ==================================== */

/* ====================================== Simulator Instances ================================== */

#define SWITCHES_X       (10.0)
#define SWITCHES_SPACING (0.6)
#define SWITCHES_Y       (5.0)
#define SWITCHES_ANGLE   (0.0)
#define SWITCHES_RADIUS  (2.0)
#define SWITCHES_COUNT   (8)

static switch_t switches[SWITCHES_COUNT] = {
	{ .x = SWITCHES_X * SWITCHES_SPACING * 8.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 7.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 6.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 5.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 4.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 3.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 2.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 1.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
};

#define LEDS_X       (10.0)
#define LEDS_SPACING (0.6)
#define LEDS_Y       (10.0)
#define LEDS_ANGLE   (0.0)
#define LEDS_RADIUS  (2.0)
#define LEDS_COUNT   (8)

static led_t leds[LEDS_COUNT] = {
	{ .x = LEDS_X * LEDS_SPACING * 8.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 7.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 6.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 5.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },

	{ .x = LEDS_X * LEDS_SPACING * 4.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 3.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 2.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 1.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
};

static dpad_t dpad = {
	.x       =  X_MAX - 8.0,
	.y       =  Y_MIN + 8.0,
	.angle   =  0.0,
	.radius  =  2.0,
	.up      =  false,
	.down    =  false,
	.left    =  false,
	.right   =  false,
	.center  =  false,
};

static vga_t vga = {
	.x     = X_MIN + 2.0,
	.y     = Y_MAX - 8.0,
	.angle = 0.0,

	.cursor_x = 0,
	.cursor_y = 0,

	.control = 0x7A,

	.blink_count = 0,
	.blink_on    = false,

	.m = { 0 }
};

#define SEGMENT_COUNT   (4)
#define SEGMENT_SPACING (1.1)
#define SEGMENT_X       (50)
#define SEGMENT_Y       (3)
#define SEGMENT_WIDTH   (6)
#define SEGMENT_HEIGHT  (8)

static led_8_segment_t segments[SEGMENT_COUNT] = {
	{ .x = SEGMENT_X + (SEGMENT_SPACING * SEGMENT_WIDTH * 1.0), .y = SEGMENT_Y, .width = SEGMENT_WIDTH, .height = SEGMENT_HEIGHT, .segment = 0 },
	{ .x = SEGMENT_X + (SEGMENT_SPACING * SEGMENT_WIDTH * 2.0), .y = SEGMENT_Y, .width = SEGMENT_WIDTH, .height = SEGMENT_HEIGHT, .segment = 0 },
	{ .x = SEGMENT_X + (SEGMENT_SPACING * SEGMENT_WIDTH * 3.0), .y = SEGMENT_Y, .width = SEGMENT_WIDTH, .height = SEGMENT_HEIGHT, .segment = 0 },
	{ .x = SEGMENT_X + (SEGMENT_SPACING * SEGMENT_WIDTH * 4.0), .y = SEGMENT_Y, .width = SEGMENT_WIDTH, .height = SEGMENT_HEIGHT, .segment = 0 },
};

static terminal_t terminal = {
	.blink_count = 0,
	.cursor = 0,
	.cursor_saved = 0,
	.state = TERMINAL_NORMAL_MODE,
	.x = X_MIN + 2.0,
	.y = Y_MIN + 28.5,
	.cursor_on   = true,
	.n1 = 1, .n2 = 1,
	.blink_on = false,
	.blinks   = false,
	.m = { 0 },
	.attribute  = { 0 },
	.attributes = { { 0 } }
};

static h2_t *h = NULL;
static h2_io_t *h2_io = NULL;
static fifo_t *uart_rx_fifo = NULL;
static fifo_t *uart_tx_fifo = NULL;
static fifo_t *ps2_rx_fifo = NULL;

/* ====================================== Simulator Instances ================================== */

/* ====================================== H2 I/O Handling ====================================== */

static uint16_t h2_io_get_gui(h2_soc_state_t *soc, uint16_t addr, bool *debug_on)
{
	assert(soc);
	assert(ps2_rx_fifo);
	assert(uart_tx_fifo);
	assert(uart_rx_fifo);

	if(debug_on)
		*debug_on = false;
	switch(addr) {
	case iUart:
		{
			uint16_t r = 0;
			r |= fifo_is_empty(uart_tx_fifo) << UART_TX_FIFO_EMPTY_BIT;
			r |= fifo_is_full(uart_tx_fifo)  << UART_TX_FIFO_FULL_BIT;
			r |= fifo_is_empty(uart_rx_fifo) << UART_RX_FIFO_EMPTY_BIT;
			r |= fifo_is_full(uart_rx_fifo)  << UART_RX_FIFO_FULL_BIT;
			r |= soc->uart_getchar_register;
			return r;
		}
	case iSwitches:     
		soc->switches = 0;
		for(size_t i = 0; i < SWITCHES_COUNT; i++)
			soc->switches |= switches[i].on << i;
		return soc->switches;
	case iTimerCtrl:    return soc->timer_control;
	case iTimerDin:     return soc->timer;
	case iVga:          return 0x1100;
	case iPs2:
		{
			uint8_t c = 0;
			bool char_arrived = fifo_pop(ps2_rx_fifo, &c);
			return (char_arrived << PS2_NEW_CHAR_BIT) | c;
		}
	case iLfsr:     return soc->lfsr;
	case iMemDin:   return h2_io_memory_read_operation(soc);
	default:
		warning("invalid read from %04"PRIx16, addr);
		break;
	}
	return 0;
}

static void h2_io_set_gui(h2_soc_state_t *soc, uint16_t addr, uint16_t value, bool *debug_on)
{
	assert(soc);
	assert(uart_tx_fifo);
	assert(uart_rx_fifo);

	if(debug_on)
		*debug_on = false;

	switch(addr) {
	case oUart:
		if(value & UART_TX_WE) {
			fifo_push(uart_tx_fifo, value);
			/*putchar(value);
			fflush(stdout);*/
		}
		if(value & UART_RX_RE) {
			uint8_t c = 0;
			fifo_pop(uart_rx_fifo, &c);
			soc->uart_getchar_register = c;
		}
		break;
	case oLeds:
		soc->leds          = value;
		for(size_t i = 0; i < LEDS_COUNT; i++)
			leds[i].on = value & (1 << i);
		break;
	case oTimerCtrl:  
		soc->timer_control  = value; 
		break;
	case oVga: /**@todo implement VT100 terminal emulator */
		if(0x2000 & value) {
			vga.m[soc->vga_cursor & 0x1fff]   = value;
			soc->vga[soc->vga_cursor & 0x1fff] = value;
			soc->vga_cursor++;
		}
		break;
	case o8SegLED:
		for(size_t i = 0; i < SEGMENT_COUNT; i++)
			segments[i].segment = (value >> ((SEGMENT_COUNT - i - 1) * 4)) & 0xf;
		soc->led_8_segments = value;
		break;
	case oIrcMask:    soc->irc_mask       = value; break;
	case oLfsr:       soc->lfsr           = value; break;
	case oMemControl:
		soc->mem_control    = value;
		if(!(soc->mem_control & FLASH_MEMORY_OE) && (soc->mem_control & FLASH_MEMORY_WE))
			soc->vram[((uint32_t)(soc->mem_control & FLASH_MASK_ADDR_UPPER_MASK) << 16) | soc->mem_addr_low] = soc->mem_dout;
		break;
	case oMemAddrLow: soc->mem_addr_low   = value; break;
	case oMemDout:    soc->mem_dout       = value; break;
	default:
		warning("invalid write to %04"PRIx16 ":%04"PRIx16, addr, value);
		break;
	}
}

/* ====================================== H2 I/O Handling ====================================== */

/* ====================================== Main Loop ============================================ */

static double fps(void)
{
	static unsigned frame = 0, timebase = 0;
	static double fps = 0;
	int time = glutGet(GLUT_ELAPSED_TIME);
	frame++;
	if(time - timebase > 1000) {
		fps = frame*1000.0/(time-timebase);
		timebase = time;
		frame = 0;
	}
	return fps;
}

static void draw_debug_info(const world_t *world, double fps, double x, double y)
{
	textbox_t t = { .x = x, .y = y, .draw_border = true, .color_text = WHITE, .color_box = WHITE };
	assert(world);
	fifo_t *f = world->use_uart_input ? uart_rx_fifo : ps2_rx_fifo;
	const char *fifo_str = world->use_uart_input ? "UART" : "PS/2";
	char buf[256] = { 0 };

	fill_textbox(&t, "tick:               %u", world->tick);
	//fill_textbox(&t, "seconds:         %f", ticks_to_seconds(world->tick));
	fill_textbox(&t, "fps:                %f", fps);

	if(world->debug_extra) {
		fill_textbox(&t, "Mode:               %s", world->debug_mode ? "step" : "continue");
		fill_textbox(&t, "%s RX fifo full:  %s", fifo_str, fifo_is_full(f)  ? "true" : "false");
		fill_textbox(&t, "%s RX fifo empty: %s", fifo_str, fifo_is_empty(f) ? "true" : "false");
		fill_textbox(&t, "%s RX fifo count: %u", fifo_str, (unsigned)fifo_count(f));
		fill_textbox(&t, "UART TX fifo full:  %s", fifo_is_full(uart_tx_fifo)  ? "true" : "false");
		fill_textbox(&t, "UART TX fifo empty: %s", fifo_is_empty(uart_tx_fifo) ? "true" : "false");
		fill_textbox(&t, "UART TX fifo count: %u", (unsigned)fifo_count(uart_tx_fifo));

		sprintf(buf, "%08lu", (unsigned long)(world->cycle_count));
		fill_textbox(&t, "cycles:             %s", buf);
		fill_textbox(&t, "cycles/tick         %u", (unsigned)(world->cycles));
	}
	draw_textbox(&t);
}

static void fill_textbox_memory(textbox_t *t, uint16_t *m, size_t length)
{
	assert(t);
	assert(m);
	assert((length % 4) == 0);
	for(size_t i = 0; i < length; i+=4)
		fill_textbox(t, "%s%u: %x %x %x %x", i < 10 ? " " : "", i, m[i], m[i+1], m[i+2], m[i+3]);
}

static void draw_debug_h2_screen_1(h2_t *h, double x, double y)
{
	assert(h);
	textbox_t t = { .x = x, .y = y, .draw_border = true, .color_text = WHITE, .color_box = WHITE };
	fill_textbox(&t, "H2 CPU State", h->tos);
	fill_textbox(&t, "tp: %u", h->tos);
	fill_textbox_memory(&t, h->dstk, STK_SIZE);
	fill_textbox(&t, "pc: %u", h->pc);
	fill_textbox(&t, "rp: %u (max %u)", h->rp, h->rpm);
	fill_textbox(&t, "dp: %u (max %u)", h->sp, h->spm);
	fill_textbox(&t, "ie: %s", h->ie ? "true" : "false");
	draw_textbox(&t);
}

static void draw_debug_h2_screen_2(h2_t *h, double x, double y)
{
	textbox_t t = { .x = x, .y = y, .draw_border = true, .color_text = WHITE, .color_box = WHITE };
	assert(h);
	fill_textbox(&t, "H2 CPU Return Stack");
	fill_textbox_memory(&t, h->rstk, STK_SIZE);
	draw_textbox(&t);
}

static void draw_debug_h2_screen_3(h2_io_t *io, double x, double y)
{
	textbox_t t = { .x = x, .y = y, .draw_border = true, .color_text = WHITE, .color_box = WHITE };
	assert(io);
	assert(io->soc);
	h2_soc_state_t *s = io->soc;
	fill_textbox(&t, "I/O");
	fill_textbox(&t, "LED             %x", (unsigned)s->leds);
	fill_textbox(&t, "VGA Cursor:     %x", (unsigned)s->vga_cursor);
	fill_textbox(&t, "Timer Control:  %x", (unsigned)s->timer_control);
	fill_textbox(&t, "Timer Count:    %x", (unsigned)s->timer);
	fill_textbox(&t, "IRQ Mask:       %x", (unsigned)s->irc_mask);
	fill_textbox(&t, "LED 8 Segments: %x", (unsigned)s->led_8_segments);
	fill_textbox(&t, "Switches:       %x", (unsigned)s->switches);
	fill_textbox(&t, "LFSR:           %x", (unsigned)s->lfsr);
	fill_textbox(&t, "Memory Control: %x", (unsigned)s->mem_control);
	fill_textbox(&t, "Memory Address: %x", (unsigned)s->mem_addr_low);
	fill_textbox(&t, "Memory Output:  %x", (unsigned)s->mem_dout);
	fill_textbox(&t, "Wait:           %s", s->wait ? "yes" : "no");
	fill_textbox(&t, "Interrupt:      %s", s->interrupt ? "yes" : "no");
	fill_textbox(&t, "IRQ Selector:   %x", (unsigned)s->interrupt_selector);
	fill_textbox(&t, ""); 
	fill_textbox(&t, "Flash"); 
	fill_textbox(&t, "we:             %s", s->flash.we ? "on" : "off"); 
	fill_textbox(&t, "cs:             %s", s->flash.cs ? "on" : "off"); 
	fill_textbox(&t, "mode:           %x", (unsigned)s->flash.mode);
	fill_textbox(&t, "status:         %x", (unsigned)s->flash.status); 
	fill_textbox(&t, "address arg 1:  %x", (unsigned)s->flash.arg1_address); 
	fill_textbox(&t, "data            %x", (unsigned)s->flash.data); 
	fill_textbox(&t, "cycle:          %x", (unsigned)s->flash.cycle);
	draw_textbox(&t);
}

static void keyboard_handler(unsigned char key, int x, int y)
{
	UNUSED(x);
	UNUSED(y);
	assert(uart_tx_fifo);
	assert(ps2_rx_fifo);
	if(key == ESCAPE) {
		world.halt_simulation = true;
	} else {
		if(world.use_uart_input)
			fifo_push(uart_rx_fifo, key);
		else
			fifo_push(ps2_rx_fifo, key);
	}
}

static void keyboard_special_handler(int key, int x, int y)
{
	UNUSED(x);
	UNUSED(y);
	switch(key) {
	case GLUT_KEY_UP:    dpad.up    = true; break;
	case GLUT_KEY_LEFT:  dpad.left  = true; break;
	case GLUT_KEY_RIGHT: dpad.right = true; break;
	case GLUT_KEY_DOWN:  dpad.down  = true; break;
	case GLUT_KEY_F1:    switches[7].on = !(switches[7].on); break;
	case GLUT_KEY_F2:    switches[6].on = !(switches[6].on); break;
	case GLUT_KEY_F3:    switches[5].on = !(switches[5].on); break;
	case GLUT_KEY_F4:    switches[4].on = !(switches[4].on); break;
	case GLUT_KEY_F5:    switches[3].on = !(switches[3].on); break;
	case GLUT_KEY_F6:    switches[2].on = !(switches[2].on); break;
	case GLUT_KEY_F7:    switches[1].on = !(switches[1].on); break;
	case GLUT_KEY_F8:    switches[0].on = !(switches[0].on); break;
	case GLUT_KEY_F9:    world.step       = true;
			     world.debug_mode = true;
			     break;
	case GLUT_KEY_F10:   world.debug_mode     = !(world.debug_mode);     break;
	case GLUT_KEY_F11:   world.use_uart_input = !(world.use_uart_input); break;
	case GLUT_KEY_F12:   world.debug_extra    = !(world.debug_extra);    break;
	default:
		break;
	}
}

static void keyboard_special_up_handler(int key, int x, int y)
{
	UNUSED(x);
	UNUSED(y);
	switch(key) {
	case GLUT_KEY_UP:    dpad.up    = false; break;
	case GLUT_KEY_LEFT:  dpad.left  = false; break;
	case GLUT_KEY_RIGHT: dpad.right = false; break;
	case GLUT_KEY_DOWN:  dpad.down  = false; break;
	default:
		break;
	}
}

typedef struct {
	double x;
	double y;
} coordinate_t;

static double abs_diff(double a, double b)
{
	return fabsl(fabsl(a) - fabsl(b));
}

static void resize_window(int w, int h)
{
	double window_x_min, window_x_max, window_y_min, window_y_max;
	double scale, center;
	world.window_width  = w;
	world.window_height = h;

	glViewport(0, 0, w, h);

	w = (w == 0) ? 1 : w;
	h = (h == 0) ? 1 : h;
	if ((X_MAX - X_MIN) / w < (Y_MAX - Y_MIN) / h) {
		scale = ((Y_MAX - Y_MIN) / h) / ((X_MAX - X_MIN) / w);
		center = (X_MAX + X_MIN) / 2;
		window_x_min = center - (center - X_MIN) * scale;
		window_x_max = center + (X_MAX - center) * scale;
		world.window_scale_x = scale;
		window_y_min = Y_MIN;
		window_y_max = Y_MAX;
	} else {
		scale = ((X_MAX - X_MIN) / w) / ((Y_MAX - Y_MIN) / h);
		center = (Y_MAX + Y_MIN) / 2;
		window_y_min = center - (center - Y_MIN) * scale;
		window_y_max = center + (Y_MAX - center) * scale;
		world.window_scale_y = scale;
		window_x_min = X_MIN;
		window_x_max = X_MAX;
	}

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(window_x_min, window_x_max, window_y_min, window_y_max, -1, 1);
}

static coordinate_t pixels_to_coordinates(const world_t *world, int x, int y)
{
	assert(world);
	coordinate_t c = { .0, .0 };
	double xd = abs_diff(X_MAX, X_MIN);
	double yd = abs_diff(Y_MAX, Y_MIN);
	double xs = world->window_width  / world->window_scale_x;
	double ys = world->window_height / world->window_scale_y;
	c.x = Y_MIN + (xd * ((x - (world->window_width  - xs)/2.) / xs));
	c.y = Y_MAX - (yd * ((y - (world->window_height - ys)/2.) / ys));
	return c;
}

static void mouse_handler(int button, int state, int x, int y)
{
	coordinate_t c = pixels_to_coordinates(&world, x, y);
	/*fprintf(stderr, "button: %d state: %d x: %d y: %d\n", button, state, x, y);
	fprintf(stderr, "x: %f y: %f\n", c.x, c.y); */

	for(size_t i = 0; i < SWITCHES_COUNT; i++) {
		/*fprintf(stderr, "x: %f y: %f\n", switches[i].x, switches[i].y);*/
		if(detect_circle_circle_collision(c.x, c.y, 0.1, switches[i].x, switches[i].y, switches[i].radius)) {
			/*fprintf(stderr, "hit\n");*/
			if(button == GLUT_LEFT_BUTTON && state == GLUT_DOWN)
				switches[i].on = true;
			if(button == GLUT_RIGHT_BUTTON && state == GLUT_DOWN)
				switches[i].on = false;
			return;
		}
	}

	if(button == GLUT_LEFT_BUTTON && state == GLUT_DOWN) {
		switch(dpad_collision(&dpad, c.x, c.y, 0.1)) {
		case DPAN_COL_NONE:                       break;
		case DPAN_COL_RIGHT:  dpad.right  = true; break;
		case DPAN_COL_LEFT:   dpad.left   = true; break;
		case DPAN_COL_DOWN:   dpad.down   = true; break;
		case DPAN_COL_UP:     dpad.up     = true; break;
		case DPAN_COL_CENTER: dpad.center = true; break;
		}
	} else if(button == GLUT_LEFT_BUTTON && state == GLUT_UP) {
		dpad.right  = false;
		dpad.left   = false;
		dpad.down   = false;
		dpad.up     = false;
		dpad.center = false;
	}
}

static void timer_callback(int value)
{
	world.tick++;
	glutTimerFunc(world.arena_tick_ms, timer_callback, value);
}

static void draw_scene(void)
{
	static uint64_t next = 0;
	double f = fps();
	if(world.halt_simulation)
		exit(EXIT_SUCCESS);

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	draw_regular_polygon_line(X_MAX/2, Y_MAX/2, PI/4, sqrt(Y_MAX*Y_MAX/2)*0.99, SQUARE, LINE_WIDTH, WHITE);

	if(next != world.tick) {
		unsigned long increment = 0;
		next = world.tick;
		update_terminal(&terminal, uart_tx_fifo);

		if(world.debug_mode && world.step)
			increment = 1;
		else if(!(world.debug_mode))
			increment = world.cycles;

		if(!CYCLE_MODE_FIXED && increment) {
			uint64_t n = world.cycles + (f > TARGET_FPS ? CYCLE_INCREMENT : -CYCLE_DECREMENT);
			if(f > (TARGET_FPS + CYCLE_HYSTERESIS)) {
				world.cycles = MIN(((uint64_t)-1), n);
			} else if(f < (TARGET_FPS - CYCLE_HYSTERESIS)) {
				world.cycles = MAX(CYCLE_MINIMUM, n);
			}
		}

		if(increment)
			if(h2_run(h, h2_io, stderr, increment, NULL, false) < 0)
				world.halt_simulation = true;

		world.step = false;
		world.cycle_count += increment;
	}
	draw_debug_info(&world, f, X_MIN + X_MAX/40., Y_MAX - Y_MAX/40.);
	if(world.debug_extra) {
		draw_debug_h2_screen_1(h,     X_MIN + X_MAX/40., Y_MAX*0.70);
		draw_debug_h2_screen_2(h,     X_MAX / 3.0,       Y_MAX*0.70);
		draw_debug_h2_screen_3(h2_io, X_MAX / 1.55,  Y_MAX*0.70);
	} else {
		draw_vga(&world, &vga);
	}

	for(size_t i = 0; i < SWITCHES_COUNT; i++)
		draw_switch(&switches[i]);

	for(size_t i = 0; i < LEDS_COUNT; i++)
		draw_led(&leds[i]);

	for(size_t i = 0; i < SEGMENT_COUNT; i++)
		draw_led_8_segment(&segments[i]);

	draw_dpad(&dpad);
	draw_terminal(&world, &terminal);

	{
		textbox_t t = { .x = X_MAX-50, .y = Y_MAX-2, .draw_border = false, .color_text = WHITE, .color_box = WHITE };
		fill_textbox(&t, "EXIT/QUIT     ESCAPE");
		fill_textbox(&t, "SWITCHES     F-1...8");
		fill_textbox(&t, "SINGLE STEP      F-9");

	}
	{
		textbox_t t = { .x = X_MAX-25, .y = Y_MAX-2, .draw_border = false, .color_text = WHITE, .color_box = WHITE };
		fill_textbox(&t, "CPU PAUSE/RESUME F-10");
		fill_textbox(&t, "SWITCH INPUT     F-11");
		fill_textbox(&t, "CHANGE DISPLAY   F-12");
	}

	glFlush();
	glutSwapBuffers();
	glutPostRedisplay();
}

static void initialize_rendering(char *arg_0)
{
	char *glut_argv[] = { arg_0, NULL };
	int glut_argc = 0;
	memset(terminal.m, ' ', TERMINAL_SIZE);
	glutInit(&glut_argc, glut_argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH );
	glutInitWindowPosition(world.window_x_starting_position, world.window_y_starting_position);
	glutInitWindowSize(world.window_width, world.window_height);
	glutCreateWindow("H2 Simulator (GUI)");
	glShadeModel(GL_FLAT);
	glEnable(GL_DEPTH_TEST);
	glutKeyboardFunc(keyboard_handler);
	glutSpecialFunc(keyboard_special_handler);
	glutSpecialUpFunc(keyboard_special_up_handler);
	glutMouseFunc(mouse_handler);
	glutReshapeFunc(resize_window);
	glutDisplayFunc(draw_scene);
	glutTimerFunc(world.arena_tick_ms, timer_callback, 0);
}

void finalize(void)
{
	FILE *nvram_fh = NULL;
	errno = 0;
	if((nvram_fh = fopen(nvram_file, "wb"))) {
		fwrite(h2_io->soc->flash.nvram, CHIP_MEMORY_SIZE, 1, nvram_fh);
		fclose(nvram_fh);
	} else {
		error("nvram file write (to %s) failed: %s", nvram_file, strerror(errno));
	}
	h2_free(h);
	h2_io_free(h2_io);
	fifo_free(uart_tx_fifo);
	fifo_free(uart_rx_fifo);
	fifo_free(ps2_rx_fifo);
}

int main(int argc, char **argv)
{
	FILE *hexfile = NULL;
	FILE *nvram_fh = NULL;
	int r = 0;

	assert(Y_MAX > 0. && Y_MIN < Y_MAX && Y_MIN >= 0.);
	assert(X_MAX > 0. && X_MIN < X_MAX && X_MIN >= 0.);

	log_level = LOG_NOTE;

	if(argc != 2) {
		fprintf(stderr, "usage %s h2.hex\n", argv[0]);
		return -1;
	}
	hexfile = fopen_or_die(argv[1], "rb");

	h = h2_new(START_ADDR);
	r = h2_load(h, hexfile);
	fclose(hexfile);
	if(r < 0) {
		fprintf(stderr, "h2 load failed\n");
		goto fail;
	}
	h2_io      = h2_io_new();
	h2_io->in  = h2_io_get_gui;
	h2_io->out = h2_io_set_gui;

	{ /* attempt to load initial contents of VGA memory */
		FILE *vga_init = fopen(VGA_INIT_FILE, "rb");
		if(vga_init) {
			memory_load(vga_init, h2_io->soc->vga, VGA_BUFFER_LENGTH);
			for(size_t i = 0; i < VGA_BUFFER_LENGTH; i++)
				vga.m[i] = h2_io->soc->vga[i];
			fclose(vga_init);
		} else {
			warning("");
		}
	}

	uart_rx_fifo = fifo_new(UART_FIFO_DEPTH);
	uart_tx_fifo = fifo_new(UART_FIFO_DEPTH * 100); /** @note x100 to speed things up */
	ps2_rx_fifo  = fifo_new(8 /** @bug should be 1 - but this does not work, FIFO implementation needs correcting */);

	errno = 0;
	if((nvram_fh = fopen(nvram_file, "rb"))) {
		fread(h2_io->soc->flash.nvram, CHIP_MEMORY_SIZE, 1, nvram_fh);
		fclose(nvram_fh);
	} else {
		debug("nvram file read (from %s) failed: %s", nvram_file, strerror(errno));
	}

	atexit(finalize);
	initialize_rendering(argv[0]);
	glutMainLoop();

	return 0;
fail:
	h2_free(h);
	return -1;
}

