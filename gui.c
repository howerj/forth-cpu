/**@file      h2.c
 * @brief     Simulate the H2 SoC peripherals visually
 * @copyright Richard James Howe (2017)
 * @license   MIT 
 *
 * @todo Print debugging information for the H2 CPU to the screen
 * @todo Add font scaling, see <https://stackoverflow.com/questions/29872095/drawing-large-text-with-glut>
 * Functions to use:
 * 	* glutStrokeHeight
 * 	* glutstrokelength
 * 	* glutStrokeCharacter
 * 	* glScalef
 * And font:
 * 	GLUT_STROKE_MONO_ROMAN
 *
 * @todo Replace magic numbers like "0.5" with 1/200 of the absolute difference
 * between x/y minimum and maximum. 
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
#include <GL/glut.h>
#include <stdarg.h>

/* ====================================== Utility Functions ==================================== */

#define PI          (3.1415926535897932384626433832795)
#define MAX(X, Y)   ((X) > (Y) ? (X) : (Y))
#define MIN(X, Y)   ((X) < (Y) ? (X) : (Y))
#define FONT_HEIGHT (15)
#define FONT_WIDTH  (9)
#define ESC         (27)
#define UNUSED(X)   ((void)(X))
#define X_MAX       (100.0)
#define X_MIN       (0.0)
#define Y_MAX       (100.0)
#define Y_MIN       (0.0)
#define RUN_FOR     (512)

static double window_height               = 800.0;
static double window_width                = 800.0;
static double window_x_starting_position  = 60.0;
static double window_y_starting_position  = 20.0;
static double window_scale_x              = 1.0; 
static double window_scale_y              = 1.0;
static volatile unsigned tick             = 0;
static volatile bool     halt_simulation  = false;

static unsigned arena_tick_ms              = 30; /**@todo This should be automatically adjusted based on frame rate */
static bool use_uart_input                 = true;

typedef enum {
	WHITE,
	RED,
	YELLOW,
	GREEN,
	CYAN,
	BLUE,
	MAGENTA,
	BROWN,
	BLACK,
	INVALID_COLOR
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
	double x, y;
	bool draw_box;
	color_t color_text, color_box;
	double width, height;
} textbox_t;

typedef struct { /**@note it might be worth translating some functions to use points*/
	double x, y;
} point_t;

static double seconds_to_ticks(double s)
{
	return s * (1000. / (double)arena_tick_ms);
}

static double ticks_to_seconds(double t)
{
	return t * (((double)arena_tick_ms)/1000.);
}

static void _error(const char *func, unsigned line, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "[ERROR %s %d]: ", func, line);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fputc('\n', stderr);
	exit(EXIT_FAILURE);
}

#define error(FMT, ...) _error(__func__, __LINE__, (FMT), ## __VA_ARGS__)

double rad2deg(double rad)
{
	return (rad / (2.0 * PI)) * 360.0;
}

double deg2rad(double deg)
{
	return (deg / 360.0) * 2.0 * PI;
}

void set_color(color_t color)
{
	switch(color) {      /* RED  GRN  BLU */
	case WHITE:   glColor3f(0.8, 0.8, 0.8);   break;
	case RED:     glColor3f(0.8, 0.0, 0.0);   break;
	case YELLOW:  glColor3f(0.8, 0.8, 0.0);   break;
	case GREEN:   glColor3f(0.0, 0.8, 0.0);   break;
	case CYAN:    glColor3f(0.0, 0.8, 0.8);   break;
	case BLUE:    glColor3f(0.0, 0.0, 0.8);   break;
	case MAGENTA: glColor3f(0.8, 0.0, 0.8);   break;
	case BROWN:   glColor3f(0.35, 0.35, 0.0); break;
	case BLACK:   glColor3f(0.0, 0.0, 0.0);   break;
	default:
		error("invalid color '%d'", color);
	}
}

void draw_line(double x, double y, double angle, double magnitude, double thickness, color_t color)
{
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
		glLoadIdentity();
		glTranslated(x, y, 0);
		glRotated(rad2deg(angle), 0, 0, 1);
		glLineWidth(thickness);
		set_color(color);
		glBegin(GL_LINES);
			glVertex3d(0, 0, 0);
			glVertex3d(magnitude, 0, 0);
		glEnd();
	glPopMatrix();
}

void draw_cross(double x, double y, double angle, double magnitude, double thickness, color_t color)
{
	double xn, yn;
	xn = x-cos(angle)*(magnitude/2);
	yn = y-sin(angle)*(magnitude/2);
	draw_line(xn, yn, angle, magnitude, thickness, color);
	xn = x-cos(angle+PI/2)*(magnitude/2);
	yn = y-sin(angle+PI/2)*(magnitude/2);
	draw_line(xn, yn, angle+PI/2, magnitude, thickness, color);
}

static void _draw_arc(double x, double y, double angle, double magnitude, double arc, bool lines, double thickness, color_t color)
{
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
		glLoadIdentity();
		glTranslated(x, y, 0.0);
		glRotated(rad2deg(angle), 0, 0, 1);
		set_color(color);
		if(lines) {
			glLineWidth(thickness);
			glBegin(GL_LINE_LOOP);
		} else {
			glBegin(GL_POLYGON);
		}
			glVertex3d(0, 0, 0.0);
			for(double i = 0; i < arc; i += arc / 24.0)
				glVertex3d(cos(i) * magnitude, sin(i) * magnitude, 0.0);
		glEnd();
	glPopMatrix();

}

void draw_arc_filled(double x, double y, double angle, double magnitude, double arc, color_t color)
{
	return _draw_arc(x, y, angle, magnitude, arc, false, 0, color);
}

void draw_arc_line(double x, double y, double angle, double magnitude, double arc, double thickness, color_t color)
{
	return _draw_arc(x, y, angle, magnitude, arc, true, thickness, color);
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
		glTranslated(x, y, 0.0);
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

void draw_rectangle_filled(double x, double y, double width, double height, color_t color)
{
	return _draw_rectangle(x, y, width, height, false, 0, color); 
}

void draw_rectangle_line(double x, double y, double width, double height, double thickness, color_t color)
{
	return _draw_rectangle(x, y, width, height, true, thickness, color); 
}

double shape_to_sides(shape_t shape)
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
		error("invalid shape '%d'", shape);
	return sides[shape % INVALID_SHAPE];
}

void draw_regular_polygon_filled(double x, double y, double orientation, double radius, shape_t shape, color_t color)
{
	double sides = shape_to_sides(shape);
	_draw_regular_polygon(x, y, orientation, radius, sides, false, 0, color);
}

void draw_regular_polygon_line(double x, double y, double orientation, double radius, shape_t shape, double thickness, color_t color)
{
	double sides = shape_to_sides(shape);
	_draw_regular_polygon(x, y, orientation, radius, sides, true, thickness, color);
}

static void draw_char(uint8_t c)
{
	c = c >= 32 && c <= 127 ? c : '?';
	glutBitmapCharacter(GLUT_BITMAP_9_BY_15, c);
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

int vdraw_text(color_t color, double x, double y, const char *fmt, va_list ap)
{
	char f;
	int r = 0;
	assert(fmt);

	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	set_color(color); 
	/*glTranslated(x, y, 0);*/
	glRasterPos2d(x, y);
	while(*fmt) {
		if('%' != (f = *fmt++)) {
			glutBitmapCharacter(GLUT_BITMAP_9_BY_15, f);
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
			error("invalid format specifier '%c'", f);
		}

	}
	glPopMatrix();
	return r;
}

int draw_text(color_t color, double x, double y, const char *fmt, ...)
{
	assert(fmt);
	int r;
	va_list ap;
	va_start(ap, fmt);
	r = vdraw_text(color, x, y, fmt, ap);
	va_end(ap);
	return r;
}

void fill_textbox(textbox_t *t, bool on, const char *fmt, ...)
{
	double r;
	va_list ap;
	double char_width  = (X_MAX / window_width) * FONT_WIDTH * window_scale_x;
	assert(t && fmt);
	if(!on)
		return;
	va_start(ap, fmt);
	r = vdraw_text(t->color_text, t->x, t->y - t->height, fmt, ap);
	r *= char_width;
	r += 1;
	va_end(ap);
	t->width = MAX(t->width, r); 
	t->height += ((Y_MAX / window_height) * FONT_HEIGHT * window_scale_y); /*correct?*/
}

void draw_textbox(textbox_t *t)
{
	assert(t);
	double char_height = (Y_MAX / window_height) * FONT_HEIGHT * window_scale_y;
	if(!(t->draw_box))
		return;
	/**@todo fix this */
	draw_rectangle_line(t->x - 0.5, t->y - t->height + char_height - 1, t->width, t->height + 1, 0.5, t->color_box);
}

bool detect_circle_circle_collision(
		double ax, double ay, double aradius,
		double bx, double by, double bradius)
{
	double dx = ax - bx;
	double dy = ay - by;
	double distance = hypot(dx, dy);
	return (distance < (aradius + bradius));
}

/* From: https://stackoverflow.com/questions/215557/how-do-i-implement-a-circular-list-ring-buffer-in-c */

typedef uint8_t fifo_data_t;

typedef struct {
	size_t head;
	size_t tail;
	size_t size;
	fifo_data_t *buffer;
} fifo_t;

fifo_t *fifo_new(size_t size)
{
	fifo_data_t *buffer = allocate_or_die(size * sizeof(buffer[0]));
	fifo_t *fifo = allocate_or_die(sizeof(fifo_t));

	fifo->buffer = buffer;
	fifo->head = 0;
	fifo->tail = 0;
	fifo->size = size;

	return fifo;
}

void fifo_free(fifo_t *fifo)
{
	if(!fifo)
		return;
	free(fifo->buffer);
	free(fifo);
}

bool fifo_is_full(fifo_t * fifo)
{
	assert(fifo);
	return (fifo->head == (fifo->size - 1) && fifo->tail == 0)
	    || (fifo->head == (fifo->tail - 1));
}

bool fifo_is_empty(fifo_t * fifo)
{
	assert(fifo);
	return fifo->head == fifo->tail;
}

size_t fifo_count(fifo_t * fifo)
{
	assert(fifo);
	if (fifo_is_empty(fifo))
		return 0;
	else if (fifo_is_full(fifo))
		return fifo->size;
	else if (fifo->head < fifo->tail)
		return (fifo->head) + (fifo->size - fifo->tail);
	else
		return fifo->head - fifo->tail;
}

size_t fifo_push(fifo_t * fifo, fifo_data_t data)
{
	assert(fifo);

	if (fifo_is_full(fifo))
		return 0;

	fifo->buffer[fifo->head] = data;

	fifo->head++;
	if (fifo->head == fifo->size)
		fifo->head = 0;

	return 1;
}

size_t fifo_pop(fifo_t * fifo, fifo_data_t * data)
{
	assert(fifo);
	assert(data);

	if (fifo_is_empty(fifo))
		return 0;

	*data = fifo->buffer[fifo->tail];

	fifo->tail++;
	if (fifo->tail == fifo->size)
		fifo->tail = 0;

	return 1;
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

void draw_led(led_t *l)
{
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

void draw_switch(switch_t *s)
{
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
	switch(segment & 0xf) {
	case 0x0: return 0x3F;
	case 0x1: return 0x06;
	case 0x2: return 0x5B;
	case 0x3: return 0x4F;
	case 0x4: return 0x66;
	case 0x5: return 0x6D;
	case 0x6: return 0x7D;
	case 0x7: return 0x07;
	case 0x8: return 0x7F;
	case 0x9: return 0x6F;
	case 0xa: return 0x77;
	case 0xb: return 0x7C;
	case 0xc: return 0x39;
	case 0xd: return 0x5E;
	case 0xe: return 0x79;
	case 0xf: return 0x71;
	default:  return 0x00;
	}
}

#define SEG_CLR(SG,BIT) (((SG) & (1 << BIT)) ? RED : BLACK)

void draw_led_8_segment(led_8_segment_t *l)
{
	uint8_t sgs = convert_to_segments(l->segment);

	draw_rectangle_filled(l->x + l->width * 0.20, l->y + l->height * 0.45, l->width * 0.5, l->height * 0.1, SEG_CLR(sgs, LED_SEGMENT_G)); /* Center */
	draw_rectangle_filled(l->x + l->width * 0.20, l->y + l->height * 0.1,  l->width * 0.5, l->height * 0.1, SEG_CLR(sgs, LED_SEGMENT_D)); /* Bottom */
	draw_rectangle_filled(l->x + l->width * 0.20, l->y + l->height * 0.8,  l->width * 0.5, l->height * 0.1, SEG_CLR(sgs, LED_SEGMENT_A)); /* Top */

	draw_rectangle_filled(l->x + l->width * 0.05, l->y + l->height * 0.15, l->width * 0.1, l->height * 0.3,  SEG_CLR(sgs, LED_SEGMENT_E)); /* Bottom Left */
	draw_rectangle_filled(l->x + l->width * 0.75, l->y + l->height * 0.15, l->width * 0.1, l->height * 0.3,  SEG_CLR(sgs, LED_SEGMENT_C)); /* Bottom Right */

	draw_rectangle_filled(l->x + l->width * 0.05, l->y + l->height * 0.50, l->width * 0.1, l->height * 0.3,  SEG_CLR(sgs, LED_SEGMENT_F)); /* Top Left */
	draw_rectangle_filled(l->x + l->width * 0.75, l->y + l->height * 0.50, l->width * 0.1, l->height * 0.3,  SEG_CLR(sgs, LED_SEGMENT_B)); /* Top Right */

	draw_regular_polygon_filled(l->x + l->width * 0.9, l->y + l->height * 0.07, 0.0, sqrt(l->width*l->height)*.06, CIRCLE, SEG_CLR(sgs, LED_SEGMENT_DP));

	draw_rectangle_filled(l->x, l->y, l->width, l->height, WHITE);

}

#define VGA_MEMORY_SIZE (8192)
#define VGA_WIDTH       (80)
#define VGA_HEIGHT      (40)
#define VGA_AREA        (VGA_WIDTH * VGA_HEIGHT)

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
	uint8_t m[VGA_MEMORY_SIZE];
} vga_t;

static void vga_map_color(uint8_t c)
{
	switch(c & 0x7) { /* RED  GRN  BLU */
	case 0:    glColor3f(0.0, 0.0, 0.0); break;
	case 1:    glColor3f(0.0, 0.0, 1.0); break;
	case 2:    glColor3f(0.0, 1.0, 0.0); break;
	case 3:    glColor3f(0.0, 1.0, 1.0); break;
	case 4:    glColor3f(1.0, 0.0, 0.0); break;
	case 5:    glColor3f(1.0, 0.0, 1.0); break;
	case 6:    glColor3f(1.0, 1.0, 0.0); break;
	case 7:    glColor3f(1.0, 1.0, 1.0); break;
	}
}

void draw_vga(vga_t *v)
{
	if(!(v->control & VGA_EN))
		return;
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	/**@todo Draw the cursor if enabled */
	vga_map_color(v->control & 0x7);

	double char_height = (Y_MAX / window_height) * FONT_HEIGHT;
	double char_width  = (X_MAX / window_width)  * FONT_WIDTH;

	for(size_t i = 0; i < VGA_HEIGHT; i++) {
		glRasterPos2d(v->x, v->y - (((double)i) * char_height));
		draw_block(v->m + (i*VGA_WIDTH), VGA_WIDTH);
	}

	if(v->control & VGA_CUR_EN) {
		double now = tick - v->blink_count;
		if(now > seconds_to_ticks(1.0)) {
			v->blink_on = !(v->blink_on);
			v->blink_count = tick;
		}
		if(v->blink_on) {
			char_height *= window_scale_y;
			char_width  *= window_scale_x;
			draw_rectangle_filled(v->x + (char_width * (v->cursor_x-1.0)), v->y - (char_height * v->cursor_y), char_width, char_height, WHITE);
		}
	}

	glPopMatrix();

	/* draw_regular_polygon_line(X_MAX/2, Y_MAX/2, PI/4, sqrt(Y_MAX*Y_MAX/2), SQUARE, 0.5, WHITE); */
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

void draw_dpad(dpad_t *d)
{
	draw_regular_polygon_filled(d->x + (d->radius*2.0), d->y,                   d->angle,            d->radius, TRIANGLE, d->right  ? GREEN : RED);
	draw_regular_polygon_filled(d->x - (d->radius*2.0), d->y,                   d->angle + (PI/3.0), d->radius, TRIANGLE, d->left   ? GREEN : RED);
	draw_regular_polygon_filled(d->x,                   d->y - (d->radius*2.0), d->angle - (PI/2.0), d->radius, TRIANGLE, d->down   ? GREEN : RED);
	draw_regular_polygon_filled(d->x,                   d->y + (d->radius*2.0), d->angle + (PI/2.0), d->radius, TRIANGLE, d->up     ? GREEN : RED);
	draw_regular_polygon_filled(d->x,                   d->y,                   d->angle,            d->radius, CIRCLE,   d->center ? GREEN : RED);

	draw_regular_polygon_line(d->x, d->y, d->angle, d->radius * 3.1, CIRCLE, 0.5, WHITE);
}

typedef enum {
	DPAN_COL_NONE,
	DPAN_COL_RIGHT,
	DPAN_COL_LEFT,
	DPAN_COL_DOWN,
	DPAN_COL_UP,
	DPAN_COL_CENTER
} dpad_collision_e;

dpad_collision_e dpad_collision(dpad_t *d, double x, double y, double radius)
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

/* ====================================== Simulator Objects ==================================== */

/* ====================================== Simulator Instances ================================== */

#define SWITCHES_X       (10.0)
#define SWITCHES_SPACING (0.6)
#define SWITCHES_Y       (5.0)
#define SWITCHES_ANGLE   (0.0)
#define SWITCHES_RADIUS  (2.0)
#define SWITCHES_COUNT   (8)

static switch_t switches[SWITCHES_COUNT] = {
	{ .x = SWITCHES_X * SWITCHES_SPACING * 1.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 2.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 3.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 4.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },

	{ .x = SWITCHES_X * SWITCHES_SPACING * 5.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 6.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 7.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
	{ .x = SWITCHES_X * SWITCHES_SPACING * 8.0, .y = SWITCHES_Y, .angle = SWITCHES_ANGLE, .radius = SWITCHES_RADIUS, .on = false },
};

#define LEDS_X       (10.0)
#define LEDS_SPACING (0.6)
#define LEDS_Y       (10.0)
#define LEDS_ANGLE   (0.0)
#define LEDS_RADIUS  (2.0)
#define LEDS_COUNT   (8)

static led_t leds[LEDS_COUNT] = {
	{ .x = LEDS_X * LEDS_SPACING * 1.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 2.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 3.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 4.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },

	{ .x = LEDS_X * LEDS_SPACING * 5.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 6.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 7.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
	{ .x = LEDS_X * LEDS_SPACING * 8.0, .y = LEDS_Y, .angle = LEDS_ANGLE, .radius = LEDS_RADIUS, .on = false },
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
	.y     = Y_MAX - 6.0,
	.angle = 0.0,

	.cursor_x = 0,
	.cursor_y = 0,

	.control = 0,

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

static h2_t *h = NULL;
static h2_io_t *h2_io = NULL;
static fifo_t *uart_rx_fifo = NULL;
static fifo_t *ps2_rx_fifo = NULL;

/* ====================================== Simulator Instances ================================== */

/* ====================================== H2 I/O Handling ====================================== */

static uint16_t h2_io_get_gui(h2_soc_state_t *soc, uint16_t addr, bool *debug_on)
{
	assert(soc);

	if(debug_on)
		*debug_on = false;
	switch(addr) {
	case iUart:        
		{
			uint16_t r = 0;
			r |= fifo_is_empty(uart_rx_fifo) << UART_RX_FIFO_EMPTY_BIT;
			r |= fifo_is_full(uart_rx_fifo)  << UART_RX_FIFO_FULL_BIT;
			r |= soc->uart_getchar_register;
			r |= UART_TX_FIFO_EMPTY;
			return r;
		}
	case iSwitches:     soc->switches = 0;
			    for(size_t i = 0; i < SWITCHES_COUNT; i++)
				    soc->switches |= switches[i].on << i;
			    return soc->switches;
	case iTimerCtrl:    return soc->timer_control;
	case iTimerDin:     return soc->timer;
	/** @bug reading from VGA memory is broken for the moment */
	case iVgaTxtDout:   return 0; 
	case iPs2:
		{
			uint8_t c = 0;
			bool char_arrived = !fifo_pop(ps2_rx_fifo, &c);
			return (char_arrived << PS2_NEW_CHAR_BIT) | c;
		}	    
	}
	return 0;
}

static void h2_io_set_gui(h2_soc_state_t *soc, uint16_t addr, uint16_t value, bool *debug_on)
{
	assert(soc);

	if(debug_on)
		*debug_on = false;
	if(addr & 0x8000) {
		soc->vga[addr & 0x1FFF] = value;
		vga.m[addr & 0x1FFF]    = value;
		return;
	}

	switch(addr) {
	case oUart:
			if(value & UART_TX_WE) {
				/**@todo send this output to the screen */
				putchar(0xFF & value);
				fflush(stdout);
			}
			if(value & UART_RX_RE) {
				uint8_t c = 0;
				fifo_pop(uart_rx_fifo, &c);
				soc->uart_getchar_register = c;
			}
			break;
	case oLeds:       soc->leds           = value; 
			  for(size_t i = 0; i < LEDS_COUNT; i++)
				  leds[i].on = value & (1 << i);
			  break;
	case oTimerCtrl:  soc->timer_control  = value; break;
	case oVgaCtrl:    soc->vga_control    = value; 
			  vga.control         = value;
			  break;
	case oVgaCursor:  soc->vga_cursor     = value;
			  vga.cursor_x        = value & 0x7f;
			  vga.cursor_y        = (value >> 8) & 0x3f;
			  break;
	case o8SegLED:    for(size_t i = 0; i < SEGMENT_COUNT; i++)
				  segments[i].segment = (value >> ((SEGMENT_COUNT - i - 1) * 4)) & 0xf;
			  
			  soc->led_8_segments = value; break;
	case oIrcMask:    soc->irc_mask       = value; break;
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

static void draw_debug_info(void)
{
	textbox_t t = { .x = X_MIN + X_MAX/40, .y = Y_MAX - Y_MAX/40, .draw_box = true, .color_text = WHITE };
	fifo_t *f = use_uart_input ? uart_rx_fifo : ps2_rx_fifo;
	const char *fifo_str = use_uart_input ? "UART" : "PS/2";

	fill_textbox(&t, true, "tick:          %u", tick);
	fill_textbox(&t, true, "seconds:       %f", ticks_to_seconds(tick));
	fill_textbox(&t, true, "fps:           %f", fps());
	fill_textbox(&t, true, "%s fifo full:  %s", fifo_str, fifo_is_full(f)  ? "true" : "false");
	fill_textbox(&t, true, "%s fifo empty: %s", fifo_str, fifo_is_empty(f) ? "true" : "false");
	fill_textbox(&t, true, "%s fifo count: %u", fifo_str, (unsigned)fifo_count(f));

	draw_textbox(&t);
}

static void keyboard_handler(unsigned char key, int x, int y)
{
	UNUSED(x);
	UNUSED(y);
	if(key == ESC) {
		halt_simulation = true;
	} else {
		if(use_uart_input)
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
	case GLUT_KEY_F1:    switches[0].on = !(switches[0].on); break;
	case GLUT_KEY_F2:    switches[1].on = !(switches[1].on); break;
	case GLUT_KEY_F3:    switches[2].on = !(switches[2].on); break;
	case GLUT_KEY_F4:    switches[3].on = !(switches[3].on); break;
	case GLUT_KEY_F5:    switches[4].on = !(switches[4].on); break;
	case GLUT_KEY_F6:    switches[5].on = !(switches[5].on); break;
	case GLUT_KEY_F7:    switches[6].on = !(switches[6].on); break;
	case GLUT_KEY_F8:    switches[7].on = !(switches[7].on); break;
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
	window_width  = w;
	window_height = h;

	glViewport(0, 0, w, h);

	w = (w == 0) ? 1 : w;
	h = (h == 0) ? 1 : h;
	if ((X_MAX - X_MIN) / w < (Y_MAX - Y_MIN) / h) {
		scale = ((Y_MAX - Y_MIN) / h) / ((X_MAX - X_MIN) / w);
		center = (X_MAX + X_MIN) / 2;
		window_x_min = center - (center - X_MIN) * scale;
		window_x_max = center + (X_MAX - center) * scale;
		window_scale_x = scale;
		window_y_min = Y_MIN;
		window_y_max = Y_MAX;
	} else {
		scale = ((X_MAX - X_MIN) / w) / ((Y_MAX - Y_MIN) / h);
		center = (Y_MAX + Y_MIN) / 2;
		window_y_min = center - (center - Y_MIN) * scale;
		window_y_max = center + (Y_MAX - center) * scale;
		window_scale_y = scale;
		window_x_min = X_MIN;
		window_x_max = X_MAX;
	}

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(window_x_min, window_x_max, window_y_min, window_y_max, -1, 1);
}

static coordinate_t pixels_to_coordinates(int x, int y)
{
	coordinate_t c = { .0, .0 };
	double xd = abs_diff(X_MAX, X_MIN);
	double yd = abs_diff(Y_MAX, Y_MIN);
	double xs = window_width  / window_scale_x;
	double ys = window_height / window_scale_y;
	c.x = Y_MIN + (xd * ((x - (window_width  - xs)/2.) / xs));
	c.y = Y_MAX - (yd * ((y - (window_height - ys)/2.) / ys));
	return c;
}

static bool push_button(int button, int state)
{
	return button == GLUT_LEFT_BUTTON && state == GLUT_DOWN;
}

static void mouse_handler(int button, int state, int x, int y)
{
	coordinate_t c = pixels_to_coordinates(x, y);
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

	/**@bug push buttons need to be unset when the mouse click is released
	 * regardless of where is happens on the screen */
	switch(dpad_collision(&dpad, c.x, c.y, 0.1)) {
	case DPAN_COL_NONE:                                             break;
	case DPAN_COL_RIGHT:  dpad.right  = push_button(button, state); break;
	case DPAN_COL_LEFT:   dpad.left   = push_button(button, state); break;
	case DPAN_COL_DOWN:   dpad.down   = push_button(button, state); break;
	case DPAN_COL_UP:     dpad.up     = push_button(button, state); break;
	case DPAN_COL_CENTER: dpad.center = push_button(button, state); break;
	}
}

static void timer_callback(int value)
{
	tick++;
	glutTimerFunc(arena_tick_ms, timer_callback, value);
}

static void draw_scene(void)
{
	static uint64_t next = 0;
	if(halt_simulation)
		exit(EXIT_SUCCESS);

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	draw_regular_polygon_line(X_MAX/2, Y_MAX/2, PI/4, sqrt(Y_MAX*Y_MAX/2)*0.99, SQUARE, 0.5, WHITE);

	draw_debug_info();

	if(next != tick) {
		next = tick;
		if(h2_run(h, h2_io, stderr, 512, NULL, false) < 0)
			halt_simulation = true;
	}

	for(size_t i = 0; i < SWITCHES_COUNT; i++)
		draw_switch(&switches[i]);

	for(size_t i = 0; i < LEDS_COUNT; i++)
		draw_led(&leds[i]);

	for(size_t i = 0; i < LEDS_COUNT; i++)
		draw_led_8_segment(&segments[i]);

	draw_dpad(&dpad);
	draw_vga(&vga);

	//fill_textbox(&t, arena_paused, "PAUSED: PRESS 'R' TO CONTINUE");
	//fill_textbox(&t, arena_paused, "        PRESS 'S' TO SINGLE STEP");
	glFlush();
	glutSwapBuffers();
	glutPostRedisplay();
}

static void initialize_rendering(char *arg_0)
{
	char *glut_argv[] = { arg_0, NULL };
	int glut_argc = 0;
	glutInit(&glut_argc, glut_argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH );
	glutInitWindowPosition(window_x_starting_position, window_y_starting_position);
	glutInitWindowSize(window_width, window_height);
	glutCreateWindow("H2 Simulator (GUI)");
	glShadeModel(GL_FLAT);   
	glEnable(GL_DEPTH_TEST);
	glutKeyboardFunc(keyboard_handler);
	glutSpecialFunc(keyboard_special_handler);
	glutSpecialUpFunc(keyboard_special_up_handler);
	glutMouseFunc(mouse_handler);
	glutReshapeFunc(resize_window);
	glutDisplayFunc(draw_scene);
	glutTimerFunc(arena_tick_ms, timer_callback, 0);
}

int main(int argc, char **argv)
{
	FILE *hexfile = NULL;
	int r = 0;

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
	h2_io = h2_io_new();
	h2_io->in  = h2_io_get_gui;
	h2_io->out = h2_io_set_gui;

	uart_rx_fifo = fifo_new(UART_FIFO_DEPTH);
	ps2_rx_fifo  = fifo_new(1);

	for(int i = 0; i < VGA_MEMORY_SIZE / VGA_WIDTH; i++)
		memset(vga.m + (i * VGA_WIDTH), ' '/*'a'+(i%26)*/, VGA_MEMORY_SIZE / VGA_WIDTH);
	initialize_rendering(argv[0]);
	glutMainLoop();

	return 0;
fail:
	h2_free(h);
	return -1;
}

