/**@file      h2.c
 * @brief     Simulate the H2 SoC peripherals visually
 * @copyright Richard James Howe (2017)
 * @license   MIT 
 *
 * @note This is a work in progress 
 * @todo Draw irregular polygon, or alternatively, just draw rectangle
 */


#include <assert.h>
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

static double window_height               =  410.0;
static double window_width                =  410.0;
static double window_x_starting_position  =  60.0;
static double window_y_starting_position  =  20.0;
static const double arena_tick_ms         =  15.0;
static unsigned tick                      =  0;

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
	case WHITE:   glColor3f(1.0, 1.0, 1.0);   break;
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

/* see: https://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_Text_Rendering_01
 *      https://stackoverflow.com/questions/538661/how-do-i-draw-text-with-glut-opengl-in-c
 *      https://stackoverflow.com/questions/20866508/using-glut-to-simply-print-text */
static int draw_block(const uint8_t *msg, size_t len)
{	
	assert(msg);
	for(size_t i = 0; i < len; i++)
		glutBitmapCharacter(GLUT_BITMAP_9_BY_15, msg[i]);	
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
	assert(t && fmt);
	if(!on)
		return;
	va_start(ap, fmt);
	r = vdraw_text(t->color_text, t->x, t->y - t->height, fmt, ap);
	va_end(ap);
	t->width = MAX(t->width, r); 
	t->height += ((Y_MAX / window_height) * FONT_HEIGHT); /*correct?*/
}

void draw_textbox(textbox_t *t)
{
	assert(t);
	if(!(t->draw_box))
		return;
	/**@todo fix this */
	draw_rectangle_line(t->x, t->y-t->height, t->width, t->height, 0.5, t->color_box);
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
	draw_regular_polygon_filled(l->x, l->y, l->angle + (PI/4.0), l->radius/2, SQUARE, l->on ? GREEN : BLACK);
	draw_regular_polygon_filled(l->x, l->y, l->angle + (PI/4.0), l->radius,   SQUARE, WHITE);
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
	draw_regular_polygon_filled(s->x, s->y, s->angle + (PI/4.0), s->radius/2, SQUARE, s->on ? GREEN : RED);
	draw_regular_polygon_filled(s->x, s->y, s->angle + (PI/4.0), s->radius,   SQUARE, BLUE);
}

typedef struct {
	double x;
	double y;
	double angle;
	double radius;
	uint8_t segments;
} led_8_segment_t;

void draw_led_8_segment(led_8_segment_t *l)
{
	UNUSED(l);
}

#define VGA_MEMORY_SIZE (8192)
#define VGA_WIDTH       (80)
#define VGA_HEIGHT      (40)
#define VGA_AREA        (VGA_WIDTH * VGA_HEIGHT)

#define VGA_ENABLE_BIT        (6)
#define VGA_CURSOR_ENABLE_BIT (5)
#define VGA_CURSOR_BLINK_BIT  (4)
#define VGA_CURSOR_MODE_BIT   (3)
#define VGA_RED_BIT           (2)
#define VGA_GREEN_BIT         (1)
#define VGA_BLUE_BIT          (0)

#define  VGA_ENABLE         (1  <<  VGA_ENABLE_BIT)
#define  VGA_CURSOR_ENABLE  (1  <<  VGA_CURSOR_ENABLE_BIT)
#define  VGA_CURSOR_BLINK   (1  <<  VGA_CURSOR_BLINK_BIT)
#define  VGA_CURSOR_MODE    (1  <<  VGA_CURSOR_MODE_BIT)
#define  VGA_RED            (1  <<  VGA_RED_BIT)
#define  VGA_GREEN          (1  <<  VGA_GREEN_BIT)
#define  VGA_BLUE           (1  <<  VGA_BLUE_BIT)

typedef struct {
	double x;
	double y;
	double angle;

	uint8_t cursor_x;
	uint8_t cursor_y;

	uint16_t control;

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
	if(!(v->control & VGA_ENABLE))
		return;
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	//set_color(GREEN); 
	vga_map_color(v->control & 0x7);

	double char_height = (Y_MAX / window_height) * FONT_HEIGHT;

	for(size_t i = 0; i < VGA_HEIGHT; i++) {
		glRasterPos2d(v->x, v->y - (((double)i) * char_height));
		draw_block(v->m + (i*VGA_WIDTH), VGA_WIDTH);
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

	.control = (VGA_ENABLE | VGA_GREEN),

	.m = { 0 }
};

/* ====================================== Simulator Instances ================================== */

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
	textbox_t t = { .x = X_MIN + X_MAX/40, .y = Y_MAX - Y_MAX/40, .draw_box = false, .color_text = WHITE };

	fill_textbox(&t, true, "tick:       %u", tick);
	fill_textbox(&t, true, "fps:        %f", fps());

	draw_textbox(&t);
}

static void keyboard_handler(unsigned char key, int x, int y)
{
	UNUSED(x);
	UNUSED(y);
	switch(key) {
	case ESC:
		exit(EXIT_SUCCESS);
	default:
		break;
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

static void mouse_handler(int button, int state, int x, int y)
{
	fprintf(stderr, "button: %d state: %d x: %d y: %d\n", button, state, x, y);
	//coordinate_t c = convert_pixels_to_coordinates(x, y);
	//fprintf(stderr, "x: %f y: %f\n", c.x, c.y);
}

static void resize_window(int w, int h)
{
	double scale, center;
	double window_x_min, window_x_max, window_y_min, window_y_max;

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
		window_y_min = Y_MIN;
		window_y_max = Y_MAX;
	} else {
		scale = ((X_MAX - X_MIN) / w) / ((Y_MAX - Y_MIN) / h);
		center = (Y_MAX + Y_MIN) / 2;
		window_y_min = center - (center - Y_MIN) * scale;
		window_y_max = center + (Y_MAX - center) * scale;
		window_x_min = X_MIN;
		window_x_max = X_MAX;
	}

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(window_x_min, window_x_max, window_y_min, window_y_max, -1, 1);
}

static void timer_callback(int value)
{
	tick++;
	glutTimerFunc(arena_tick_ms, timer_callback, value);
}

static void draw_scene(void)
{
	static uint64_t next = 0;

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	draw_regular_polygon_line(X_MAX/2, Y_MAX/2, PI/4, sqrt(Y_MAX*Y_MAX/2), SQUARE, 0.5, WHITE);

	draw_debug_info();

	if(next != tick) {
		// update_scene(world);
		next = tick;
	}

	for(size_t i = 0; i < SWITCHES_COUNT; i++)
		draw_switch(&switches[i]);

	for(size_t i = 0; i < LEDS_COUNT; i++)
		draw_led(&leds[i]);

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
	for(int i = 0; i < VGA_MEMORY_SIZE / VGA_WIDTH; i++)
		memset(vga.m + (i * VGA_WIDTH), 'a'+(i%26), VGA_MEMORY_SIZE / VGA_WIDTH);
	initialize_rendering(argv[0]);
	glutMainLoop();
	return 0;
}

