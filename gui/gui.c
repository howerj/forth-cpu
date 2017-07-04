/**@file      h2.c
 * @brief     Simulate the H2 SoC peripherals visually
 * @copyright Richard James Howe (2017)
 * @license   MIT 
 *
 * @note This is a work in progress 
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

#define PI  (3.1415926535897932384626433832795)
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define FONT_HEIGHT (15)
#define FONT_WIDTH  (9)
#define ESC         (27)
#define UNUSED(X)   ((void)(X))

static double window_height               =  410.0;
static double window_width                =  410.0;
static double window_x_starting_position  =  60.0;
static double window_y_starting_position  =  20.0;
static double Xmax                        =  400.0;
static double Xmin                        =  0.0;
static double Ymax                        =  400.0;
static double Ymin                        =  0.0;
static double arena_tick_ms               =  15.0;
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
	switch(color) {
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
	if(shape > INVALID_SHAPE)
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
static int draw_block(const char *msg, size_t len)
{	
	assert(msg);
	for(size_t i = 0; i < len; i++)
		glutBitmapCharacter(GLUT_BITMAP_9_BY_15, msg[i]);	
	return len;
}

static int draw_string(const char *msg)
{	
	assert(msg);
	return draw_block(msg, strlen(msg));
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
	t->height += ((Ymax / window_height) * FONT_HEIGHT); /*correct?*/
}

void draw_textbox(textbox_t *t)
{
	assert(t);
	if(!(t->draw_box))
		return;
	/**@todo fix this */
	draw_rectangle_line(t->x, t->y-t->height, t->width, t->height, 0.5, t->color_box);
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
	UNUSED(l);
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
	draw_regular_polygon_line(s->x, s->y, 0, s->radius, SQUARE,   0.5, BLUE);
	draw_regular_polygon_line(s->x, s->y, 0, s->radius/2, SQUARE, 0.5, s->on ? WHITE : BLUE);
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

typedef struct {
	double x;
	double y;
	double angle;
	double radius;

	uint8_t cursor_x;
	uint8_t cursor_y;

	uint16_t control;

	uint16_t m[VGA_MEMORY_SIZE];
} vga_t;

void draw_vga(vga_t *v)
{
	UNUSED(v);
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

	UNUSED(d);
}

/* ====================================== Simulator Objects ==================================== */

/* ====================================== Simulator Instances ================================== */

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
	textbox_t t = { .x = Xmin + Xmax/40, .y = Ymax - Ymax/40, .draw_box = false, .color_text = WHITE };

	fill_textbox(&t, true, "tick:       %u", tick);
	fill_textbox(&t, true, "fps:        %f", fps());

	draw_textbox(&t);
}

static void keyboard_handler(unsigned char key, int x, int y)
{
	UNUSED(x);
	UNUSED(y);
	key = tolower(key);
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
	case GLUT_KEY_UP:
		break;
	case GLUT_KEY_LEFT:
		break;
	case GLUT_KEY_RIGHT:
		break;
	case GLUT_KEY_DOWN:
		break;
	default:
		break;
	}
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
	if ((Xmax - Xmin) / w < (Ymax - Ymin) / h) {
		scale = ((Ymax - Ymin) / h) / ((Xmax - Xmin) / w);
		center = (Xmax + Xmin) / 2;
		window_x_min = center - (center - Xmin) * scale;
		window_x_max = center + (Xmax - center) * scale;
		window_y_min = Ymin;
		window_y_max = Ymax;
	} else {
		scale = ((Xmax - Xmin) / w) / ((Ymax - Ymin) / h);
		center = (Ymax + Ymin) / 2;
		window_y_min = center - (center - Ymin) * scale;
		window_y_max = center + (Ymax - center) * scale;
		window_x_min = Xmin;
		window_x_max = Xmax;
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

	draw_regular_polygon_line(Xmax/2, Ymax/2, PI/4, sqrt(Ymax*Ymax/2), SQUARE, 0.5, WHITE);

	draw_debug_info();

	if(next != tick) {
		// update_scene(world);
	}

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
	glutReshapeFunc(resize_window);
	glutDisplayFunc(draw_scene);
	glutTimerFunc(arena_tick_ms, timer_callback, 0);
}

int main(int argc, char **argv)
{
	initialize_rendering(argv[0]);
	glutMainLoop();
	return 0;
}

