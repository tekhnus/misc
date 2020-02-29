from __future__ import division
from collections import namedtuple
from random import uniform

import pyglet
from pyglet.window import key


class Unit:

    def __init__(self, radius):
        self.radius = radius
        self.x, self.y, self.dx, self.dy = 0.0, 0.0, 0.0, 0.0

    def set_location(self, x, y):
        self.x = x
        self.y = y

    def set_speed(self, dx, dy):
        self.dx = dx
        self.dy = dy

    def bounce(self, bounce_x, bounce_y):
        self.dx *= bounce_x
        self.dy *= bounce_y

    def update(self, time):
        self.x += self.dx * time
        self.y += self.dy * time

    def intersects_with(self, another):
        return self.contains(another.x, another.y) or \
               self.contains(another.x + another.radius, another.y) or \
               self.contains(another.x + another.radius, another.y + another.radius) or \
               self.contains(another.x, another.y + another.radius)

    def contains(self, x, y):
        return self.x <= x <= self.x + self.radius and \
               self.y <= y <= self.y + self.radius


GameSettings = namedtuple('GameSettings', 'level_width level_height')


class LevelSettings:
    
    def __init__(self, level_number):
        self.level_number = level_number
        self.ward_count = 10 + level_number * 5
        self.ward_speed = 300 + 20 * level_number
        self.hero_speed = 350 + 5 * level_number


class GameState:
    
    def __init__(self, game_settings):
        self.game_settings = game_settings
        self.level_settings = LevelSettings(0)
        self.construct_level()

    def construct_level(self):
        self.hero = Unit(30)
        self.wards = [Unit(20) for count in range(self.level_settings.ward_count)]
        level_width, level_height = self.game_settings.level_width, self.game_settings.level_height
        low_width_bound, low_height_bound = level_width / 3, level_height / 3
        ward_speed = self.level_settings.ward_speed
        for ward in self.wards:
            ward.set_location(uniform(low_width_bound, level_width),
                              uniform(low_height_bound, level_height))

            ward.set_speed(uniform(-ward_speed, ward_speed),
                           uniform(-ward_speed, ward_speed))
        self.home = Unit(60)
        self.home.set_location(level_width - 60, level_height - 60)

    def set_level(self, level_number):
        self.level_settings = LevelSettings(level_number)
        self.construct_level()

    def next_level(self):
        self.set_level(self.level_settings.level_number + 1)

    def update(self, time):
        limit_x, limit_y = self.calculate_bounce(self.hero, 0)
        self.hero.bounce(limit_x, limit_y)
        for ward in self.wards:
            bounce_x, bounce_y = self.calculate_bounce(ward, 1)
            ward.bounce(bounce_x, bounce_y)
        self.hero.update(time)
        for ward in self.wards:
            ward.update(time)

    def is_game_over(self):
        for ward in self.wards:
            if self.hero.intersects_with(ward):
                return True
        return False

    def is_level_over(self):
        return self.level_settings.level_number == 0 or self.home.intersects_with(self.hero)

    def calculate_bounce(self, unit, factor):
        upper_width_bound = self.game_settings.level_width - unit.radius
        upper_height_bound = self.game_settings.level_height - unit.radius
        bounce_x = -factor if (unit.dx < 0 and unit.x <= 0) or \
                              (unit.dx > 0 and unit.x >= upper_width_bound) else 1
        bounce_y = -factor if (unit.dy < 0 and unit.y <= 0) or \
                              (unit.dy > 0 and unit.y >= upper_height_bound) else 1
        return bounce_x, bounce_y


game_settings = GameSettings(1200, 400)
game_state = GameState(game_settings)

window = pyglet.window.Window(fullscreen=True)
scale = min(window.width / game_settings.level_width, window.height / game_settings.level_height)
vertical_margin = (window.width - game_settings.level_width * scale) / 2
horizontal_margin = (window.height - game_settings.level_height * scale) / 2

keyboard = key.KeyStateHandler()
window.push_handlers(keyboard)

level_label = pyglet.text.Label('', x = 10, y = 10, font_size = 20)


def update(time):
    if game_state.is_game_over():
        game_state.construct_level()
    elif game_state.is_level_over():
        game_state.next_level()
        level_label.text = "level {}".format(game_state.level_settings.level_number)
    def get_speed(towards, backwards):
        if towards and not backwards:
            return hero_speed
        elif backwards and not towards:
            return -hero_speed
        return 0 
    hero_speed = game_state.level_settings.hero_speed
    game_state.hero.dx = get_speed(keyboard[key.RIGHT], keyboard[key.LEFT])
    game_state.hero.dy = get_speed(keyboard[key.UP], keyboard[key.DOWN])
    game_state.update(time)


def draw_rect(x1, y1, width, height):
    x2, y2 = x1 + width, y1 + height
    def transform_x(coordinate):
        return coordinate * scale + vertical_margin
    def transform_y(coordinate):
        return coordinate * scale + horizontal_margin
    a1, b1, a2, b2 = transform_x(x1), transform_y(y1), \
                     transform_x(x2), transform_y(y2)
    pyglet.graphics.draw(4,
                         pyglet.gl.GL_QUADS,
                         ('v2f', (a1, b1, a2, b1, a2, b2, a1, b2)))


def draw_unit(unit):
    draw_rect(unit.x, unit.y, unit.radius, unit.radius)


@window.event
def on_draw():
    window.clear()
    pyglet.gl.glColor3f(0, 0.1, 0.1)
    draw_rect(0, 0, game_settings.level_width, game_settings.level_height)
    pyglet.gl.glColor3f(0, 0.2, 0.1)
    draw_unit(game_state.home)
    pyglet.gl.glColor3f(0, 0.7, 0.7)
    draw_unit(game_state.hero)
    pyglet.gl.glColor3f(0.8, 0.1, 0.1)
    for ward in game_state.wards:
        draw_unit(ward)
    level_label.draw()


def main():
    pyglet.clock.schedule_interval(update, 0.005)
    pyglet.app.run()


if __name__ == "__main__":
    main()
