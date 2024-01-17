import random
import inspect


class World:
    def __init__(self):
        self.entities = []
        self.t = 0.0
        self.deleted = []
        self.rng = random.Random(42)

    def add(self, unit):
        un = unit(self)
        self.add_entity(un)
        return un

    def add_entity(self, entity):
        self.entities.append([entity, 0.0])

    def abort(self, process):
        for i, (proc, _) in enumerate(self.entities):
            if proc is process:
                break
        else:
            raise RuntimeError("Tried to abort inexisting process")
        self.deleted.append(i)

    def exists(self, caster, ability):
        x = next(
            (
                entity
                for entity, _ in self.entities
                if isinstance(entity, ability) and entity.caster == caster
            ),
            None,
        )
        return x

    def remaining_time(self, caster, proc):
        dt = next(
            (
                dt
                for entity, dt in self.entities
                if isinstance(entity, proc) and entity.caster == caster
            ),
            None,
        )
        return dt

    def tick(self):
        _, next_dt = min(self.entities, key=lambda pair: pair[1])
        self.t += next_dt
        for i in range(len(self.entities)):
            self.entities[i][1] -= next_dt
        new_entities = []
        for entity, dt in self.entities:
            if dt == 0.0:
                try:
                    dt = next(entity.implem)
                except StopIteration:
                    continue
            new_entities.append([entity, dt])
        self.entities = [e for i, e in enumerate(new_entities) if i not in self.deleted]
        self.deleted = []


class Script:
    def __init__(self, implem):
        self.implem = implem


# fmt: off
RATING_FOR_1P_VERSATILITY = [
    None,
    3.091154721, 3.091154721, 3.091154721, 3.091154721, 3.091154721, 3.091154721, 3.091154721, 3.091154721, 3.091154721,
    3.091154721, 3.091154721, 3.245712457, 3.400270193, 3.554827929, 3.709385665, 3.863943401, 4.018501137, 4.173058873,
    4.327616609, 4.482174345, 4.636732081, 4.791289817, 4.945847553, 5.100405289, 5.254963025, 5.414083305, 5.579537691,
    5.751610634, 5.930600757, 6.11682162, 6.310602529, 6.512289386, 6.722245596, 6.940853023, 7.168513002, 7.405647412,
    7.65269981, 7.910136631, 8.178448466, 8.458151403, 8.773380327, 9.100357595, 9.439521059, 9.79132489, 10.15624018,
    10.5347556, 10.92737799, 11.33463313, 11.75706636, 12.19524335, 13.46417035, 14.86513044, 16.4118618, 18.11953208,
    20.00488711, 22.08641518, 24.38452828, 26.92176229, 29.72299799, 40.0000001,
]
RATING_FOR_1P_HASTE = [
    None,
    2.550202644, 2.550202644, 2.550202644, 2.550202644, 2.550202644, 2.550202644, 2.550202644, 2.550202644, 2.550202644,
    2.550202644, 2.550202644, 2.677712777, 2.805222909, 2.932733041, 3.060243173, 3.187753306, 3.315263438, 3.44277357,
    3.570283702, 3.697793835, 3.825303967, 3.952814099, 4.080324231, 4.207834363, 4.335344496, 4.466618727, 4.603118595,
    4.745078773, 4.892745624, 5.046377837, 5.206247087, 5.372638744, 5.545852617, 5.726203744, 5.914023226, 6.109659115,
    6.313477343, 6.525862721, 6.747219984, 6.977974908, 7.23803877, 7.507795016, 7.787604874, 8.077843034, 8.378898152,
    8.691173367, 9.015086844, 9.351072331, 9.699579745, 10.06107577, 11.10794054, 12.26373262, 13.53978599, 14.94861396,
    16.50403187, 18.22129252, 20.11723583, 22.21045389, 24.52147334, 33.00000009,
]
RATING_FOR_1P_CRIT = [
    None,
    2.704760381, 2.704760381, 2.704760381, 2.704760381, 2.704760381, 2.704760381, 2.704760381, 2.704760381, 2.704760381,
    2.704760381, 2.704760381, 2.8399984, 2.975236419, 3.110474438, 3.245712457, 3.380950476, 3.516188495, 3.651426514,
    3.786664533, 3.921902552, 4.057140571, 4.19237859, 4.327616609, 4.462854628, 4.598092647, 4.737322892, 4.88209548,
    5.032659304, 5.189275662, 5.352218918, 5.521777213, 5.698253213, 5.881964896, 6.073246395, 6.272448877, 6.479941485,
    6.696112333, 6.921369552, 7.156142407, 7.400882478, 7.676707786, 7.962812896, 8.259580927, 8.567409279, 8.886710161,
    9.217911147, 9.561455743, 9.917803988, 10.28743306, 10.67083793, 11.78114906, 13.00698914, 14.36037908, 15.85459057,
    17.50427622, 19.32561328, 21.33646224, 23.55654201, 26.00762324, 35.00000009,
]
# fmt: on


class Character:
    def __init__(self, world):
        self.world = world
        self.implem = self.impl()

        self.intellect = 149 + 248
        self.stamina = 144 + 241
        self.armor = 149
        self.mana_regen = 376

        self.critical_strike = 169
        self.haste = 16
        self.mastery = 0
        self.versatility = 69

        self.health = 0
        self.mana = 9420
        self.level = 50

    @property
    def total_spell_power(self):
        return self.intellect

    @property
    def total_attack_power(self):
        return self.total_spell_power * 1.04

    @property
    def versatility_mul(self):
        return self.versatility / RATING_FOR_1P_VERSATILITY[self.level] / 100.0

    @property
    def haste_mul(self):
        return self.haste / RATING_FOR_1P_HASTE[self.level] / 100.0

    @property
    def crit_prob(self):
        return 0.05 + self.critical_strike / RATING_FOR_1P_CRIT[self.level] / 100.0

    def ready(self, ability, target):
        ab = ability(self, target)
        return ab.ready()

    def use(self, ability, target):
        ab = ability(self, target)
        if not ab.ready():
            raise RuntimeError("Ability not ready!")
        self.world.add_entity(ab)

    def run(self, ability, target, **kwargs):
        ab = ability(self, target, **kwargs)
        if isinstance(ab, Ability):
            raise TypeError("You probably want to use 'use' instead of 'run'")
        self.world.add_entity(ab)

    def heal(self, multiplier, target):
        points = round(self.total_spell_power * multiplier * (1 + self.versatility_mul))
        if self.world.rng.random() <= self.crit_prob:
            points *= 2
        target.health += points

    def hit(self, multiplier, target):
        points = round(
            self.total_attack_power * multiplier * (1 + self.versatility_mul)
        )
        if self.world.rng.random() <= self.crit_prob:
            points *= 2
        target.health -= points

    def impl(self):
        while True:
            yield 2.0


class Process:
    def __init__(self, caster, target):
        self.world = caster.world
        self.caster = caster
        self.target = target
        if not inspect.isgeneratorfunction(self.impl):
            raise TypeError(f"{self.impl} is not a generator")
        self.implem = self.impl()

    def impl(self):
        raise NotImplementedError()
        yield


class GCD(Process):
    def impl(self):
        yield 1.5 * (1 - self.caster.haste_mul)


class Ability(Process):
    def ready(self):
        raise NotImplementedError()


class Effect(Process):
    pass


class SoothingMist(Ability):
    def ready(self):
        return not self.world.exists(self.caster, GCD)

    def impl(self):
        self.caster.run(GCD, self.caster)
        for _ in range(8):
            yield 1.0 * (1 - self.caster.haste_mul)
            self.caster.heal(0.55, self.target)


class Vivify(Ability):
    def ready(self):
        return not self.world.exists(self.caster, GCD)

    def impl(self):
        self.caster.run(GCD, self.caster)
        if not self.world.exists(self.caster, SoothingMist):
            yield 1.5 * (1 - self.caster.haste_mul)
        self.caster.heal(1.41, self.target)


class EnvelopingMist(Ability):
    def ready(self):
        return not self.world.exists(self.caster, GCD)

    def impl(self):
        self.caster.run(GCD, self.caster)
        if not self.world.exists(self.caster, SoothingMist):
            yield 2.0 * (1 - self.caster.haste_mul)
        # TODO: implement naive renewal
        self.caster.run(EffectEnvelopingMist, self.target)


class EffectEnvelopingMist(Effect):
    def impl(self):
        duration = 0.0
        while duration < 6.0:
            tick = 2.0 * (1 - self.caster.haste_mul)
            multiplier = 1.0
            if duration + tick >= 6.0:
                normal_tick = tick
                tick = 6.0 - duration
                multiplier = tick / normal_tick
            yield tick
            self.caster.heal(1.2 * multiplier, self.target)
            duration += tick


class RenewingMistCooldown(Process):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.remaining_charges = 1

    def impl(self):
        while self.remaining_charges < 2:
            yield 9.0
            self.remaining_charges += 1


class EffectRenewingMist(Effect):
    def impl(self):
        duration = 0.0
        while duration < 20.0:
            tick = 2.0 * (1 - self.caster.haste_mul)
            multiplier = 1.0
            if duration + tick >= 20.0:
                normal_tick = tick
                tick = 20.0 - duration
                multiplier = tick / normal_tick
            yield tick
            self.caster.heal(0.225 * multiplier, self.target)
            duration += tick


class RenewingMist(Ability):
    def ready(self):
        if self.world.exists(self.caster, GCD):
            return False
        recharge = self.world.exists(self.caster, RenewingMistCooldown)
        if recharge and not recharge.remaining_charges:
            return False
        return True

    def impl(self):
        self.caster.run(GCD, self.caster)
        recharge = self.world.exists(self.caster, RenewingMistCooldown)
        if recharge:
            assert recharge.remaning_charges > 0
            recharge.remaining_charges -= 1
        else:
            self.caster.run(RenewingMistCooldown, self.caster)
        current_run = self.world.exists(self.caster, EffectRenewingMist)
        if current_run:
            pass  # TODO implement naive renewal
        else:
            self.caster.run(EffectRenewingMist, self.target)
        if False:
            yield


class ExpelHarm(Ability):
    def ready(self):
        return not self.world.exists(self.caster, GCD)

    def impl(self):
        self.caster.run(GCD, self.caster)
        self.caster.heal(1.2, self.caster)
        soothing_mist = self.world.exists(self.caster, SoothingMist)
        if soothing_mist:
            self.caster.heal(1.2, soothing_mist.target)
        if False:
            yield


class RisingSunKick(Ability):
    def ready(self):
        return not self.world.exists(self.caster, GCD) and not self.world.exists(
            self.caster, RisingSunKickCooldown
        )

    def impl(self):
        self.caster.run(GCD, self.caster)
        self.caster.run(RisingSunKickCooldown, self.caster)
        self.caster.hit(1.438, self.target)
        if False:
            yield


class TigerPalm(Ability):
    def ready(self):
        return not self.world.exists(self.caster, GCD)

    def impl(self):
        self.caster.run(GCD, self.caster)
        current_run = self.world.exists(self.caster, TeachingsOfTheMonastery)
        if current_run:
            stack_size = current_run.stack_size
            self.world.abort(current_run)
        else:
            stack_size = 0
        if stack_size < 3:
            stack_size += 1
        self.caster.run(TeachingsOfTheMonastery, self.caster, stack_size=stack_size)
        self.caster.hit(0.27027, self.target)
        if False:
            yield


class BlackoutKick(Ability):
    def ready(self):
        return not self.world.exists(self.caster, GCD) and not self.world.exists(
            self.caster, BlackoutKickCooldown
        )

    def impl(self):
        self.caster.run(GCD, self.caster)
        self.caster.run(BlackoutKickCooldown, self.caster)
        current_run = self.world.exists(self.caster, TeachingsOfTheMonastery)
        if current_run:
            stack_size = current_run.stack_size
            self.world.abort(current_run)
        else:
            stack_size = 0
        for _ in range(stack_size + 1):
            self.caster.hit(0.847, self.target)
            if self.world.rng.random() <= 0.15:
                rising_sun_cd = self.world.exists(self.caster, RisingSunKickCooldown)
                if rising_sun_cd:
                    self.world.abort(rising_sun_cd)
        if False:
            yield


class RisingSunKickCooldown(Process):
    def impl(self):
        yield 12.0 * (1 - self.caster.haste_mul)


class BlackoutKickCooldown(Process):
    def impl(self):
        yield 3.0 * (1 - self.caster.haste_mul)


class TeachingsOfTheMonastery(Process):
    def __init__(self, *args, stack_size=1, **kwargs):
        super().__init__(*args, **kwargs)
        self.stack_size = stack_size

    def impl(self):
        yield 20

    def __repr__(self):
        return f"TeachingsOfTheMonastery(stack_size={self.stack_size})"
