class Covariant(object):
    def __init__(self, attr):
        self._attr = attr

    def __get__(self, instance, owner):
        return (
            getattr(instance.left, self._attr) |
            getattr(instance.right, self._attr)
        )


class Contravariant(object):
    def __init__(self, attr):
        self._attr = attr

    def __get__(self, instance, owner):
        return (
            getattr(instance.right, self._attr) |
            getattr(instance.left, self._attr)
        )
