from .transform import Transform


class BaseBuild:
    @property
    def source_code(self):
        return "".join(statement._source_code for statement in self.statements)


class Build(BaseBuild, Transform):
    class Composition(BaseBuild, Transform.Composition):
        pass

    def __init__(self, rule, outputs, *args):
        self.rule = rule

        self.outputs = outputs
        self.inputs = " ".join(args)

    @property
    def statements(self):
        return {self.rule, self}

    @property
    def _source_code(self):
        return "build {self.outputs}: {self.rule.name} {self.inputs}\n\n".format(self=self)


class Rule:
    def __init__(self, name, command):
        self.name = name
        self.command = command

    def build(self, outputs, *args):
        return Build(self, outputs, *args)

    @property
    def _source_code(self):
        return "rule {self.name}\n  command = {self.command}\n\n".format(self=self)
