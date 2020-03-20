from os import devnull

from transpython import Process, Tunnel, Rule, Computation


commands = (
    Process(["ls", "/usr/include"]) |
    Process(["wc", "-l"])
)

print(commands)
stream = commands.runner(open(devnull))
print(stream.read())

remote_runner = Tunnel("some_server") | commands.remote
print(remote_runner.command("cat"))


@Computation
def one(_):
    return 1


@Computation
def pi_and_e(one):
    return {
        "pi": one + 2.14,
        "e": one + 1.78
    }


@Computation
def pi_plus_e(pi_and_e):
    return pi_and_e["pi"] + pi_and_e["e"]


operators = one | pi_and_e | pi_plus_e

result = operators(None)
print(result)
stream = operators.as_json_pipe.runner(open(devnull))
print(stream.read())


cc = Rule("cc", "gcc -c $in -o $out")
ld = Rule("ld", "gcc $in -o $out")

builds = cc.build("foo.o", "foo.c") | cc.build("bar.o", "bar.c") | ld.build("result", "foo.o", "bar.o")
print(builds.source_code)
