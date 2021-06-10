#!/usr/bin/env python
import subprocess
from pathlib import Path
from itertools import chain
from jinja2 import Environment, FileSystemLoader

content = Path("content")
site = Path("docs")

env = Environment(loader=FileSystemLoader("./content/"))

for item in chain([content], content.glob("**/*")):
    destination = site / item.relative_to(content)
    if destination.suffix == ".py":
        destination = destination.with_suffix(".html")
    if item.is_dir():
        destination.mkdir(exist_ok=True)
    else:
        if item.name.endswith(".py"):
            item_reader = lambda: subprocess.Popen(
                ["python3", str(item)],
                env={"PYTHONPATH": "."},
                stdout=subprocess.PIPE,
                universal_newlines=True
            ).stdout
        else:
            item_reader = lambda: item.open("r")
        with item_reader() as reader, destination.open("w") as writer:
            contents = reader.read()
            rendered = env.from_string(contents).render()
            writer.write(rendered)
