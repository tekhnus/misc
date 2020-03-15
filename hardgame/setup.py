import setuptools

setuptools.setup(
    name="hardgame",
    version="0.0.1",
    py_modules=["hardgame"],
    entry_points="""
        [console_scripts]
        hardgame=hardgame:main
    """,
    packages=setuptools.find_packages(),
    install_requires=["pyglet == 1.5.0"],
)
