from setuptools import find_packages
from setuptools import setup


setup(
    name="nav",
    version="0.0.1",
    url="https://github.com/tekhnus/nav",
    packages=find_packages(),
    # package_dir={"": "src"},
    zip_safe=False,
    python_requires=">=3.6",
    entry_points={
        "console_scripts": ["nav = nav:main"]
    },
)
