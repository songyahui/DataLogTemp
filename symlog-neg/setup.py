from setuptools import setup, find_packages

setup(
    name="symlog",
    version="0.1.0",
    packages=find_packages(),
    install_requires=[
        "deepdiff==6.3.0",
        "lark==1.1.5",
        "more_itertools==10.1.0",
        "pytest==7.3.1",
        "z3_solver==4.12.2.0",
    ],
)
