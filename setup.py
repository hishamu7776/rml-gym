
import setuptools

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setuptools.setup(
    name="rmlgym",
    version="0.0.1",
    author="Hisham Unniyankal",
    author_email="uaj.hisham@gmail.com",
    description="",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        # "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6, <3.9',
    install_requires=[
        "pyaml >= 21.10.1",
        "gym >= 0.15.7",
        "numpy >= 1.18.5"
    ]
)